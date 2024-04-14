
(* ir representations:
 *
 *     boolean is |0| or |1|
 *     rune    is |7-bit ascii|
 *     integer is a word/register
 *     string is |size|chars....|
 *     struct is |field1|field2|field3|...|
 *     array  is |size|elt0|elt1|...|
 *
 * note that |_| means padded to wordsize and aligned on word
 *)

type temp = Symbol.symbol
type label = Symbol.symbol

type exp = CONST of int
         | BINOP of temp * bop * temp
         | ROP of temp * rop * temp
         | TEMP of temp

and stm = STORE of temp * temp          (* MOVE MEM[t1] <- t2 *)
        | LOAD  of temp * temp          (* MOVE t1 <- MEM[t2] *)
        | MOVE  of temp * exp           (* MOVE t <- ... *)
                                        (* no MOVE MEM[t1] <- MEM[t2] *)
        | CALL of label * temp list * temp list (* function, args, returns *)
        | JUMP of label                 (* no tabular jumps *)
        | CJUMP of temp * label * label (* bool * true_place * false_place *)
        (* also note that codegen can recognize
         *      EVAL t7 <- t3 EQ 0
         *      CJUMP t7, _zero:, _nonzero:
         * as            BZ t3 _zero:
         *    _non_zero: ...
         *)
        | LABEL of label

and bb = label * stm list (* a basic block -- enter at top *)

and bop = | PLUS | MINUS | MULT | DIV | AND | OR | SHL | SHR

and rop = | EQ | NEQ | LT | LE | GE | GT

type frag = IRSTRING of string
          | IRFUNC of label * bb list  (* each function *)
          | IRMAIN of bb list          (* the main block *)

(* this is the output from ir code generation: *)
type ircode = frag list



(* do things now *)
let ircode: ircode = [
  (* func printBoard ()->() *)
  IRFUNC(label "_func_printBoard", []);

  (* func try (c int) -> () *)
  IRFUNC(label "_func_try", []);


  IRMAIN([
    MOVE(temp "ZERO", CONST(0)); (* const 0 *)
    MOVE(temp "ONE", CONST(1)); (* const 1 *)
    MOVE(temp "TWO", CONST(2)); (* const 2 *)
    MOVE(temp "FOUR", CONST(4)); (* getting wordsize *)
    (* init global vals and vars. *)
    LABEL(label "_start");
    (* Base: const N = 8 *)
    (* Tabsyn: ConstDecl(N, IntType, IntExp(8)) *)
    MOVE(temp "N", CONST(8));
    MOVE(temp "_arr_offset", CONST(8)); (* 8 bytes array header *)
  
    (* Base: type boolArray []boolean
             type intArray []int *)
    (* Tabsyn: TypeDecl(boolArray, ArrayType(BoolType))
               TypeDecl(intArray, ArrayType(IntType))*)
    (* we don't care about the type of the arrays *)
    
    (* Base: var row boolArray = [N]boolean *)
    (* Tabsyn: VarDecl([ (row, NameType(boolArray), NilExp)])
             , AssignStmt( (* _allocate_ *)
                [SimpleVar(boolArray, row)], 
                [NewExp(bool, SimpleVar(IntType, N))]) *)
    (* for allocation, assuming we have a library that does this,
       providing a size in bytes *)
    LABEL(label "_decl_row");
      MOVE(temp "_alloc_row_size", 
        BINOP(temp "N", PLUS, temp "_arr_offset"));
      CALL(label "_alloc_",
        (* size is N x sizeof(bool) + 8 *)
          (* assume we knows that bool takes 1 byte *)
          [temp "_alloc_row_size"]
            (* assuming, like C, we have a special `errno` that takes 
          return from system library calls *)
          [temp "row", temp "errno"]);
      MOVE(temp "row_body", (* body of an array begin at 8 bytes *)
        BINOP(temp "row", PLUS, temp "_arr_offset"));
    (* now, we need to initialize the array (to the zero-val) *)
    (* Base: <sugar> *)
    (* Tabsyn: /*
      , Scope([ VarDecl(%init0, IntType, IntExp(0))
             , ForStmt(OpExp(SimpleVar(IntType, %init0), IntLtOp, SimpleVar(IntType, N))
	             (* _loop body_ *)
	             , Scope([ Scope ([ AssignStmt(SubScriptVar(SimpleVar(boolArray, row), IntExp(SimpleVar(IntType, %init0))), BoolExp(false))])
		     (* _continue scope_ *)
		     , AssignStmt([(SimpleVar(IntType, %init0))], [OpExp(SimpleVar(IntType, %init0), PlusOp, IntExp(1))])]))])
*/ *)
    LABEL(label "_init_row"); (* like the `scope` in tabsyn *)
      MOVE(temp "%init0", CONST(0));
      (* I'm going to rewrite a loop into 
        a do-while wrapped in if. Saves (n-1) instructions *)
      LABEL(label "_init_row_loop_head_0");
        (* ite test *)
        MOVE(temp "_t_init_row_loop_test", 
          ROP(temp "%init0", LT, temp "N"));
        CJUMP(temp "_init_row_loop_test",
          label "_l_init_row_loop_body_0",
          label "_l_init_row_loop_end_0");
        LABEL(label "_l_init_row_loop_body_0");
          (* row[i] <- false *)
          (* STORE of temp * temp is MOVE MEM[t1] <- t2 *)
          (* row[i] is at (row+8+i) *)
          MOVE(temp "_t_init_row_mem", 
            BINOP(temp "row_body", PLUS, temp "%init0"));
          (* assuming we use 0 for false *)
          STORE(temp "_t_init_row_mem", temp "ZERO"); (* ZERO is a const 0 *)
          (* i++ *)
          MOVE(temp "%init0", 
            BINOP(temp "%init0", PLUS, temp "ONE"));
          (* test again *)
          MOVE(TEMP "_t_init_row_loop_test", 
            ROP(temp "%init0", LT, temp "N"));
          CJUMP(temp "_init_row_loop_test",
            label "_l_init_row_loop_body_0",
            label "_l_init_row_loop_end_0");
        LABEL(label "_l_init_row_loop_end_0");

    (* very cool, now do the same thing for col *)
    (* Base: var col intArray = [N]int *)
    (* Tabsyn: 
     , VarDecl([ (col, NameType(intArray), NilExp)])
     , AssignStmt([SimpleVar(intArray, col)], [NewExp(int, SimpleVar(IntType, N))] *)

    LABEL(label "_decl_col");
      MOVE(temp "_alloc_col_body_bytes",
        BINOP(temp "N", SHL, temp "TWO"));
      MOVE(temp "_alloc_col_size", 
        BINOP(
          temp "_arr_offset",
          PLUS,
          temp "_alloc_col_body_bytes"));
    CALL(label "_alloc_",
        (* size is N x sizeof(int) + 8 *)
          (* assume we knows that int takes 4 byte *)
          [temp "_alloc_col_size"]
          [temp "col", temp "errno"]);
    MOVE(temp "col_body", (* body of an array begin at 8 bytes *)
      BINOP(temp "col", PLUS, temp "_arr_offset"));

    (* now, we need to initialize the array (to the zero-val) *)
    (* Tabsyn: /*
     , Scope([ VarDecl(%init1, IntType, IntExp(0))
             , ForStmt(OpExp(SimpleVar(IntType, %init1), IntLtOp, SimpleVar(IntType, N))
	             , Scope([ Scope ([ AssignStmt(SubScriptVar(SimpleVar(NameType(intArray), col), IntExp(SimpleVar(IntType, %init1))), IntExp(0))])
		     , AssignStmt([(SimpleVar(IntType, %init1))], [OpExp(SimpleVar(IntType, %init1), PlusOp, IntExp(1))])]))])
*/ *)
    LABEL(label "_init_col");
      MOVE(temp "%init1", CONST(0));
      LABEL(label "_init_col_loop_head_0");
        MOVE(temp "_t_init_col_loop_test", 
          ROP(temp "%init1", LT, temp "N"));
        CJUMP(temp "_init_col_loop_test",
          label "_l_init_col_loop_body_0",
          label "_l_init_col_loop_end_0");
        LABEL(label "_l_init_col_loop_body_0");
          MOVE(temp "_t_init_col_offset",
            BINOP(temp "%init1", SHL, temp "TWO"));
          MOVE(temp "_t_init_col_mem", 
            BINOP(temp "col_body", PLUS, temp "_t_init_col_offset"));
          STORE(temp "_t_init_col_mem", temp "ZERO");
          MOVE(temp "%init1", 
            BINOP(temp "%init1", PLUS, temp "ONE"));
          MOVE(TEMP "_t_init_col_loop_test", 
            ROP(temp "%init1", LT, temp "N"));
        CJUMP(temp "_init_col_loop_test",
        label "_l_init_col_loop_body_0",
        label "_l_init_col_loop_end_0");
        LABEL(label "_l_init_col_loop_end_0");


    (* do diag1 now. *)
    (* Base: var diag1 boolArray = [N+N-1]boolean *)
    (* Tabsyn: , VarDecl([ (diag1, NameType(boolArray), NilExp)])
               , AssignStmt([SimpleVar(boolArray, diag1)], [NewExp(bool, OpExp(OpExp(VarExp(IntType, SimpleVar(N)),PlusOp,VarExp(IntType, SimpleVar(N))),MinusOp,IntExp(1)))])
      *)
    MOVE(temp "_t_diag_len_2n", BINOP(temp "N", PLUS, temp "N"));
    MOVE(temp "_t_diag_len", BINOP(temp "_t_diag_len_2n", MINUS, temp "ONE"));
    (* get 2n-1 *)
    MOVE(temo "_alloc_diag1_size", )
      BINOP(temp "_t_diag_len", ADD, temp "_arr_offset"); (* 8 bytes array header *)

    LABEL(label "_decl_diag1");
    CALL(label "_alloc_",
      (* size is (2n-1) x sizeof(bool) + 8 *)
        (* assume we knows that bool takes 1 byte *)
        [temp "_alloc_diag1_size"]
        [temp "diag1", temp "errno"]);
    MOVE(temp "diag1_body", (* body of an array begin at 8 bytes *)
      BINOP(temp "diag1", PLUS, temp "_arr_offset"));
          (* now, we need to initialize the array (to the zero-val) *)
          (* Tabsyn: /*
 , Scope(
    [ VarDecl(%init2, IntType, IntExp(1)) (* %init2 is i *)
    , ForStmt(
        OpExp(
          SimpleVar(IntType, %init2), 
          IntLtOp, 
          OpExp(
            OpExp(
              VarExp(IntType, SimpleVar(N)),
              PlusOp,
              VarExp(IntType, SimpleVar(N))),
            MinusOp,
            IntExp(1))) (* N+N-1 *)
        (* _continue scope _*)
      , Scope([ (* loop body *)
          Scope([
            AssignStmt( (* A <- B*)
              SubScriptVar( (* A is row[i] *)
                SimpleVar(boolArray, row), 
                IntExp(SimpleVar(IntType, %init2))), 
              BoolExp(false))]) (* row[i] <- false *)
        , AssignStmt( (* A <- B *)
            [(SimpleVar(IntType, %init2))], (* A is i *)
            [OpExp(
              SimpleVar(IntType, %init2), 
              PlusOp, 
              IntExp(1))])]))]) (* i <- i+1 *)
    */ *)
    LABEL(label "_init_diag1");
      MOVE(temp "%init2", CONST(0));
      LABEL(label "_init_diag1_loop_head_0");
      MOVE(temp "_t_init_diag1_loop_test", 
        ROP(temp "%init2", LT, temp "_t_diag_len"));
      CJUMP(temp "_init_diag1_loop_test",
        label "_l_init_diag1_loop_body_0",
        label "_l_init_diag1_loop_end_0");
      LABEL(label "_l_init_diag1_loop_body_0");
        MOVE(temp "_t_init_diag1_mem", 
          BINOP(temp "diag1_body", PLUS, temp "%init2"));
        STORE(temp "_t_init_diag1_mem", temp "ZERO");
        MOVE(temp "%init2", 
          BINOP(temp "%init2", PLUS, temp "ONE"));
        MOVE(TEMP "_t_init_diag1_loop_test", 
          ROP(temp "%init2", LT, temp "_t_diag_len"));
      CJUMP(temp "_init_diag1_loop_test",
        label "_l_init_diag1_loop_body_0",
        label "_l_init_diag1_loop_end_0");
      LABEL(label "_l_init_diag1_loop_end_0");
  ]) (* end irmain *)
] (* end ircode *)
