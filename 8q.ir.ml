
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

  IRMAIN([

    (label "_start",
      [ (* bb start start *)
        MOVE(temp "ZERO", CONST(0)); (* const 0 *)
        MOVE(temp "ONE", CONST(1)); (* const 1 *)
        MOVE(temp "TWO", CONST(2)); (* const 2 *)
        MOVE(temp "THREE", CONST(3)); (* const 3 *)
        MOVE(temp "FOUR", CONST(4)); (* getting wordsize *)
        (* init global vals and vars. *)
        (* Base: const N = 8 *)
        (* Tabsyn: ConstDecl(N, IntType, IntExp(8)) *)
        MOVE(temp "N", CONST(8));
        MOVE(temp "_arr_offset", CONST(8)); (* 8 bytes array header *)
        JUMP(label "_l_decl_row")
      ] (* bb start end *)
    );
  
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
    (label "_l_decl_row",
      [ (* bb l_decl_row start *)
        MOVE(temp "_alloc_row_offset", (* 8N *)
          BINOP(temp "N", SHL, temp "THREE"));
        MOVE(temp "_alloc_row_size",  (* 8N+8 *)
          BINOP(temp "N", PLUS, temp "_arr_offset"));
        CALL(label "_alloc_",
        (* size is N x sizeof(bool) + 8 *)
          (* bool takes a word (8 bit) *)
          [temp "_alloc_row_size"],
            (* assuming, like C, we have a special `errno` that takes 
          return from system library calls *)
          [temp "row", temp "errno"]);
        MOVE(temp "row_body", (* body of an array begin at 8 bytes *)
          BINOP(temp "row", PLUS, temp "_arr_offset"));
        JUMP(label "_l_init_row")
      ] (* bb l_decl_row end *)
    );
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
    (label "_l_init_row", (* like the `scope` in tabsyn *)
      [ MOVE(temp "%init0", CONST(0));
        JUMP(label "_l_init_row_loop_head_0");
      ] (* end of _l_init_row *)
    );
      (* I'm going to rewrite a loop into 
        a do-while wrapped in if. Saves (n-1) instructions *)
      (label "_l_init_row_loop_head_0",
        [ (* ite test *)
          MOVE(temp "_t_init_row_loop_test", 
            ROP(temp "%init0", LT, temp "N"));
          CJUMP(temp "_init_row_loop_test",
            label "_l_init_row_loop_body_0",
            label "_l_init_row_loop_end_0")
        ] (* end of _l_init_row_loop_head_0 *)
      );



  (label "_l_init_row_loop_body_0",
    [ (* bb l_init_row_loop_body_0 start *)
      (* row[i] <- false *)
      (* STORE of temp * temp is MOVE MEM[t1] <- t2 *)
      (* row[i] is at (row+8+i) *)
      MOVE(temp "_t_init_row_offset",
        BINOP(temp "%init0", SHL, temp "THREE"));
      MOVE(temp "_t_init_row_mem", 
        BINOP(temp "row_body", PLUS, temp "_t_init_row_offset"));
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
    ] (* bb l_init_row_loop_body_0 end *)
  );
  
    (label "_l_init_row_loop_end_0",
      [ (* bb l_init_row_loop_end_0 start *)
        MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
        JUMP(label "_l_decl_col")
      ] (* bb l_init_row_loop_end_0 end *)
    );
        

    (* very cool, now do the same thing for col *)
    (* Base: var col intArray = [N]int *)
    (* Tabsyn: 
     , VarDecl([ (col, NameType(intArray); NilExp)])
     , AssignStmt([SimpleVar(intArray, col)], [NewExp(int, SimpleVar(IntType, N))] *)

    (label "_l_decl_col",
      [ (* bb l_decl_col start *)
        MOVE(temp "_alloc_col_body_bytes",
          BINOP(temp "N", SHL, temp "THREE"));
        MOVE(temp "_alloc_col_size", 
          BINOP(temp "_arr_offset", PLUS, temp "_alloc_col_body_bytes"));
        CALL(label "_alloc_",
            (* size is N x sizeof(int) + 8 *)
              (* assume we knows that int takes 4 byte *)
              [temp "_alloc_col_size"]
              [temp "col", temp "errno"]);
        MOVE(temp "col_body", (* body of an array begin at 8 bytes *)
          BINOP(temp "col", PLUS, temp "_arr_offset"));
        JUMP(label "_l_init_col")
      ] (* bb l_decl_col end *)
    );


    (* now, we need to initialize the array (to the zero-val) *)
    (* Tabsyn: /*
     , Scope([ VarDecl(%init1, IntType, IntExp(0))
             , ForStmt(OpExp(SimpleVar(IntType, %init1), IntLtOp, SimpleVar(IntType, N))
	             , Scope([ Scope ([ AssignStmt(SubScriptVar(SimpleVar(NameType(intArray), col), IntExp(SimpleVar(IntType, %init1))), IntExp(0))])
		     , AssignStmt([(SimpleVar(IntType, %init1))], [OpExp(SimpleVar(IntType, %init1), PlusOp, IntExp(1))])]))])
*/ *)
    (label "_l_init_col",
      [ (* bb l_init_col start *)
        MOVE(temp "%init1", CONST(0));
        JUMP(label "_l_init_col_loop_head_0")
      ] (* bb l_init_col end *)
    );

      (label "_l_init_col_loop_head_0",
        [ (* bb l_init_col_loop_head_0 start *)
          MOVE(temp "_t_init_col_loop_test", 
            ROP(temp "%init1", LT, temp "N"));
          CJUMP(temp "_init_col_loop_test",
            label "_l_init_col_loop_body_0",
            label "_l_init_col_loop_end_0")
        ] (* bb l_init_col_loop_head_0 end *)
    );

      (label "_l_init_col_loop_body_0",
        [ (* bb l_init_col_loop_body_0 start *)
          MOVE(temp "_t_init_col_offset",
            BINOP(temp "%init1", SHL, temp "THREE"));
          MOVE(temp "_t_init_col_mem", 
            BINOP(temp "col_body", PLUS, temp "_t_init_col_offset"));
          STORE(temp "_t_init_col_mem", temp "ZERO");
          MOVE(temp "%init1", 
            BINOP(temp "%init1", PLUS, temp "ONE"));
          MOVE(TEMP "_t_init_col_loop_test", 
            ROP(temp "%init1", LT, temp "N"));
          CJUMP(temp "_init_col_loop_test",
            label "_l_init_col_loop_body_0",
            label "_l_init_col_loop_end_0")
        ] (* bb l_init_col_loop_body_0 end *)
    );

      (label "_l_init_col_loop_end_0",
        [ (* bb l_init_col_loop_end_0 start *)
          MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
          JUMP(label "_decl_diag1_setup")
        ] (* bb l_init_col_loop_end_0 end *)
    );

    (* do diag1 now. *)
    (* Base: var diag1 boolArray = [N+N-1]boolean *)
    (* Tabsyn: , VarDecl([ (diag1, NameType(boolArray), NilExp)])
               , AssignStmt([SimpleVar(boolArray, diag1)], [NewExp(bool, OpExp(OpExp(VarExp(IntType, SimpleVar(N)),PlusOp,VarExp(IntType, SimpleVar(N))),MinusOp,IntExp(1)))])
      *)
    (label "_l_decl_diag1_setup",
      [ (* bb l_decl_diag1_setup start *)
        MOVE(temp "_t_diag_len_2n", BINOP(temp "N", PLUS, temp "N"));
        MOVE(temp "_t_diag_len", 
          BINOP(temp "_t_diag_len_2n", MINUS, temp "ONE"));
        (* get 2n-1 *)
        MOVE(temp "_t_alloc_diag1_OFFSET", 
          BINOP(temp "_t_diag_len", SHL, temp "THREE"));
        MOVE(temp "_alloc_diag1_size", 
          BINOP(temp "_t_diag_OFFSET", ADD, temp "_arr_offset")); (* 8 bytes array header *)
        JUMP(label "_l_decl_diag1")
      ] (* bb l_decl_diag1_setup end *)
    );

    (label "_l_decl_diag1",
      [ (* bb l_decl_diag1 start *)
        CALL(label "_alloc_",
      (* size is (2n-1) x sizeof(bool) + 8 *)
      (* assume we knows that bool takes 1 byte *)
          [temp "_alloc_diag1_size"]
          [temp "diag1", temp "errno"]);
        MOVE(temp "diag1_body", (* body of an array begin at 8 bytes *)
          BINOP(temp "diag1", PLUS, temp "_arr_offset"));
        JUMP(label "_l_init_diag1")
      ] (* bb l_decl_diag1 end *)
    );
          (* now, we need to initialize the array (to the zero-val) *)
          (* Tabsyn: /*
 , Scope(
    [ VarDecl(%init2, IntType, IntExp(1)) (* %init2 is i *)
    , ForStmt(
        OpExp(
          SimpleVar(IntType, %init2); 
          IntLtOp, 
          OpExp(
            OpExp(
              VarExp(IntType, SimpleVar(N));
              PlusOp,
              VarExp(IntType, SimpleVar(N)));
            MinusOp,
            IntExp(1))) (* N+N-1 *)
        (* _continue scope _*)
      , Scope([ (* loop body *)
          Scope([
            AssignStmt( (* A <- B*)
              SubScriptVar( (* A is row[i] *)
                SimpleVar(boolArray, row); 
                IntExp(SimpleVar(IntType, %init2))); 
              BoolExp(false))]) (* row[i] <- false *)
        , AssignStmt( (* A <- B *)
            [(SimpleVar(IntType, %init2))], (* A is i *)
            [OpExp(
              SimpleVar(IntType, %init2); 
              PlusOp, 
              IntExp(1))])]))]) (* i <- i+1 *)
    */ *)
    (label "_l_init_diag1",
      [ (* bb l_init_diag1 start *)
        MOVE(temp "%init2", CONST(0));
        JUMP(label "_l_init_diag1_loop_head_0")
      ] (* bb l_init_diag1 end *)
    );

    (label "_l_init_diag1_loop_head_0",
      [ (* bb l_init_diag1_loop_head_0 start *)
        MOVE(temp "_t_init_diag1_loop_test", 
          ROP(temp "%init2", LT, temp "_t_diag_len"));
        CJUMP(temp "_init_diag1_loop_test",
          label "_l_init_diag1_loop_body_0",
          label "_l_init_diag1_loop_end_0")
      ] (* bb l_init_diag1_loop_head_0 end *)
    );

    (label "_l_init_diag1_loop_body_0",
      [ (* bb l_init_diag1_loop_body_0 start *)
        MOVE(temp "_t_init_diag1_offset",
          BINOP(temp "%init2", SHL, temp "THREE"));
        MOVE(temp "_t_init_diag1_mem", 
          BINOP(temp "diag1_body", PLUS, temp "_t_init_diag1_offset"));
        STORE(temp "_t_init_diag1_mem", temp "ZERO");
        MOVE(temp "%init2", 
          BINOP(temp "%init2", PLUS, temp "ONE"));
        MOVE(TEMP "_t_init_diag1_loop_test", 
          ROP(temp "%init2", LT, temp "_t_diag_len"));
        CJUMP(temp "_init_diag1_loop_test",
          label "_l_init_diag1_loop_body_0",
          label "_l_init_diag1_loop_end_0")
      ] (* bb l_init_diag1_loop_body_0 end *)
    );

    (label "_l_init_diag1_loop_end_0",
        [ (* bb l_init_diag1_loop_end_0 start *)
          MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
          JUMP(label "_decl_diag2_setup")
        ] (* bb l_init_diag1_loop_end_0 end *)
    );
    (* nearly same way, do diag2 *)
    (* Base: var diag2 boolArray = [N+N-1]boolean *)
    (* Tabsyn: /*
 , VarDecl([ (diag2, NameType(boolArray); NilExp)]) (* diag2 init to nil *)
 , AssignStmt(
    [SimpleVar(boolArray, diag2)], (* diag2 to alloc_B *)
    [NewExp(bool, (* alloc bool with length *)
      OpExp( (* (N+N)-1 *)
        OpExp(
          VarExp(IntType, SimpleVar(N));
          PlusOp,
          VarExp(IntType, SimpleVar(IntType, N)));
        MinusOp,
      IntExp(1)))])
*/ *)
  (label "_decl_diag2_setup",
    [ (* bb l_decl_diag2_setup start *)
      MOVE(temp "_t_diag2_len_2n", BINOP(temp "N", PLUS, temp "N"));
      MOVE(temp "_t_diag2_len", 
        BINOP(temp "_t_diag2_len_2n", MINUS, temp "ONE"));
      MOVE(temp "_t_diag2_offset",
        BINOP(temp "_t_diag2_len", SHL, temp "THREE"));
      MOVE(temp "_alloc_diag2_size", 
        BINOP(temp "_t_diag2_offset", ADD, temp "_arr_offset")); (* 8 bytes array header *)
      JUMP(label "_l_decl_diag2")
    ] (* bb l_decl_diag2_setup end *)
  );
 
  (label "_l_decl_diag2",
    [ (* bb l_decl_diag2 start *)
      CALL(label "_alloc_",
        (* size is (2n-1) x sizeof(bool) + 8 *)
        (* assume we knows that bool takes 1 byte *)
        [temp "_alloc_diag2_size"]
        [temp "diag2", temp "errno"]);
      MOVE(temp "diag2_body", (* body of an array begin at 8 bytes *)
        BINOP(temp "diag2", PLUS, temp "_arr_offset"));
      JUMP(label "_l_init_diag2")
    ] (* bb l_decl_diag2 end *)
  );
      (* now, we need to initialize the array (to the zero-val) *)
      (* Tabsyn: /*
 , AssignStmt(
    SubScriptVar( 
      SimpleVar(boolArray, row); 
      IntExp(0)); 
    BoolExp(false))
 , Scope([ 
    VarDecl(%init2, IntType, IntExp(2)) 
  , ForStmt(
      OpExp( (* For i < N+N-1 *)
        SimpleVar(IntType, %init2); 
        IntLtOp, 
        OpExp( (* N+N-1 *)
          OpExp(
            VarExp(IntType, SimpleVar(IntType, N));
            PlusOp,
            VarExp(IntType, SimpleVar(IntType, N)));
          MinusOp,
          IntExp(1)))
    , Scope([ (* loop body *)
        Scope ([ (* loop body scope *)
          AssignStmt( (* A <- B *)
            SubScriptVar( (* A is row[i] *)
              SimpleVar(boolArray, row); 
              IntExp(SimpleVar(IntType, %init2))); 
            BoolExp(false))]) (* row[i] <- false *) 
          (* continue scope *)
      , AssignStmt( (* A <- B *)
        [(SimpleVar(IntType, %init2))], (* A is i *)
        [OpExp(
          SimpleVar(IntType, %init2); 
          PlusOp, 
          IntExp(1))])]))]) (* i <- i+1 *)
*/ *)
    (label "_l_init_diag2",
      [ (* bb l_init_diag2 start *)
        MOVE(temp "%init2", CONST(0));
        JUMP(label "_init_diag2_loop_head_0")
      ] (* bb l_init_diag2 end *)
    );

    (label "_init_diag2_loop_head_0",
      [ (* bb l_init_diag2_loop_head_0 start *)
        MOVE(temp "_t_init_diag2_loop_test", 
          ROP(temp "%init2", LT, temp "_t_diag2_len"));
        CJUMP(temp "_init_diag2_loop_test",
          label "_l_init_diag2_loop_body_0",
          label "_l_init_diag2_loop_end_0");
      ] (* bb l_init_diag2_loop_head_0 end *)
    );

    (label "_l_init_diag2_loop_body_0",
      [ (* bb l_init_diag2_loop_body_0 start *)
        MOVE("_t_init_diag2_offset", 
          BINOP(temp "%init2", SHL, temp ""))
        MOVE(temp "_t_init_diag2_mem", 
          BINOP(temp "diag2_body", PLUS, temp "%init2"));
        STORE(temp "_t_init_diag2_mem", temp "ZERO");
        MOVE(temp "%init2", 
          BINOP(temp "%init2", PLUS, temp "ONE"));
        MOVE(TEMP "_t_init_diag2_loop_test", 
          ROP(temp "%init2", LT, temp "_t_diag2_len"));
        CJUMP(temp "_init_diag2_loop_test",
          label "_l_init_diag2_loop_body_0",
          label "_l_init_diag2_loop_end_0");
      ] (* bb l_init_diag2_loop_body_0 end *)
    );

    (label "_l_init_diag2_loop_end_0",
      [ (* bb l_init_diag2_loop_end_0 start *)
        MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
        JUMP(label "_irmain_to_func_main")
      ] (* nop *)
    );
    
    (* now, we need to call the main function *)
    (label "_irmain_to_func_main",
      [ (* bb start irmain_to_func_main *)
        CALL(label "_func_main", [], []);
        JUMP(label "_end_irmain")
      ]
    ); (* bb end irmain_to_func_main *)
    (* end of main *)
    (* so I treat main as a function *)
    (label "_end_irmain",
      [ (* bb end start *)
        CALL (label "_sys_exit", [temp "ZERO"], []);
        (* no return, just exit *)
      ]
    ) (* bb end end *)
  ]) (* end irmain *)
  (* assuming all in IRMAIN are visible *)

  (* func main ()->() *)
  (* Base: func main ()->() *)
  (* There is no tabsyn decl for main *)
  IRFUNC(label "_func_main", [
    (* Base: try(0) *)
    (label "_main",
      [ (* bb start main *)
        CALL(label "_func_try", [temp "ZERO"], []);
        JUMP(label "_end_main")
      ]
    );

    (label " _end_main",
      [ (* bb end main *)
        MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
        JUMP(label "%__SECRET_INTERNAL_DONT_SET_THIS_OR_YOU_WILL_BE_FIRED")
      ]
    ) (* end main *)
  ]); (* end irmain *)

  (* func printBoard ()->() *)
  IRFUNC(label "_func_printBoard", [
    


  ]);

  (* func try (c int) -> () *)
  IRFUNC(label "_func_try", []);


] (* end ircode *)
