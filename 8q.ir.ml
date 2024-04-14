
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
  IRSTRING("O");
  IRSTRING(".");
  IRSTRING("\n");

  IRMAIN([

    (label "_start",
      [ (* bb start start *)
        MOVE(temp "ZERO", CONST(0)); (* const 0 *)
        MOVE(temp "ONE", CONST(1)); (* const 1 *)
        MOVE(temp "TWO", CONST(2)); (* const 2 *)
        MOVE(temp "THREE", CONST(3)); (* const 3 *)
        MOVE(temp "FOUR", CONST(4)); (* getting wordsize *)
        MOVE(temp "SEVEN", CONST(7));
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
  (* Base: /*
    func printBoard() {
      for i:=0; i<N; i=i+1 {
        for j:=0; j<N; j=j+1 {
          if col[i]==j { write("O")
          } else { write(".") }
        }
        write("\n")
      }
      write("\n")
    }
    */ *)
  (* Tabsyn: /*
 (* func printBoard() { *)
 , FunDecl( printBoard,[],[]
  (* name: printBoard, varList: null, retList: null *)
      (* for i:=0; i<N; i=i+1 { *)
      (* _hoist stmt out of `for'_ *)
  , Scope ([ 
      VarDecl([(i, IntType), IntExp(0)])
    , ForStmt( (* for i < N *)
        OpExp( (* outer loop header *)
          VarExp(SimpleVar(IntType, i)), 
          IntLtOp, 
          VarExp(SimpleVar(IntType, N))),
      , Scope ([ (* outer loop body *)
        , Scope ([ (* for j:=0; j<N; j=j+1 { *)
            VarDecl([(j, IntType), IntExp(0)]) (* init j <- 0 *)
          , ForStmt( (* for j < N *)
              OpExp( (* inner loop header *)
                VarExp(SimpleVar(IntType, j)), 
                IntLtOp, 
                VarExp(SimpleVar(IntType, N))),
            , Scope ([ (* inner loop body *)
              , Scope ([ (* this is the body scope *)
                  IfElseStmt(  (* if (col[i]==j) *)
                    SubScriptVar( (* col[j] *)
                      SimpleVar(NameType(intArray), col), 
                      IntEqOp, 
                      SimpleVar(IntType, j)),
                      (* { write("O") } *)
                  , Scope([ (* 'then' of the 'if' *)
                      CallStmt( (* apply write (list "O") *)
                        write, 
                        [(StringType, StringExp("O"))]) ])
                      (* else { write(".") } *)
                  , Scope([ 
                      CallStmt( (* apply write (list ".") *)
                        write, 
                        [(StringType, StringExp("."))]) ])) ])
                  (* this is the continue scope *)
              , AssignStmt( (* j <- j+1 *)
                [SimpleVar(IntType, j)], 
                [OpExp(
                  SimpleVar(IntType, j), 
                  PlusOp, 
                  IntExp(1))]) ]))
              (* done the inner loop *)
          , CallStmt( (* apply write (list "\n") *)
              write, 
              [(StringType, StringExp("\n"))]) ])
              (* this is the continue scope *)
        , AssignStmt( (* i <- i+1 *)
            [SimpleVar(IntType, i)], 
            [OpExp(
              SimpleVar(IntType, i), 
              PlusOp, 
              IntExp(1))]) ]))
      (* done the outer loop *)
    , CallStmt(write, [(StringType, StringExp("\n"))]) ]))
  *)

  IRFUNC(label "_func_printBoard", [
    (* Translate all loops into:
      if(cond) { do {...} while(cond); }
    *)
    (* First, init the i, test and link *)
    (label "_f_pb_loop_outer_first_0" [
      MOVE(temp "_t_i_pb_0", CONST(0));
      MOVE(temp "_t_i_pb_test_0",
        ROP(temp "_t_i_pb_0", LT, temp "N"));
      CJUMP(temp "_t_i_pb_test_0",
        label "_l_pb_loop_outer_body_0",
        label "_l_pb_loop_outer_fi_0")
      ]);

    (* Outer loop body *)
    (label "_l_pb_loop_outer_body_0", [
      (* nop *)    
      MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
      JUMP(label "_f_pb_loop_inner_first_0")
    ]);

    (* Inner loop header *)
    (label "_f_pb_loop_inner_first_0", [
      MOVE(temp "_t_j_pb_0", CONST(0));
      MOVE(temp "_t_j_pb_test_0",
      ROP(temp "_t_j_pb_0", LT, temp "N"));
      CJUMP(temp "_t_j_pb_test_0",
        label "_l_pb_loop_inner_body_0",
        label "_l_pb_loop_inner_fi_0")
    ]);
    
    (label "_l_pb_loop_inner_body_0", [
      (* nop *)
      MOVE(temp "_t_pb_inner_offset",
        BINOP(temp "_t_i_pb_0", SHL, temp "THREE"));
      MOVE(temp "_t_pb_inner_mem", (* &col[i] *)
        BINOP(temp "col_body", PLUS, temp "_t_pb_inner_offset"));
      LOAD(temp "_t_pb_coli_val", temp "_t_pb_inner_mem");
      
      (* if (col[i]==j) *)
      MOVE(temp "_t_pb_inner_test",
        ROP(temp "_t_pb_coli_val", EQ, temp "_t_j_pb_0"));
      CJUMP(temp "_t_pb_inner_test",
        label "_l_pb_inner_then_0",
        label "_l_pb_inner_else_0")
    ]);
    (* Inner loop then *)
    (label "_l_pb_inner_then_0", [
      CALL(label "_write", [temp "_str_O"], []);
      JUMP(label "_l_pb_inner_last_0")
    ]);
    (* Inner loop else *)
    (label "_l_pb_inner_else_0", [
      CALL(label "_write", [temp "_str_."], []);
      JUMP(label "_l_pb_inner_last_0")
    ]);
    (* Inner loop last *)
    (label "_l_pb_inner_last_0", [
      MOVE(temp "_t_j_pb_0", (* j <- j+1 *)
        BINOP(temp "_t_j_pb_0", PLUS, temp "ONE"));
      JUMP(label "_l_pb_loop_test_0")
    ]);
    (* Inner loop test *)
    (label "_l_pb_loop_test_0", [
      MOVE(temp "_t_j_pb_test_0", 
        ROP(temp "_t_j_pb_0", LT, temp "N"));
      CJUMP(temp "_t_j_pb_test_0",
        label "_l_pb_loop_inner_body_0",
        label "_l_pb_loop_inner_fi_0")
    ]);
    (* Inner loop fi *)
    (label "_l_pb_loop_inner_fi_0", [
      MOVE(temo "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
      JUMP(label "_l_pb_loop_outer_last_0")
    ]);
    
    (* Outer loop last *)
    (label "_l_pb_loop_outer_last_0", [
      CALL(label "_write", [temp "_str_nl"], []);
      MOVE(temp "_t_i_pb_0", (* i <- i+1 *)
        BINOP(temp "_t_i_pb_0", PLUS, temp "ONE"));
    ]);
    (* Outer loop test *)
    (label "_l_pb_loop_outer_test_0", [
      MOVE(temp "_t_i_pb_test_0",
        ROP(temp "_t_i_pb_0", LT, temp "N"));
      CJUMP(temp "_t_i_pb_test_0",
        label "_l_pb_loop_outer_body_0",
        label "_l_pb_loop_outer_fi_0")
    ]);
        
    (* Outer loop fi *)
    (label "_l_pb_loop_outer_fi_0", [
      MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
      JUMP(label "_l_pb_fn_end_0")
    ]);
  
    (* End of fn printBoard *)
    (label "_l_pb_fn_end_0", [
      CALL(label "_write", [temp "_str_nl"], []);
      JUMP(label "_l_pb_fn_fi")
    ]);

    (* Fi of fn printBoard *)
    (label "_l_pb_fn_fi", [
      MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
      JUMP(label "%__SECRET_INTERNAL_DONT_SET_THIS_OR_YOU_WILL_BE_FIRED")
    ]);
  ]);

  (* Base: /*
func try (c int) {
  if c==N { 
    printBoard()
  } else {
    for r:=0; r<N; r=r+1 {
      if !row[r] && !diag1[r+c] && !diag2[r+7-c] {
        row[r] = true; diag1[r+c] = true; diag2[r+7-c] = true
        col[c] = r
        try(c+1)
        row[r] = false; diag1[r+c] = false; diag2[r+7-c] = false
      }
    }
  }
}
  */ *)
  (* Tabsyn: /* 
, FunDecl( try, [(IntType, c)],[] 
  (* name: try, varList: c:int, retList: null *)
  , Scope ([  (* func body start here: *)
    IfElseStmt( (* if (c==N) *)
      OpExp( (* c == N *)
        SimpleVar(IntType, c), 
        IntEqOp, 
        SimpleVar(IntType, N))
      (* thenL { printBoard() } *)
    , Scope ([ CallStmt(printBoard, []) ])
    , Scope ([ (* else scope from ite *)
        Scope ([ (* r := 0, for the loop *)
          VarDecl([(r, IntType), IntExp(0)])
            (* for ... r < N ... { *)
        , ForStmt( 
            OpExp( (* r < N *)
              SimpleVar(IntType, r), 
              IntLtOp, 
              SimpleVar(IntType, N))
          , Scope ([ (* loop body *)
              Scope ([ (* the body scope of the loop *)
                (* if !row[r] && !diag1[r+c] && !diag2[r+7-c] *)
              , IfElseStmt ( (* _rewrite AndOp to multiple IfStmt *)
                  SubScriptVar( (* testing on !row[r] *)
                    SimpleVar(NamedType(boolArray), row), 
                    SimpleVar(IntType, r))
                , Scope ([]) (* if row[r], nothing, else: *)
                , IfElseStmt ( (* passed !row[r], now test !diag1[r+c] *)
                    SubScriptVar( (* testing on !diag1[r+c] *)
                      SimpleVar(NamedType(boolArray), diag1), 
                      OpExp(
                        SimpleVar(IntType, r), 
                        PlusOp, 
                        SimpleVar(IntType, c)))
                  , Scope ([]) (* if diag1[r+c], nothing, else: *)
                  , IfElseStmt ( (* passed !diag1[r+c], now test !diag2[r+7-c] *)
                      SubScriptVar( (* testing on !diag2[r+7-c] *)
                        SimpleVar(NamedType(boolArray), diag2), 
                        OpExp(
                          OpExp(
                            SimpleVar(IntType, r), 
                            PlusOp, 
                            IntExp(7)),
                          MinusOp, 
                          SimpleVar(IntType, c)))
                        , Scope ([]) (* if diag2[r+7-c], nothing, else: *)
                        , Scope ([ (* all three cases are passed *)
                            AssignStmt([ (* As <- Bs *)
                              SubScriptVar( (* A1 is row[r] *)
                                SimpleVar(boolArray, row),   
                                SimpleVar(IntType, r))
                            , SubScriptVar( (* A2 is diag1[r+c] *)
                                SimpleVar(NamedType(boolArray), diag1), 
                                OpExp( (* r+c *)
                                  SimpleVar(IntType, r), 
                                  PlusOp, 
                                  SimpleVar(IntType, c)))
                            , SubScriptVar( (* A3 is diag2[r+7-c] *)
                                SimpleVar(NamedType(boolArray), diag2), 
                                OpExp( (* (r+7)-c *)
                                  OpExp(
                                    SimpleVar(IntType, r), 
                                    PlusOp, 
                                    IntExp(7)), 
                                  MinusOp, 
                                  SimpleVar(IntType, c)))]
                                (* Bs is [false, false, false] *)
                          , [ BoolExp(true), BoolExp(true), BoolExp(true) ])
                                
                          , AssignStmt([ (* col[c] <- r *)
                              SubScriptVar(
                                SimpleVar(NamedType(intArray), col), 
                                SimpleVar(IntType, c))], 
                              [SimpleVar(IntType, r)])
                                   
                          , CallExp( (* (apply try (c+1)) *)
                              try, 
                              OpExp( 
                                SimpleVar(IntType, c), 
                                PlusOp, 
                                IntExp(1)))
                                           
                          , AssignStmt([ (* As <- Bs *)
                              SubScriptVar( (* A1 is row[r] *)
                                SimpleVar(boolArray, row),   
                                SimpleVar(IntType, r))
                            , SubScriptVar( (* A2 is diag1[r+c] *)
                                SimpleVar(NamedType(boolArray), diag1), 
                                OpExp(
                                  SimpleVar(IntType, r),
                                  PlusOp, 
                                  SimpleVar(IntType, c)))
                            , SubScriptVar( (* A3 is diag2[r+7-c] *)
                                SimpleVar(
                                  NamedType(boolArray), diag2), 
                                  OpExp( (* r+7-c *)
                                    OpExp(
                                      SimpleVar(IntType, r), 
                                      PlusOp, 
                                      IntExp(7)), 
                                    MinusOp, 
                                    SimpleVar(IntType, c)))]
                            (* Bs is [false, false, false] *)
                          , [ BoolExp(false), BoolExp(false), BoolExp(false)])])
                          (* outside of the *)
                          )))]) (* end of the loop body scope *)
                      (* continue scope *)
                      (* ... r=r+1 ... *)
            , AssignStmt( (* r <- r+1 *)
                [SimpleVar(IntType, r)], 
                [OpExp(
                  SimpleVar(IntType, r), 
                  PlusOp, 
                  IntExp(1))])]))])]))])) (* end of the function body *)
  */ *)
  (* func try (c int) -> () *)
  IRFUNC(label "_func_try", [
    (* assume temp _arg_c is passed in *)
    (* fn preambles *)
    (label "_f_try_preamble", [
      MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
      JUMP(label "_f_try_if_0_header")
    ]);
  (* if (c==N) *)
  (label "_f_try_if_0_header", [
    MOVE(temp "_t_try_if_0_test",
      ROP(temp "_t_try_arg_c", EQ, temp "N"));
    CJUMP(temp "_t_try_if_0_test",
      label "_f_try_if_0_then",
      label "_f_try_if_0_else")
    ]);
  (* then *)
  (label "_f_try_if_0_then", [
    CALL(label "_func_printBoard", [], []);
    JUMP(label "_f_try_if_0_fi")
  ]);
  (* if_0 fi *)
  (label "_f_try_if_0_fi", [
    MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
    JUMP(label "_f_try_fn_end")
  ]);
  (* else *)
  (label "_f_try_if_0_else", [
    MOVE(temp "_t_try_r_0", CONST(0));
    JUMP(label "_f_try_loop_first_0")
  ]);
  (* loop *)
  (label "_f_try_loop_first_0", [
    MOVE(temp "_t_try_loop_test_0",
      ROP(temp "_t_try_r_0", LT, temp "N"));
    CJUMP(temp "_t_try_loop_test_0",
      label "_f_try_loop_body_0",
      label "_f_try_loop_fi_0")
    ]);
  (* loop test *)
  (label "_f_try_loop_test_0", [
    MOVE(temp "_t_try_loop_test_0",
      ROP(temp "_t_try_r_0", LT, temp "N"));
    CJUMP(temp "_t_try_loop_test_0",
      label "_f_try_loop_body_0",
      label "_f_try_loop_fi_0")
    ]);
  (* loop fi *)
  (label "_f_try_loop_fi_0", [
    MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
    JUMP(label "_f_try_if_0_fi")
  ]);
  
  (* loop body *)
  (label "_f_try_loop_body_0" [
    MOVE(temp "zero", BINOP(temp "ZERO", OR, temp "ZERO"));
    JUMP(label "_f_try_if_1_header_0")
  ]);

  (* loop last *)
  (label "_f_try_loop_last_0", [
    MOVE(temp "_t_try_r_0",
      BINOP(temp "_t_try_r_0", PLUS, temp "ONE"));
    JUMP(label "_f_try_loop_test_0")
  ]);

  (* if 1: !row[r] *)
  (label "_f_try_if_1_header_0"[
    MOVE(temp "_t_row_r_offset",
      BINOP(temp "_t_try_r_0", SHL, temp "THREE"));
    MOVE(temp "_t_row_r_mem",
      BINOP(temp "row_body", PLUS, temp "_t_row_r_offset"));
    LOAD(temp "_t_row_r_val", temp "_t_row_r_mem");
    MOVE(temp "_f_try_if_1_cond_0",
      ROP(temp "_t_row_r_val", NEQ, temp "ZERO"));
    CJUMP(temp "_f_try_if_1_cond_0",
      label "_f_try_if_2_header_0",
      label "_f_try_if_fin_1")
  ]);

  (* if 2: !diag1[r+c] *)
  (label "_f_try_if_2_header_0", [
    MOVE(temp "_t_rc",
      BINOP(temp "_t_try_r_0", PLUS, temp "_t_try_arg_c"));
    MOVE(temp "_t_diag1_rc_offset",
      BINOP(temp "_t_rc", SHL, temp "THREE"));
    MOVE(temp "_t_diag1_rc_mem",
      BINOP(temp "_t_diag1_rc_offset", PLUS, temp "diag1_body"));
    LOAD(temp "_t_diag1_rc_val", temp "_t_diag1_rc_mem");
    MOVE(temp "_f_try_if_2_test_0",
      ROP(temp "_t_diag1_rc_val", NEQ, temp "ZERO"));
    CJUMP(temp "_t_diag1_rc_val",
      label "_f_try_if_3_header_0",
      label "_f_try_if_fin_1")]);

  (* if 3: !diag2[r+7-c] *)
  (label "_f_try_if_3_header_0", [
    MOVE(temp "_t_rmc",
      BINOP(temp "_t_try_r_0" MINUS, temp "_t_try_arg_c"));
    MOVE(temp "_t_rp7mc",
      BINOP(temp "_t_rmc" PLUS, temp "SEVEN"));
    MOVE(temp "_t_diag2_rp7mc_offset",
      BINOP(temp "_t_rp7mc", SHL, temp "THREE"));
    MOVE(temp "_t_diag2_rp7mc_mem",
      BINOP(temp "_t_diag2_rp7mc_offset", PLUS, temp "diag2_body"));
    LOAD(temp "_t_diag2_rp7mc_val", temp "_t_diag2_rp7mc_mem");
    MOVE(temp "_f_try_if_3_test_0",
      ROP(temp "_t_diag2_rp7mc_val", NEQ, temp "ZERO"));
    CJUMP(temp "_f_try_if_3_test_0",
      label "_f_try_if_body_1",
      label "_f_try_if_fin_1")]);


  (* if body *)
  (label "_f_try_if_body_1", [
    (* row[r] = true *)
    STORE(temp "_t_row_r_mem", temp "ONE");
    (* diag1[r+c] = true *)
    STORE(temp "_t_diag1_rc_mem", temp "ONE");
    (* diag2[r+7-c] = true *)
    STORE(temp "_t_diag2_rp7mc_mem", temp "ONE");
    (* col[c] = r *)
    MOVE(temp "_t_col_c_offset",
      BINOP(temp "_t_try_arg_c", SHL, temp "THREE"));
    MOVE(temp "_t_col_c_mem",
      BINOP(temp "col_body", PLUS, temp "_t_col_c_offset"));
    STORE(temp "_t_col_c_mem", temp "_t_try_r_0");
    (* try(c+1) *)
    MOVE(temp "_t_try_cp1",
      BINOP(temp "_t_try_arg_c", PLUS, temp "ONE"));
    CALL(label "_func_try", 
      [temp "_t_try_cp1"], []);

    (* row[r] = false *)
    STORE(temp "_t_row_r_mem", temp "ZERO");
    (* diag1[r+c] = false *)
    STORE(temp "_t_diag1_rc_mem", temp "ZERO");
    (* diag2[r+7-c] = false *)
    STORE(temp "_t_diag2_rp7mc_mem", temp "ZERO");
    JUMP(label "_f_try_if_fin_1")]);

  
  
  (* if fin 1 *)(* jump to f_try_loop_last_0 *)
  (label "_f_try_if_fin_1", [
    MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
    JUMP(label "_f_try_loop_last_0")
  ]);

  (* fn try fn end *) (* view-unshift and return *)
  (label "_f_try_fn_end", [
    MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
    JUMP(label "%__SECRET_INTERNAL_DONT_SET_THIS_OR_YOU_WILL_BE_FIRED")
  ]);
  (* end of try *)
  ]


] (* end ircode *)
