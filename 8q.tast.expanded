Prog([
 , (* const N = 8 *)
  ConstDecl(N, IntType, IntExp(8))

   (* type boolArray [] bool *)
, TypeDecl(boolArray, ArrayType(BoolType))

 (* type intArray [] int *)
 , TypeDecl(intArray, ArrayType(IntType))0

   (* var row boolArray = [N] bool *)
 , VarDecl([ (row, NameType(boolArray), NilExp)])
 , AssignStmt( (* _allocate_ *)
    [SimpleVar(boolArray, row)], 
    [NewExp(bool, SimpleVar(IntType, N))])

   (* _initialize with zero value using loop over internal variable_ *)
   (* _scope variable *)
 , Scope([
    VarDecl(%init0, IntType, IntExp(0))
  , ForStmt( (* implicit loop (desugar) *)
      OpExp( (* i < N *)
        SimpleVar(IntType, %init0), 
        IntLtOp, 
        SimpleVar(IntType, N))
        (* _loop body_ *)
    , Scope([
        Scope([
          AssignStmt( (* A <- B *)
            SubScriptVar( (* A is row[i] *)
              SimpleVar(boolArray, row), 
              IntExp(SimpleVar(IntType, %init0)))
          , BoolExp(false))]) (* B is false *)
          (* _continue scope_ *)
      , AssignStmt( (* A <- B *)
          [(SimpleVar(IntType, %init0))], (* A is i *)
          [OpExp( (* B is (i+1) *)
              SimpleVar(IntType, %init0), 
              PlusOp, 
              IntExp(1))])]))])

   (* var col intArray = [N] int { } *)
 , VarDecl([ (col, NameType(intArray), NilExp)])
 , AssignStmt( (* allocate for `col` *)
      [SimpleVar(intArray, col)], 
      [NewExp(int, SimpleVar(IntType, N))])
 , Scope([ (* initializer... *)
      VarDecl(%init1, IntType, IntExp(0))
    , ForStmt(
        OpExp( (* for i < N *)
          SimpleVar(IntType, %init1), 
          IntLtOp, 
          SimpleVar(IntType, N))
    , Scope(
      [Scope( 
        [AssignStmt(
          SubScriptVar(
            SimpleVar(NameType(intArray), col), 
            IntExp(SimpleVar(IntType, %init1))), 
          IntExp(0))])
    (* for-loop post amble *)
    , AssignStmt(
        [(SimpleVar(IntType, %init1))], 
        [OpExp(
          SimpleVar(IntType, %init1), 
          PlusOp, 
          IntExp(1))])]))])

   (* var diag1 boolArray = [N+N-1]bool { false } *)
 , VarDecl([ (diag1, NameType(boolArray), NilExp)])
 , AssignStmt(
    [SimpleVar(boolArray, diag1)], 
    [NewExp( (* allocate (N+N-1) bools *)
      bool,
      OpExp( (* (N+N) -1 *)
        OpExp(
          VarExp(IntType, SimpleVar(N)),
          PlusOp,
          VarExp(IntType, SimpleVar(N))),
        MinusOp, IntExp(1)))])
   (* one initializer given, rest set to zero-value *)
 , AssignStmt( (* A <- B *)
    SubScriptVar( (* A is row[0] *)
      SimpleVar(boolArray, row), 
      IntExp(0)), 
    BoolExp(false)) (* row[0] <- false *)
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

   (* var diag2 boolArray = [N+N-1]bool { false, false } *)
 , VarDecl([ (diag2, NameType(boolArray), NilExp)]) (* diag2 init to nil *)
 , AssignStmt(
    [SimpleVar(boolArray, diag2)], (* diag2 to alloc_B *)
    [NewExp(bool, (* alloc bool with length *)
      OpExp( (* (N+N)-1 *)
        OpExp(
          VarExp(IntType, SimpleVar(N)),
          PlusOp,
          VarExp(IntType, SimpleVar(IntType, N))),
        MinusOp,
      IntExp(1)))])

   (* two initializers given, rest set to zero-value *)
 , AssignStmt(
    SubScriptVar( 
      SimpleVar(boolArray, row), 
      IntExp(0)), 
    BoolExp(false))
 , Scope([ 
    VarDecl(%init2, IntType, IntExp(2)) 
  , ForStmt(
      OpExp( (* For i < N+N-1 *)
        SimpleVar(IntType, %init2), 
        IntLtOp, 
        OpExp( (* N+N-1 *)
          OpExp(
            VarExp(IntType, SimpleVar(IntType, N)),
            PlusOp,
            VarExp(IntType, SimpleVar(IntType, N))),
          MinusOp,
          IntExp(1)))
    , Scope([ (* loop body *)
        Scope ([ (* loop body scope *)
          AssignStmt( (* A <- B *)
            SubScriptVar( (* A is row[i] *)
              SimpleVar(boolArray, row), 
              IntExp(SimpleVar(IntType, %init2))), 
            BoolExp(false))]) (* row[i] <- false *) 
          (* continue scope *)
      , AssignStmt( (* A <- B *)
        [(SimpleVar(IntType, %init2))], (* A is i *)
        [OpExp(
          SimpleVar(IntType, %init2), 
          PlusOp, 
          IntExp(1))])]))]) (* i <- i+1 *)

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

(* func try (c int) { *)
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

 , (* { try(0) } *)
 (* main statment block *)
   Scope ([ CallStmt(try,[IntExp(0)])])])
  (* end of the program *)
