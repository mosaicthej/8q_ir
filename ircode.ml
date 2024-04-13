
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
