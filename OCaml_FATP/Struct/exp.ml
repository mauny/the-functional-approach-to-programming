(* #use "load.ml" ;; *)

open Lexxer

(** +exp+*)
type exp =
  | Constant of int
  | Variable of string
  | Addition of exp * exp
  | Multiplication of exp * exp ;;

let exp_parser =
  let rec atom =
    parser
  | [< 'INT n >] -> Constant n
  | [< 'IDENT s >] -> Variable s
  | [< 'LPAR; e = expr; 'RPAR >] -> e
  and mult =
    let rec restmult e1 =
      parser
    | [< 'MULT; e2 = atom;
	     e = (restmult (Multiplication (e1, e2))) >]
	  -> e
	| [<>] -> e1
    in parser [< e1 = atom; e2 = (restmult e1) >] -> e2
  and expr =
    let rec restexp e1 =
      parser
    | [< 'PLUS; e2 = atom;
	     e = (restexp (Addition (e1, e2))) >] -> e
	| [<>] -> e1
    in parser [< e1 = mult; e2 = (restexp e1) >] -> e2
  in expr ;;

let parse_exp s = exp_parser (lexer (Stream.of_string s)) ;;

let exp_of_string = parse_exp ;;
