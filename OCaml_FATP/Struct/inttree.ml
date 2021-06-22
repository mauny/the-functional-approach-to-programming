(* #use "load.ml" ;; *)

open Lexxer

(** +inttree+*)
type inttree = Leaf of int | Node of inttree * inttree ;;

let inttree_parser =
  let rec pars =
    parser
  | [< 'LPAR; a1 = pars; 'COMMA;
       a2 = pars; 'RPAR >]
    -> Node (a1, a2)
  | [< 'INT n >] -> Leaf n
  in pars ;;

let parse_inttree s = inttree_parser (lexer (Stream.of_string s)) ;;

let inttree_of_string = parse_inttree ;;

(** +print_inttree+ *)
let rec print_inttree = function
  | (Leaf n) -> print_int n
  | (Node (t1, t2)) ->
    print_string "(";
    print_inttree t1;
    print_string ",";
    print_inttree t2;
    print_string ")" ;;
