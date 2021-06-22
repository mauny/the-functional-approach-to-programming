(* #use "load.ml" ;; *)

open Prelud
open Lexxer


(** +tree+ *)
type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree ;;

let int_parser =
  let rec pars =
    parser [< 'INT n >] -> n
  in pars ;;

let parse_int s = int_parser (lexer (Stream.of_string s)) ;;
let int_of_string = parse_int ;;

let string_parser  =
  let rec pars =
    parser [< 'IDENT id >] -> id
  in pars ;;

let parse_string s = string_parser (lexer (Stream.of_string s)) ;;
let string_of_string = parse_string ;;

let string_of_stream =
  let rec sos len str =
    try
      let c =
        ( match Stream.peek str with
          | Some x -> x
          | None -> raise Stream.Failure )
      in
      if List.mem c [ '('; ','; ')' ]
      then Bytes.to_string (Bytes.sub ident_buf 0 len)
      else (Bytes.set ident_buf len c ;
            match str with
              parser [< '_c; r = (sos (succ len) ) >] -> r)
    with Stream.Failure -> Bytes.to_string (Bytes.sub ident_buf 0 len)
  in
  sos 0 ;;

let tree_parser parse =
  let rec pars =
    parser
  | [< ''('; a1 = pars; '','; a2 = pars; '')' >]
    -> Node (a1, a2)
  |   [< n = (parse % string_of_stream) >] -> Leaf n
  in pars ;;

let parse_tree parse = (tree_parser parse) % Stream.of_string ;;
let tree_of_string = parse_tree ;;
