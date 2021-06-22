(* #use "load.ml" ;; *)

open Prelud
open Lexxer

(** +gentree+*)
type 'a gentree = GenNode of 'a * 'a gentree list ;;

let ident_max_length = 48 ;;
let ident_buf = Bytes.make ident_max_length ' ' ;;

let int_parser =
  parser [< 'INT n >] -> n ;;

let parse_int s = int_parser (lexer (Stream.of_string s)) ;;
let int_of_string = parse_int ;;

let string_parser  =
  parser [< 'IDENT id >] -> id ;;

let parse_string s = string_parser (lexer (Stream.of_string s)) ;;
let parse_ident= parse_string ;;
let string_of_string= parse_string ;;
let string_of_ident= parse_string ;;


let string_int_parser  =
  parser [< 'LBRACK; 'IDENT id; 'SEMIC; 'INT n; 'RBRACK >]
  -> (id, n) ;;

let parse_string_int s = string_int_parser (lexer (Stream.of_string s)) ;;

let string_of_stream =
  let rec sos len str =
    try let c =
          ( match Stream.peek str with
            | Some x -> x
            | None -> raise Stream.Failure )
      in
      if List.mem c [ '('; ','; ')' ]
      then Bytes.to_string (Bytes.sub ident_buf 0 len)
      else
        (Bytes.set ident_buf len c ;
         match str with
           parser [<'_c; r = (sos (succ len) ) >] -> r )
    with Stream.Failure -> Bytes.to_string (Bytes.sub ident_buf 0 len)
  in
  sos 0 ;;

let gentree_parser parse =
  let rec pars =
    parser [< e = (parse % string_of_stream);
              tl = rparser >] -> GenNode (e, List.rev tl)
  and rparser =
    parser
  | [< ''('; l = (list_parser []); '')' >] -> l
  | [<>] -> []
  and list_parser l =
    parser
  | [< t = pars; l' = (rlist_parser (t :: l)) >] -> l'
  | [<>] -> l
  and rlist_parser l =
    parser
  | [< '','; t = pars; l' = (rlist_parser (t :: l)) >] -> l'
  | [<>] -> l
  in pars ;;

let parse_gentree parse =
  (gentree_parser parse) % Stream.of_string ;;

let gentree_of_string = parse_gentree ;;

let rec print_gentree print = function
  | GenNode (x, []) -> print x
  | GenNode (x, l)  -> print x; print_string "(" ;
    print_gentree_list print l ;
    print_string ")"
and print_gentree_list print = function
  | []       -> ()
  | (t :: l) -> print_gentree print t ;
    List.iter (fun t -> print_string "," ; print_gentree print t)
      l ;;

let string_of_gentree = print_gentree ;;
