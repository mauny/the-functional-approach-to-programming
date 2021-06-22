(* #use "load.ml" ;; *)

open Prelud
open Lexxer
open Binary_trees


let ident_max_length = 48 ;;

let ident_buf = Bytes.make ident_max_length ' ' ;;

let int_parser =
  parser [< 'INT n >] -> n ;;

let parse_int s = int_parser (lexer (Stream.of_string s)) ;;
let int_of_string = parse_int ;;

let string_parser  =
  parser [< 'IDENT id >] -> id ;;

let parse_string s = string_parser (lexer (Stream.of_string s)) ;;
let parse_ident = parse_string ;;
let string_of_ident = parse_string ;;
let ident_of_string = parse_string ;;

let string_int_parser  =
  parser [<'LBRACK; 'IDENT id; 'SEMIC; 'INT n; 'RBRACK >]
  ->  (id, n) ;;

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

let btree_parser parse =
  let rec pars =
    parser | [<''('; '')'>]  -> Empty
    | [< e = (parse % string_of_stream); t = (rparser e) >] -> t
  and rparser x =
    parser | [<''('; t1 = pars; '','; t2 = pars; '')'>]
    -> Bin (t1, x, t2)
    | [<>] -> Bin (Empty, x, Empty)
  in pars ;;

let parse_btree parse =
  (btree_parser parse) % Stream.of_string ;;

let btree_of_string = parse_btree ;;

let rec print_btree (print: 'a -> unit) =
  function | Empty -> print_string "()"
           | Bin (t1, x, t2)
             -> print x ;
             if t1 <> Empty || t2 <> Empty
             then ( print_string "(" ;
                    print_btree print t1 ;
                    print_string "," ;
                    print_btree print t2 ;
                    print_string ")") ;;
