#open "lexer";;

(*+exp+*)
type exp = Constant of int
         | Variable of string
         | Addition of exp * exp
         | Multiplication of exp * exp;;
(*+exp+*)

value exp_parser : token stream -> exp;;
value parse_exp : string -> exp;;
value exp_of_string : string -> exp;;


