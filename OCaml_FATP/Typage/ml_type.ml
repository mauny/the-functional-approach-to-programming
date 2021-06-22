(* #use "load.ml" ;; *)

open Prelud
open Lexxer
open Termes

(** +ml_type+ *)
type ml_type =
  | Int_type | Bool_type
  | Pair_type of ml_type * ml_type
  | Arrow_type of ml_type * ml_type
  | Var_type of string ;;

(** +ml_type_of_term+ *)
let rec ml_type_of_term = function
  | Var s -> Var_type s
  | Term ("int", [])  -> Int_type
  | Term ("bool", []) -> Bool_type
  | Term ("pair", [t1; t2]) ->
    Pair_type (ml_type_of_term t1, ml_type_of_term t2)
  | Term ("arrow", [t1; t2]) ->
    Arrow_type (ml_type_of_term t1, ml_type_of_term t2) ;;

(** +parse_ml_type+ *)
let ml_type_parser =
  let rec mltype =
    parser
  | [< 'IDENT "int" >]  -> Int_type
  | [< 'IDENT "bool" >] -> Bool_type
  | [< 'IDENT s >]      -> Var_type s
  | [< 'LPAR; t1 = mltype; t = (resttype t1) >] -> t
  and resttype t1 =
    parser
  | [< 'COMMA; t2 = mltype; 'RPAR >] -> Pair_type (t1, t2)
  | [< 'ARROW; t2 = mltype; 'RPAR >] -> Arrow_type (t1, t2)
  in mltype ;;

let parse_ml_type s = ml_type_parser (lexer (Stream.of_string s)) ;;

let ml_type_of_string = parse_ml_type ;;

(** +ml_type_printer+ *)
let rec print_ml_type  = function
  | Int_type  -> print_string "int"
  | Bool_type -> print_string "bool"
  | Arrow_type (t1, t2) ->
    print_string "(";
    print_ml_type t1;
    print_string " ml_type_infix_arrow ";
    print_ml_type t2;
    print_string ")"
  | Pair_type (t1, t2) ->
    print_string "(";
    print_ml_type t1;
    print_string " * ";
    print_ml_type t2;
    print_string ")"
  | Var_type v -> print_string v ;;
