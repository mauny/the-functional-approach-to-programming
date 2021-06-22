(* #use "load.ml" ;; *)

open Ml_ops
open Lexer
open Ml_strict

let ml_parser =
  let rec atom =
    parser
  | [< 'INT n >] -> Ml_int_const n
  | [< 'BOOL b >] -> Ml_bool_const b
  | [< 'IDENT s >]  ->  Ml_var s
  | [< 'LPAR; e = expr; 'RPAR >] -> e
  and app =
    let rec restapp e1 =
      parser
    | [< e2 = atom; e = (restapp (match e1 with
             | Ml_var "fst" -> Ml_unop (Ml_fst, e2)
             | Ml_var "snd" -> Ml_unop (Ml_snd, e2)
             | e1 -> Ml_app (e1, e2))) >] -> e
    | [<>] -> e1
    in parser [< e1 = atom; e = (restapp e1) >] -> e
  and mult =
    let rec restmult e1 =
      parser
	    [< 'MULT; e2 = app;
	       e = (restmult (Ml_binop (Ml_mult, e1, e2))) >] -> e
    | [<>] -> e1
    in parser [< e1 = app; e2 = (restmult e1) >] -> e2
  and add =
    let rec restadd e1 =
      parser
	    [<' PLUS; e2 = mult;
	      e = (restadd (Ml_binop (Ml_add, e1, e2))) >] -> e
    | [< 'MINUS; e2 = mult;
	     e = (restadd (Ml_binop (Ml_sub, e1, e2))) >] -> e
    | [<>] -> e1
    in parser [< e1 = mult; e2 = (restadd e1) >] -> e2
  and comp =
    let rec restcomp e1 =
      parser
    | [< 'EQ; e2 = atom;
	     e = (restcomp (Ml_binop (Ml_eq, e1, e2))) >] -> e
    | [< 'LT; e2 = atom;
	     e = (restcomp (Ml_binop (Ml_less, e1, e2))) >] -> e
    | [<>] -> e1
    in parser [< e1 = add; e2 = (restcomp e1) >] -> e2
  and pair =
    let rec restpair e1 =
      parser
    | [< 'COMMA; e2 = comp >] -> Ml_pair (e1, e2)
    | [<>] -> e1
    in parser [< e1 = comp; e = (restpair e1) >] -> e
  and restlet =
    parser
  | [< 'IDENT x; 'EQ; e1 = expr; 'IN; e2 = expr >]
    ->  Ml_let (x, e1, e2)
  | [< 'REC; 'IDENT x; 'EQ; e1 = expr; 'IN; e2 = expr >]
    ->  Ml_letrec (x, e1, e2)
  and expr =
    parser
  | [< 'IF; e1 = expr; 'THEN; e2 = expr; 'ELSE; e3 = expr >]
    ->  Ml_if (e1, e2, e3)
  | [< 'FUN ; 'IDENT x; 'ARROW; e = expr >]
    ->  Ml_fun (x, e)
  | [< 'LET; e = restlet >] -> e
  | [< e = pair >] -> e
  in expr ;;

let parse_ml_exp s = ml_parser (lexer (Stream.of_string s)) ;;

let ml_exp_of_string = parse_ml_exp ;;

(** +ml_exp_printer+ *)
let string_of_unop = function
  | Ml_fst -> "fst"
  | Ml_snd -> "snd" ;;

let string_of_binop = function
  | Ml_add -> "+"
  | Ml_sub -> "-"
  | Ml_mult -> "*"
  | Ml_eq -> "="
  | Ml_less -> "<" ;;

let rec print_ml_exp = function
  | Ml_int_const n -> print_int n
  | Ml_bool_const true -> print_string "true"
  | Ml_bool_const false -> print_string "false"
  | Ml_unop (op, e) -> print_string (string_of_unop op);
    print_string "(";
    print_ml_exp e;
    print_string ")"
  | Ml_binop (op, e1, e2) -> print_ml_exp e1;
    print_string (string_of_binop op);
    print_ml_exp e2
  | Ml_pair(e1, e2)  -> print_string "(";
    print_ml_exp e1;
    print_string ",";
    print_ml_exp e2;
    print_string ")"
  | Ml_var s -> print_string s
  | Ml_if (e1, e2, e3) -> print_string "if ";
    print_ml_exp e1;
    print_string " then ";
    print_ml_exp e2;
    print_string " else ";
    print_ml_exp e3
  | Ml_fun (x, e) -> print_string "(fun ";
    print_string x;
    print_string " -> ";
    print_ml_exp e;
    print_string ")"
  | Ml_app (e1, e2) -> print_ml_exp e1;
    print_string " ";
    print_ml_exp e2
  | Ml_let (x, e1, e2) -> print_string "let ";
    print_string x;
    print_string " = ";
    print_ml_exp e1;
    print_string " in ";
    print_ml_exp e2
  | Ml_letrec (x, e1, e2) -> print_string "let rec ";
    print_string x;
    print_string " = ";
    print_ml_exp e1;
    print_string " in ";
    print_ml_exp e2 ;;
