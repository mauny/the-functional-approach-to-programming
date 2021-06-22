(* #use "load.ml" ;; *)

open Ml_ops
open Ml_lazy
open Lexer

let constr_of_string = function
  | "Nil"  -> Nil
  | "Cons" -> Cons
  |   _    -> raise (Failure "Wrong Constructor") ;;

let ml_parser =
  let rec atom =
    parser
  | [< 'INT n >]    -> Ml_int_const n
  | [< 'BOOL b >]   -> Ml_bool_const b
  | [< 'IDENT s >]  -> Ml_var s
  | [< 'CONSTR0 s>] -> Ml_constr0 (constr_of_string s)
  | [< 'LPAR; e = expr; 'RPAR >] -> e
  and tuple =
    let rec resttuple el =
      parser
    | [< 'COMMA; e = exp_in_tuple; el' = (resttuple (e :: el)) >] -> el'
    | [< 'RPAR>] -> el
    in
    parser
      [< 'LPAR; e = exp_in_tuple; el = (resttuple [e]) >] -> el
  and app =
    let rec restapp e1 =
      parser
    | [< e2 = atom; e = (restapp (match e1 with
             | Ml_var "fst" -> Ml_unop (Ml_fst, e2)
             | Ml_var "snd" -> Ml_unop (Ml_snd, e2)
             | e1 -> Ml_app (e1, e2))) >] -> e
    | [<>] -> e1
    in
    parser
  | [< e1 = atom; e = (restapp e1) >] -> e
  | [< 'CONSTRN s; el = tuple >]
    -> Ml_capp (constr_of_string s, List.rev el)

  and mult =
    let rec restmult e1 =
      parser
    | [< 'MULT; e2 = atom;
	     e = (restmult (Ml_binop (Ml_mult, e1, e2))) >] -> e
    | [<>] -> e1
    in
    parser
      [< e1 = app; e2 = (restmult e1) >] -> e2
  and add =
    let rec restadd e1 =
      parser
    | [< 'PLUS; e2 = atom;
	     e = (restadd (Ml_binop (Ml_add, e1, e2))) >] -> e
    | [< 'MINUS; e2 = atom;
	     e = (restadd (Ml_binop (Ml_sub, e1, e2))) >] -> e
    | [<>] -> e1
    in
    parser
      [< e1 = mult; e2 = (restadd e1) >] -> e2
  and comp =
    let rec restcomp e1 =
      parser
    | [< 'EQ; e2 = atom;
	     e = (restcomp (Ml_binop (Ml_eq, e1, e2))) >] -> e
    | [< 'LT; e2 = atom;
	     e = (restcomp (Ml_binop (Ml_less, e1, e2))) >] -> e
    | [<>] -> e1
    in
    parser
      [< e1 = add; e2 = (restcomp e1) >] -> e2
  and pair =
    let rec restpair e1 =
      parser
    | [< 'COMMA; e2 = comp >] -> Ml_pair (e1, e2)
    | [<>] -> e1
    in
    parser
      [< e1 = comp; e = (restpair e1) >] -> e

  and restlet =
    parser
  | [< 'IDENT x; 'EQ; e1 = expr; 'IN; e2 = expr >]
    ->  Ml_let (x, e1, e2)
  | [< 'REC; 'IDENT x; 'EQ; e1 = expr; 'IN; e2 = expr >]
    -> Ml_letrec (x, e1, e2)
  and restfun =
    parser
  | [< 'IDENT x; 'ARROW; e = expr >] -> Ml_fun (x, e)
  | [< c = case; cl = (restcases [c]) >] -> Ml_func (List.rev cl)
  and case =
    parser
  | [< 'LPAR; 'CONSTRN c; vl = vars; 'RPAR; 'ARROW; e = expr >] -> (constr_of_string c, vl, e)
  | [< 'CONSTR0 c; 'ARROW; e = expr >] -> (constr_of_string c, [], e)
  and restcases cl =
    parser
  | [< 'BAR; c = case; cl' = (restcases (c :: cl)) >] -> cl'
  | [<>] -> cl
  and vars =
    parser
  | [< 'LPAR; 'IDENT v; vl = (restvars [v]) >] -> vl
  and restvars vl =
    parser
  | [< 'COMMA; 'IDENT v; vl' = (restvars (v :: vl)) >] -> vl'
  | [< 'RPAR>] -> List.rev vl
  and expr =
    parser
  | [< 'IF; e1 = expr; 'THEN; e2 = expr; 'ELSE; e3 = expr >] -> Ml_if (e1, e2, e3)
  | [< 'FUN ; e = restfun >] -> e
  | [< 'LET; e = restlet >] -> e
  | [< e = pair >] -> e
  and exp_in_tuple =
    parser
  | [< 'IF; e1 = expr; 'THEN; e2 = expr; 'ELSE; e3 = expr >] -> Ml_if (e1, e2, e3)
  | [< 'FUN; e = restfun >] -> e
  | [< 'LET; e = restlet >] -> e
  | [< e = comp >] -> e
  in expr ;;

let parse_ml_exp s = ml_parser (lexer (Stream.of_string s)) ;;

let exp_of_string = parse_ml_exp ;;
