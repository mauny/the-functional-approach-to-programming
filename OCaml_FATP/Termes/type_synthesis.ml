(* #use "load.ml" ;; *)

open Prelud
open Lexxer

open Ml_ops
open Ml_strict
open Parser_strict

open Termes


(** +ml_type+ *)
type ml_type =
  | Int_type | Bool_type
  | Pair_type of ml_type * ml_type
  | Arrow_type of ml_type * ml_type
  | Var_type of string ;;

(** +ml_type_of_term+ *)
let rec ml_type_of_term = function
  | Var s     ->    Var_type s
  | Term ("int", []) -> Int_type
  | Term ("bool", []) -> Bool_type
  | Term ("pair", [t1; t2]) ->
    Pair_type (ml_type_of_term t1, ml_type_of_term t2)
  | Term ("arrow", [t1; t2]) ->
    Arrow_type (ml_type_of_term t1, ml_type_of_term t2)
  | _ -> failwith "ml_type_of_term" ;;

(** +parse_ml_type+ *)
let ml_type_parser =
  let rec mltype =
    parser
  | [< 'IDENT "int" >] -> Int_type
  | [< 'IDENT "bool" >] -> Bool_type
  | [< 'IDENT s >]  ->  Var_type s
  | [<'LPAR; t1 = mltype; t = (resttype t1) >] -> t
  and resttype t1 =
    parser
  | [< 'COMMA; t2 = mltype; 'RPAR >]
    -> Pair_type (t1, t2)
  | [< 'ARROW; t2 = mltype; 'RPAR >]
    -> Arrow_type (t1, t2)
  in
  mltype ;;

let parse_ml_type s = ml_type_parser (lexer (Stream.of_string s)) ;;

let ml_type_of_string = parse_ml_type ;;

(** +ml_type_printer+ *)
let rec print_ml_type  = function
    Int_type  -> print_string "int"
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

(** +var_const_arrow+ *)
let var n = Var ("v" ^ (string_of_int n)) ;;
let const c = Term (c, []) ;;
let pair (t1, t2) = Term ("pair", [t1; t2]) ;;
let arrow (t1, t2) = Term ("arrow", [t1; t2]) ;;

(** +new_int+ *)
let (new_int, reset_new_int) =
  let c = ref (-1) in
  (fun () -> c := !c+1 ; !c),
  (fun () -> c := -1) ;;

(** +unop_type+ *)
let unop_type = function
  | Ml_fst -> let a = var (new_int ()) and b = var (new_int ())
    in (pair (a, b), a)
  | Ml_snd -> let a = var (new_int ()) and b = var (new_int ())
    in (pair (a, b), b) ;;

(** +binop_type+ *)
let binop_type = function
  | Ml_add  -> (const "int", const "int", const "int")
  | Ml_sub  -> (const "int", const "int", const "int")
  | Ml_mult -> (const "int", const "int", const "int")
  | Ml_eq   -> (const "int", const "int", const "bool")
  | Ml_less -> (const "int", const "int", const "bool") ;;

(** +generate_type_constraints+ *)
let generate_type_constraints e =
  let rec gen n tenv = function
    | (Ml_int_const _) -> [var n, const "int"]
    | (Ml_bool_const _) -> [var n, const "bool"]
    | (Ml_unop (op, e)) ->
      let (t1, t2) = unop_type op
      and n_e = new_int () in
      (var n, t2) :: (var n_e, t1) :: (gen n_e tenv e)
    | (Ml_binop (op, e1, e2)) ->
      let (t1, t2, t3) = binop_type op
      and n1 = new_int () and n2 = new_int () in
      (var n, t3) :: (var n1, t1) :: (var n2, t2)
      :: (gen n1 tenv e1 @ gen n2 tenv e2)
    | (Ml_pair (e1, e2)) ->
      let n1 = new_int () and n2 = new_int () in
      (var n, (pair (var n1, var n2))) :: (gen n1 tenv e1 @ gen n2 tenv e2)
    | (Ml_var x) -> [var n, List.assoc x tenv]
    | (Ml_if (e1, e2, e3)) ->
      let n1 = new_int () and n2 = new_int ()
      and n3 = new_int () in
      (var n1, const "bool") :: (var n, var n2) :: (var n, var n3)
      :: ((gen n1 tenv e1) @ (gen n2 tenv e2) @ (gen n3 tenv e3))
    | (Ml_fun (x, e)) ->
      let n_x = new_int () and n_e = new_int () in
      (var n, arrow (var n_x, var n_e)) ::
      (gen n_e ((x, var n_x) :: tenv) e)
    | (Ml_app (Ml_var "Rec", Ml_fun (f, e))) ->
      let n_f = new_int () and n_e = new_int () in
      (* (var n, var n_e) :: *)
      (var n, var n_e) ::
      (* (gen n_e ((f, var n) :: tenv) e) *)
      (gen n_e ((f, var n_f) :: tenv) e)
    | (Ml_app (e1, e2)) ->
      let n1 = new_int () and n2 = new_int () in
      (var n1, arrow (var n2, var n)) ::
      (gen n1 tenv e1 @ gen n2 tenv e2)
    | _ -> failwith "Not implemented"
  in
  reset_new_int (); gen (new_int ()) [] e ;;

(*
generate_type_constraints
  (ml_exp_of_string "fun x -> x)") ;;
*)

(** +ml_type_of_term+ *)
let rec ml_type_of_term = function
  | (Var s) -> Var_type s
  | (Term ("int", [])) -> Int_type
  | (Term ("bool", [])) -> Bool_type
  | (Term ("pair", [t1; t2])) ->
    Pair_type (ml_type_of_term t1, ml_type_of_term t2)
  | (Term ("arrow", [t1; t2])) ->
    Arrow_type (ml_type_of_term t1, ml_type_of_term t2)
  | _ -> failwith "ml_type_of_term" ;;

(** +synthetize_type+ *)
let synthetize_type e =
  ml_type_of_term
    (Termes.apply_subst (Termes.unify_list (generate_type_constraints e))
       (var 0)) ;;

(*
synthetize_type (ml_exp_of_string "fun x -> x)") ;;
(* - : ml_type = Arrow_type (Var_type "v1", Var_type "v1") *)

synthetize_type (ml_exp_of_string "fun f -> (fun x -> f (f x))") ;;

synthetize_type
 (ml_exp_of_string "fun f -> (fun g -> (fun x -> f (g x)))") ;;
*)

