(* #use "load.ml" ;; *)

open Termes
open Type_synthesis
open Ml_type

open Ml_exp1

(** +flat_map+ *)
let rec flat_map f = function
  | [] -> []
  | (a :: l) -> f a @ flat_map f l ;;

let rec except l a =
  match l with
  | []  -> []
  | (x :: l') -> if x = a then except l' a
    else x :: except l' a ;;

let rec subtract l1 l2 =
  match l2 with
  | []  -> l1
  | (a :: l) -> subtract (except l1 a) l ;;


(** +scheme+ *)
type 'a scheme = Forall of int list * 'a ;;

(** +unique+ *)
let rec unique = function
  | [] -> []
  | x :: l -> if List.mem x l then unique l else x :: unique l ;;

(** +vars_of_tyenv+ *)
let vars_of_tyenv env =
  flat_map (fun (_, Forall (gvars,t)) -> subtract (vars t) gvars) env ;;

(** +generalize+ *)
let generalize env t =
  let gvars =
    unique (subtract (vars t) (vars_of_tyenv env)) in
  Forall(gvars, t) ;;

(** +instance+ *)
let instance = function
    Forall (gvars,t) ->
    let renaming = List.map (fun n -> (n, Var (new_int()))) gvars in
    apply_subst renaming t ;;

(** +subst_but+ *)
let rec subst_but v = function
  | [] -> []
  | (v1, t1) :: subst ->
    if v1 = v then subst_but v subst
    else (v1, t1) :: (subst_but v subst) ;;

(** +subst_minus+ *)
let rec subst_minus subst vars =
  match vars with
  | [] -> subst
  | v :: vs -> subst_minus (subst_but v subst) vs ;;

(** +subst_env+ *)
let subst_env subst env =
  List.map (fun (k, Forall (gvars, t)) ->
      (k, Forall (gvars, apply_subst (subst_minus subst gvars) t))) env ;;

(** +term_map+ *)
let rec term_map fop fleaf = function
  | Term (oper, sons) -> Term (fop oper, List.map (term_map fop fleaf) sons)
  | Var (n) -> Var (fleaf n) ;;

(** +make_string_vars+ *)
let make_string_vars t =
  let var_of_int n =
    "v" ^ (string_of_int n) in
  term_map (fun x -> x) var_of_int t ;;

(** +unop_type+ *)
let unop_type = function
  | Ml_fst -> let a = Var (new_int ()) and b = Var (new_int ())
    in (pair (a, b), a)
  | Ml_snd -> let a = Var (new_int ()) and b = Var (new_int())
    in (pair (a, b), b) ;;

(** +binop_type+ *)
let binop_type = function
  | Ml_add  -> (const "int", const "int", const "int")
  | Ml_sub  -> (const "int", const "int", const "int")
  | Ml_mult -> (const "int", const "int", const "int")
  | Ml_eq   -> (const "int", const "int", const "bool")
  | Ml_less -> (const "int", const "int", const "bool") ;;

(** +type_expr+ *)
let rec type_expr tenv expr =
  match expr with
  | Ml_int_const n -> ([], const "int")
  | Ml_bool_const b -> ([], const "bool")
  | Ml_var s -> ([], try instance (List.assoc s tenv)
                 with Not_found -> failwith "Unbound variable")
  | Ml_fun(s, e) ->
    let alpha = Var (new_int ()) in
    let (su, t) = type_expr ((s, Forall ([], alpha)) :: tenv) e in
    (su, arrow (apply_subst su alpha, t))
  | Ml_let (s, e1, e2) ->
    let (su1, t1) = type_expr tenv e1 in
    let ts1 = generalize (subst_env su1 tenv) t1 in
    let (su2, t2) = type_expr ((s, ts1) :: (subst_env su1 tenv)) e2 in
    (compsubst su2 su1, t2)
  | Ml_app (e1, e2) ->
    let (su1, t1) = type_expr tenv e1 in
    let (su2, t2) = type_expr (subst_env su1 tenv) e2 in
    let alpha = Var (new_int ()) in
    let mu = unify (apply_subst su2 t1, arrow (t2, alpha)) in
    (compsubst mu (compsubst su2 su1), apply_subst mu alpha)
  | Ml_unop (unop, e) ->
    let (t_i, t_o) = unop_type unop in
    let (su, t) = type_expr tenv e in
    let mu = unify (t_i, t) in
    (compsubst mu su, apply_subst mu t_o)
  | Ml_pair (e1, e2) ->
    let (su1, t1) = type_expr tenv e1 in
    let (su2, t2) = type_expr (subst_env su1 tenv) e2 in
    (compsubst su2 su1, pair (apply_subst su2 t1, t2))
  | Ml_binop (binop, e1, e2) ->
    let (ta1, ta2, t_r) = binop_type binop in
    let (su1, t1) = type_expr tenv e1 in
    let mu1 = unify (t1, ta1) in
    let s1 = compsubst mu1 su1 in
    let (su2, t2) = type_expr (subst_env s1 tenv) e2 in
    let s2 = compsubst su2 s1 in
    let mu2 = unify (t2, apply_subst s2 ta2) in
    let s3 = compsubst mu2 s2 in
    (s3, apply_subst s3 t_r)
  | Ml_if (e1, e2, e3) ->
    let (su1, t1) = type_expr tenv e1 in
    let mu1 = unify (t1, const "bool") in
    let s1 = compsubst mu1 su1 in
    let (su2, t2) = type_expr (subst_env s1 tenv) e2 in
    let s2 = compsubst su1 s1 in
    let (su3, t3) = type_expr (subst_env s2 tenv) e3 in
    let s3 = compsubst su3 s2 in
    let mu3 = unify(t3, apply_subst su3 t2) in
    (compsubst mu3 s3, apply_subst mu3 t3)
  | Ml_letrec (s, e1, e2) ->
    let t1 = Var (new_int ()) in
    let ts1 = Forall ([], t1) in
    let (su1, t1) = type_expr ((s, ts1) :: tenv) e1 in
    let ts_s = generalize (subst_env su1 tenv) (apply_subst su1 t1) in
    let (su2, t2) = type_expr ((s, ts_s) :: (subst_env su1 tenv)) e2 in
    (compsubst su2 su1, t2) ;;

(** +type_of+ *)
let type_of e =
  reset_new_int ();
  let (su, t) = type_expr [] e in
  ml_type_of_term (make_string_vars t) ;;
