(* #use "load.ml" ;; *)

open Prelud
open Lexxer

let union l1 l2 =
  List.fold_right (fun e u -> if List.mem e u then u else e :: u) l1 l2 ;;

(** +term+ *)
type ('a,'b) term = Term of 'a * ('a,'b) term list
                  | Var of 'b ;;

let term_parser =
  let rec pars =
    parser
  | [< 'IDENT x; t = (rparser x) >] -> t
  and rparser x =
    parser
  | [< 'LPAR; l = (list_parser []); 'RPAR >]
    -> Term (x, List.rev l)
  | [<>] -> Var x
  and list_parser l =
    parser
  | [< t = pars; l' = (rlist_parser (t :: l)) >] -> l'
  | [<>] -> l
  and rlist_parser l =
    parser
  | [< 'COMMA; t = pars; l' = (rlist_parser (t :: l)) >] -> l'
  | [<>] -> l
  in pars ;;

let parse_term  = term_parser % lexer % Stream.of_string ;;

let term_of_string = parse_term ;;

let rec string_of_term = function
  | Term (x, []) -> x ^  "()"
  | Term (x, l)  -> x ^ "(" ^ string_of_term_list l ^ ")"
  | Var x        -> x
and string_of_term_list = function
  | [t]      -> string_of_term t
  | (t :: l) ->  string_of_term t ^ "," ^ string_of_term_list l
  | _        -> failwith "string_of_term_list" ;;

let rec print_term = function
  | Term (x, []) -> print_string (x ^ "()")
  | Term (x, l)  ->
    begin
      print_string x ;
      print_string "(" ;
      print_term_list l ;
      print_string ")"
    end
  | Var x        -> print_string x
and print_term_list = function
  | [t]      -> print_term t
  | (t :: l) ->
    begin
      print_term t ;
      print_string "," ;
      print_term_list l
    end
  | _        -> failwith "print_term_list" ;;

(** +term_trav+ *)
let rec term_trav f g start v = function
  | (Term (oper, sons)) -> f (oper, List.fold_right (g % (term_trav f g start v)) sons start)
  | (Var n) -> v n ;;

(** +vars+ *)
let vars t = term_trav snd union [] (fun x -> [x]) t ;;

(** +occurs+ *)
let occurs v t = List.mem v (vars t) ;;

(** +print_subst+ *)
let print_subst subst =
  List.iter (fun (x, t) -> print_string x;
              print_string "  -->  " ;
              print_term t ;
              print_newline ())
    subst ;;

(** +apply_subst+ *)
let rec apply_subst subst = function
  | (Term (f, tl)) -> Term (f, (List.map (apply_subst subst) tl))
  | (Var x as v) -> try List.assoc x subst
    with _ -> v ;;

(** +compsubst+ *)
let compsubst subst1 subst2 =
  (List.map (fun (v, t) -> (v, apply_subst subst1 t)) subst2)
  @ (let vs = List.map fst subst2
     in List.filter (fun (x, t) -> not ( List.mem x vs)) subst1) ;;

(** +compsubst2+ *)
let compsubst subst1 subst2 =
  (List.map (fun (v, t) -> (v, apply_subst subst1 t)) subst2) @ subst1 ;;

(** +som_subst+ *)
exception Match_exc ;;

let som_subst s1 s2 =
  List.fold_left (fun subst (x, t) -> try let u = List.assoc x subst in
                     if t = u then subst
                     else raise Match_exc
                   with Not_found -> (x, t) :: subst)
    s1 s2 ;;

(** +matching+ *)
let matching (t1, t2) =
  let rec matchrec subst = function
    | (Var v,t)  -> som_subst [v, t] subst
    | (t, Var v) -> raise Match_exc
    | (Term (op1, sons1), Term (op2, sons2)) ->
      if op1 = op2 then List.fold_left matchrec subst (List.combine sons1 sons2)
      else raise Match_exc
  in
  matchrec [] (t1, t2) ;;

(** +unify+ *)
exception Unify_exc ;;

let rec unify = function
  | (Var v, t2) -> if Var v = t2 then [] else
    if occurs v t2 then raise Unify_exc
    else [v, t2]
  | (t1, Var v) -> if occurs v t1 then raise Unify_exc
    else [v, t1]
  | (Term (op1, sons1), Term (op2, sons2)) ->
    if op1 = op2 then
      let subst_unif s (t1, t2) =
        compsubst (unify (apply_subst s t1, apply_subst s t2)) s
      in
      (List.fold_left subst_unif [] (List.combine sons1 sons2))
    else raise Unify_exc ;;

(** +unify_list+ *)
let unify_list tl =
  let subst_unif s (t1, t2) =
    compsubst (unify (apply_subst s t1, apply_subst s t2)) s
  in
  List.fold_left subst_unif [] tl ;;
