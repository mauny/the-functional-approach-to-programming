(* #use "load.ml" ;; *)

open Ml_ops

(** +ml_constructor+*)
type ml_constructor = Nil | Cons ;;

(** +ml_exp+*)
type ml_exp =
  | Ml_int_const of int                                     (* integer constant *)
  | Ml_bool_const of bool                                   (* Boolean constant *)
  | Ml_pair of ml_exp * ml_exp                                          (* pair *)
  | Ml_unop of ml_unop * ml_exp                              (* unary operation *)
  | Ml_binop of ml_binop * ml_exp * ml_exp                    (* base operation *)
  | Ml_var  of string                                               (* variable *)
  | Ml_constr0 of ml_constructor                         (* nullary constructor *)
  | Ml_if  of ml_exp * ml_exp * ml_exp                           (* conditional *)
  | Ml_fun  of string * ml_exp                                      (* function *)
  | Ml_func of
      (ml_constructor * string list * ml_exp) list (* pattern matching function *)
  | Ml_app  of ml_exp * ml_exp                                   (* application *)
  | Ml_capp of ml_constructor * ml_exp list                (* constructor appl. *)
  | Ml_let of string * ml_exp * ml_exp                           (* declaration *)
  | Ml_letrec of string * ml_exp * ml_exp ;;           (* recursive declaration *)

(** +valu+*)
type valu =
  | Int_Const of int
  | Bool_Const of bool
  | Pair of valu * valu
  | Clo of (string * valu) list * ml_exp                             (* closure *)
  | Fre of (string * valu) list * ml_exp                               (* thunk *)
  | Constr0 of ml_constructor
  | Constr_n of ml_constructor * valu list ;;                (* data structures *)
