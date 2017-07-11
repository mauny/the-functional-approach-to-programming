(*+ml_binop+*)
type ml_unop = Ml_fst | Ml_snd;;
type ml_binop = Ml_add | Ml_sub | Ml_mult | Ml_eq | Ml_less;;
(*+ml_binop+*)

(*+ml_exp+*)
type ml_exp =
    Ml_int_const of int                         (*> constante entière *)
 |  Ml_bool_const of bool                       (*> constante booléenne *)
 |  Ml_pair of ml_exp * ml_exp                  (*> paire *)
 |  Ml_unop of ml_unop * ml_exp                 (*> opération unaire *)
 |  Ml_binop of ml_binop * ml_exp * ml_exp      (*> opération binaire *)
 |  Ml_var  of string                           (*> variable *)
 |  Ml_if  of ml_exp * ml_exp * ml_exp          (*> conditionnelle *)
 |  Ml_fun  of string * ml_exp                  (*> fonction *)
 |  Ml_app  of ml_exp * ml_exp                  (*> application *)
 |  Ml_let of string * ml_exp * ml_exp          (*> déclaration *)
 |  Ml_letrec of string * ml_exp * ml_exp       (*> déclaration récursive *)
;;
(*+ml_exp+*)

(*+val+*)
type val =
    Int_Const of int
 |  Bool_Const of bool
 |  Pair of val * val
 |  Clo  of (string * val) list * ml_exp
;;
(*+val+*)
