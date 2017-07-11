#open "ml_ops";;

(*+ml_constructor+*)
type ml_constructor = Nil | Cons;;
(*+ml_constructor+*)

(*+ml_exp+*)
type ml_exp =
    Ml_int_const of int                                 (*> constante entière *)
 |  Ml_bool_const of bool                               (*> constante booléenne *)
 |  Ml_pair of ml_exp * ml_exp                  (*> paire *)
 |  Ml_unop of ml_unop * ml_exp                 (*> opération unaire *)
 |  Ml_binop of ml_binop * ml_exp * ml_exp              (*> opération de base *)
 |  Ml_var  of string                                   (*> variable *)
 |  Ml_constr0 of ml_constructor			(*> constructeur d'arité 0 *)
 |  Ml_if  of ml_exp * ml_exp * ml_exp                  (*> conditionnelle *)
 |  Ml_fun  of string * ml_exp                          (*> fonction *)
 |  Ml_func of 
       (ml_constructor * string list * ml_exp) list     (*> fonction par cas *)
 |  Ml_app  of ml_exp * ml_exp                          (*> application *)
 |  Ml_capp of ml_constructor * ml_exp list             (*> appl. de constructeur *)
 |  Ml_let of string * ml_exp * ml_exp                  (*> déclaration *)
 |  Ml_letrec of string * ml_exp * ml_exp               (*> déclaration récursive *)
;;
(*+ml_exp+*)

(*+val+*)
type val =
    Int_Const of int
 |  Bool_Const of bool
 |  Pair of val * val
 |  Clo  of (string * val) list * ml_exp        (*> fermeture *)
 |  Fre  of (string * val) list * ml_exp        (*> glaçon *)
 |  Constr0 of ml_constructor
 |  Constr_n of ml_constructor * val list         (*> valeur construite *)
;;
(*+val+*)

