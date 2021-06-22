open Prelud
open Lexxer

(* *+term+*)
type ('a,'b) term =
  | Term of 'a * ('a,'b) term list
  | Var of 'b ;;

val term_of_string :  string -> (string, string) term ;;
val print_term : (string, string) term -> unit ;;

val term_trav : ('a * 'b -> 'c) -> ('c -> 'b -> 'b) -> 'b ->
  ('d -> 'c) -> ('a, 'd) term -> 'c ;;
val vars : ('a, 'b) term -> 'b list ;;
val occurs : 'a -> ('b, 'a) term -> bool ;;
val print_subst : (string * (string, string) term) list -> unit ;;
val apply_subst : ('a * ('b, 'a) term) list -> ('b, 'a) term
  -> ('b, 'a) term ;;
val compsubst : ('a * ('b, 'a) term) list -> ('a * ('b, 'a) term) list
  -> ('a * ('b, 'a) term) list ;;
val som_subst : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list ;;
val matching : ('a, 'b) term * ('a, 'c) term -> ('b * ('a, 'c) term) list ;;
val unify : ('a, 'b) term * ('a, 'b) term -> ('b * ('a, 'b) term) list ;;
val unify_list : (('a, 'b) term * ('a, 'b) term) list -> ('b * ('a, 'b) term) list ;;
