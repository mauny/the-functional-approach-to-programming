type comparison = Smaller | Equiv | Greater ;;
type 'a minmax = Min | Plain of 'a | Max ;;

val mk_order : ('a -> 'a -> bool) -> 'a -> 'a -> comparison ;;

(** val int_comp : '_a -> '_a -> comparison *)
val int_comp : int -> int -> comparison ;;

val mk_preorder :
  ('a -> 'b -> bool) * ('a -> 'b -> bool) -> 'a -> 'b -> comparison ;;

val inv_rel : ('a -> 'b -> comparison) -> 'a -> 'b -> comparison ;;

val extend_order :
  ('a -> 'b -> comparison) -> 'a minmax -> 'b minmax -> comparison ;;
