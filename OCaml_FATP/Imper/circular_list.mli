(** +type_lnode+ *)
type 'a lnode = { info : 'a; mutable next: 'a lnode; } ;;

val mk_circular_list : 'a -> 'a lnode ;;
val last : 'a lnode -> 'a ;;
val first : 'a lnode -> 'a ;;
val insert_head : 'a -> 'a lnode -> 'a lnode ;;
val insert_tail : 'a -> 'a lnode -> 'a lnode ;;
val elim_head : 'a lnode -> 'a lnode ;;
