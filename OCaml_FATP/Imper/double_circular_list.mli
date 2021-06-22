(** +type_dbl_node+ *)
type 'a dblnode = {info:'a; mutable prev: 'a dblnode;
                   mutable next: 'a dblnode} ;;

val mk_dbl_circular_list : 'a -> 'a dblnode ;;
val insert_before : 'a -> 'a dblnode -> unit ;;
val insert_after : 'a -> 'a dblnode -> unit ;;
val elim : 'a dblnode -> 'a dblnode ;;
