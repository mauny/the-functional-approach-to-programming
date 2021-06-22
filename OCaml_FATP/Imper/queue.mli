open Circular_list

(** +type_queue+ *)
type 'a queue = Emptyqueue | Queue of 'a lnode ;;

val enqueue : 'a -> 'a queue -> 'a queue ;;
val dequeue : 'a queue -> 'a queue * 'a ;;
val list_of_queue : 'a queue -> 'a list ;;
