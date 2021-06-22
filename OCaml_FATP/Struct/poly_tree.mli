type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree ;;

val int_of_string : string -> int ;;
val tree_of_string : (string -> 'a) -> string -> 'a tree ;;
