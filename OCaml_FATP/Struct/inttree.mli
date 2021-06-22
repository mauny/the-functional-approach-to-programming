type inttree = Leaf of int | Node of inttree * inttree ;;

val inttree_parser : Lexxer.token Stream.t -> inttree ;;
val inttree_of_string : string -> inttree ;;
