#open "lexer";;

(*+inttree+*)
type inttree = Leaf of int | Node of inttree * inttree;;
(*+inttree+*)


value inttree_parser : token stream -> inttree;;
value inttree_of_string : string -> inttree;;

