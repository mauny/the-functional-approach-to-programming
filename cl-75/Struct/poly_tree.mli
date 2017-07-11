#open "../Util/prelude";;
#open "../Util/lexer";;


(*+tree+*)
type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree;;
(*+tree+*)

value int_of_string : string -> int;;
value tree_of_string : (string -> 'a) -> string -> 'a tree;;
