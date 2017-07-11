(* +type_comparison+ *)
type comparison = Smaller | Equiv | Greater;;
(* +type_comparison+ *)

(* +minmax+ *)
type 'a minmax = Min | Plain of 'a | Max;;
(* +minmax+ *)

value mk_order : ('a -> 'a -> bool) -> 'a -> 'a -> comparison;;
value mk_preorder : ('a -> 'b -> bool) * ('a -> 'b -> bool) 
                          -> 'a -> 'b -> comparison;;
value int_comp : int -> int -> comparison;;
value inv_rel : ('a -> 'b -> comparison) -> 'a -> 'b -> comparison;;
value extend_order : ('a -> 'b -> comparison) -> 'a minmax -> 'b minmax 
                          -> comparison;;
