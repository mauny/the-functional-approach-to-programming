
(* +type_lnode+ *)
type 'a lnode = {info: 'a; mutable next: 'a lnode};;
(* +type_lnode+ *)


value mk_circular_list : 'a -> 'a lnode;;
value last : 'a lnode -> 'a;;
value first : 'a lnode -> 'a;;
value insert_head : 'a -> 'a lnode -> 'a lnode;;
value insert_tail : 'a -> 'a lnode -> 'a lnode;;
value elim_head : 'a lnode -> 'a lnode;;

