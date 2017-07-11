

(* +type_dbl_node+ *)
type 'a dblnode = {info:'a; mutable prev: 'a dblnode; 
                           mutable next: 'a dblnode};;
(* +type_dbl_node+ *)


value mk_dbl_circular_list : 'a -> 'a dblnode;;
value insert_before : 'a -> 'a dblnode -> unit;;
value insert_after : 'a -> 'a dblnode -> unit;;
value elim : 'a dblnode -> 'a dblnode;;
