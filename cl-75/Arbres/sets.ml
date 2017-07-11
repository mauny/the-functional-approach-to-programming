#directory "../Util";;

#open "prelude";;
#open "orders";;

#infix "o";;

#open "binary_trees";;

(* +set+ *)

type 'a set =
     {set_elements:'a avltree;
      set_order:'a -> 'a -> comparison};;

(* +set+ *)
(* +cardinal+ *)

let cardinal s = btree_size s.set_elements;;

(* +cardinal+ *)
(* +make_set+ *)

let make_set c l =
  {set_elements=mk_avl (fun x y -> x) c l;
   set_order=c};;

(* +make_set+ *)
(* +set_isempty+ *)

let set_isempty s =
  (s.set_elements=Empty);;

(* +set_isempty+ *)
(* +set_member+ *)

let set_member x s =
  belongs_to_avl s.set_order x s.set_elements;;

(* +set_member+ *)
(* +set_it+ *)

let set_it f s =
  btree_it (fun x y -> f (fst x) y) s.set_elements;;

let it_set f x s =
  it_btree (fun x y -> f x (fst y)) x s.set_elements;;

let do_set f s = do_avl f s.set_elements;;

(* +set_it+ *)
(* +Set_exc+ *)

exception Set_exc of string;;

(* +Set_exc+ *)
(* +set_forall+ *)

let set_forall p s =
  try do_set (fun x->if not (p x) then raise (Set_exc "")) s; true
  with Set_exc _ -> false;;

let set_exists p s =
  try do_set (fun x->if (p x) then raise (Set_exc "")) s; false
  with Set_exc _ -> true;;

(* +set_forall+ *)
(* +sub_set+ *)

let sub_set s1 s2 =
  set_forall (fun e -> set_member e s2) s1;;

let set_equiv s1 s2 =
  sub_set s1 s2 & sub_set s2 s1;;

(* +sub_set+ *)
(* +set_random_element+ *)

let set_random_element s =
  fst (root s.set_elements);;

(* +set_random_element+ *)
(* +list_of_set+ *)

let list_of_set s =
  flat_avl s.set_elements;;

(* +list_of_set+ *)
(* +add_to_set+ *)

let add_to_set s x =
  {set_elements = add_to_avl (fun x y -> x) s.set_order s.set_elements x;
   set_order=s.set_order};;

let add_list_to_set s l=
  it_list add_to_set s l;;

(* +add_to_set+ *)
(* +remove_from_set+ *)

let remove_from_set s x =
  try {set_elements= remove_from_avl s.set_order s.set_elements x;
       set_order=s.set_order}
  with _ -> raise (Set_exc "remove_from_set");;

let remove_list_from_set s = it_list remove_from_set s;;

(* +remove_from_set+ *)
(* +subtract_from_set+ *)

let subtract_from_set s x =
  try remove_from_set s x
  with _ -> s;;

(* +subtract_from_set+ *)
(* +set_union+ *)

let set_union s1 s2 =
  if not(s1.set_order = s2.set_order)
  then raise (Set_exc "set_union: different set orders")
  else it_set add_to_set s1 s2;;

let set_diff s1 s2 =
  if not(s1.set_order = s2.set_order)
  then raise (Set_exc "set_diff: different set orders")
  else it_set subtract_from_set s1 s2;;

let set_intersection s1 s2 = set_diff s1 (set_diff s1 s2);;

(* +set_union+ *)
(* +do_set+ *)

let do_set f s =
  do_avl f s.set_elements;;

(* +do_set+ *)
(* +do_set+ *)

let map_set f s =
  map_avl f s.set_elements;;

(* +do_set+ *)
