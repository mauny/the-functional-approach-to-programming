#open "binary_trees";;
#open "orders";;

(*+dictionary+*)
type ('a,'b) dictionary =
 { dict_rel : 'b -> 'b -> comparison;
   dict_data: ('a * 'b) avltree};;
(*+dictionary+*)


(*+right+*)
(*
let right f x y = 
        f (snd x) (snd y);;
let right1 f x y =
        f x (snd y);;
*)
(*+right+*)


(*+dict_assoc+*)
let dict_assoc e d =
 fst(fst(find_avl (fun x y -> d.dict_rel x (snd y)) e d.dict_data));;
(*+dict_assoc+*)


(*+dict_add_or_replace+*)
let dict_add_or_replace {dict_rel=c;dict_data=t} e =
    {dict_rel=c; 
     dict_data=add_to_avl (fun x y -> y) (fun x y -> c (snd x) (snd y)) t e};;
(*+dict_add_or_replace+*)


(*+dict_remove+*)
let dict_remove {dict_rel=c;dict_data=t} key =
    {dict_rel=c; 
     dict_data=remove_from_avl (fun x y -> c x (snd y)) t key };;
(*+dict_remove+*)


(*+dict_merge+*)
let dict_merge opt d1 d2 =
  if not(d1.dict_rel==d2.dict_rel) then
        failwith "dict_merge: dictionnaries have different orders" else
      {dict_rel=d1.dict_rel; 
       dict_data= merge_avl opt (fun x y -> d1.dict_rel (snd x) (snd y)) 
                        d1.dict_data d2.dict_data};;
(*+dict_merge+*)

