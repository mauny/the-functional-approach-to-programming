
(* Turns a strict order into a preorder *)
(* +mk_order+ *)

let mk_order ord x y =
  if ord x y then Smaller else
  if x=y then Equiv
  else Greater;;
(* +mk_order+ *)
(* Turns a pair (lt,eq)  (strict order + equivalence into a preorder *)

let int_comp = mk_order (prefix <);;

(* +mk_preorder+ *)
let mk_preorder(lt,eq) x y =
  if lt x y then Smaller else
  if eq x y then Equiv
  else Greater;;
(* +mk_preorder+ *)

(* Various ad-hoc functions *)

let inv_rel c x y =
  match c x y with
    Smaller -> Greater
  | Greater -> Smaller
  | Equiv -> Equiv;;

(* To extend  an order with a min and a max *)
(* +extend_order+ *)

let extend_order ord x y = 
match (x,y) with
  ((Min,Min)|(Max,Max)) -> Equiv
| ((Min,_)|(_,Max)) -> Smaller
| ((Max,_)|(_,Min)) -> Greater
| (Plain x,Plain y) -> ord  x y;;
(* +extend_order+ *)

