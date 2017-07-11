load "../Util/prelude";;
#open "prelude";;
#infix "o";;

(* +type_complex+ *)
type complex = {re_part:float; im_part:float};;
(* +type_complex+ *)

(* +record_types_examples+ *)
type planar_point = {xcoord:float; ycoord:float};;
type circle = {center:planar_point ; radius:float};;
type triangle = {ptA:planar_point; 
                 ptB:planar_point; 
                 ptC:planar_point};;
(* +record_types_examples+ *)

(* +complex_abstract_type+ *)
let cx0= {re_part=0.; im_part=0.};;
let cx1= {re_part=1.; im_part=0.};;
let cx_i= {re_part=0.; im_part=1.};;
let add_complex {re_part=r1; im_part=i1} {re_part=r2; im_part=i2}=
  {re_part=r1+.r2; im_part=i1+.i2};;
let mult_complex {re_part=r1; im_part=i1} {re_part=r2; im_part=i2}=
  {re_part=r1*.r2-.i1*.i2; im_part=i1*.r2+.i2*.r1};;
(* +complex_abstract_type+ *)



(* +type_couleur+ *)
type couleur = Trefle | Carreau | Coeur | Pique;;
(* +type_couleur+ *)

(* +type_num+ *)
type num = Int of int | Float of float;;
(* +type_num+ *)

(* +add_num+ *)
let add_num =  fun
       (Int m, Int n)       -> Int(m +  n)
     | (Int m, Float n)     -> Float((float_of_int m) +. n)
     | (Float m, Int n)     -> Float(m +. (float_of_int n))
     | (Float m, Float n)   -> Float(m +. n);;
(* +add_num+ *)

(* +type_angle+ *)
type angle = Angle of float;;
(* +type_angle+ *)

load_object "inttree";;
#open "inttree";;

load_object "exp";;
#open "exp";;

(* +eval+ *)
let rec eval env expression = 
   match expression with
       (Constant n)            -> n
     | (Variable x)            -> env x
     | (Addition(e1,e2))       -> eval env e1 + eval env e2
     | (Multiplication(e1,e2)) -> eval env e1 * eval env e2;;
(* +eval+ *)

(* +deriv+ *)
let rec deriv var expression = 
  match expression with
       (Constant n)            -> Constant 0
     | (Variable x)            -> if x=var then Constant 1
                                           else Constant 0
     | (Addition(e1,e2))       -> Addition(deriv var e1, deriv var e2)
     | (Multiplication(e1,e2)) -> Addition(Multiplication(e1, deriv var e2),
                                           Multiplication(deriv var e1, e2));;
(* +deriv+ *)

load_object "poly_tree";;
#open "poly_tree";;


(* +total+ *)
let rec total = fun  (Leaf n) -> n
                  | (Node(t1,t2)) -> total(t1)+total(t2);;
(* +total+ *)




(* +length1+ *)
let rec length =   fun
        []  -> 0
     | (a::l) -> 1 + length l;;
(* +length1+ *)


(* +append1+ *)
let rec append l1 l2 =
match l1 with
   []    -> l2
| (a::l) -> a::append l l2;;
(* +append1+ *)

(* +rev1+ *)
let rec rev =  fun
       []   ->  []
     | (a::l) ->  append (rev l) [a];;
(* +rev1+ *)

(* +sigma1+ *)
let rec sigma = fun
       []    ->   0
     | (a::l) ->  a + sigma l;;
let rec pi = fun 
        []    ->   1
     | (a::l) ->  a * pi l;;
(* +sigma1+ *)


(* +map1+ *)
let rec map f l = 
   match l with
         []  ->  []
     | (a::l) -> f(a)::map f l;;
(* +map1+ *)

(* +flat1+ *)
let rec flat =  fun
           []  ->  []
     | (l::ll) -> append l (flat ll);;
(* +flat1+ *)

(* +list_hom+ *)
let rec list_hom e f l = 
  match l with
      []  -> e
   |  (a::l)  -> f a (list_hom e f l);;
(* +list_hom+ *)


(* +cons+ *)
let cons x y= x::y;;
(* +cons+ *)


(* +defs_with_list_hom+ *)
let length = list_hom 0 (fun _ n -> n+1);;
let append l1 l2 = list_hom l2 cons l1;;
let rev = list_hom [] (fun a l -> append l [a]) ;;
let sigma = list_hom 0 add_int;;
let pi = list_hom 1 mult_int;;
let map f l = list_hom [] (fun x l -> f(x)::l) l;;
let flat  = list_hom [] append;;
(* +defs_with_list_hom+ *)


(* +partition+ *)
let partition test l =
   let switch elem (l1,l2)   = 
        if test elem then (l1,elem::l2) else (elem::l1,l2)
   in list_it switch l ([],[]);;
(* +partition+ *)

(* +filter+ *)
let filter test = snd o (partition test);;
(* +filter+ *)


(* +quicksort+ *)
let rec quicksort order  liste = 
  match liste with
      []     -> []
  |   [a] -> [a]
  |  (a::l) ->  let l1,l2 = partition (order a) l
		in (quicksort order l1)@(a::(quicksort order l2));;
(* +quicksort+ *)

(* +insert+ *)
let rec insert order elem liste = 
  match liste with
           []    ->    [elem]
     | (a::l)   ->   if order elem a then elem::a::l
                            else a :: insert order elem l;;
(* +insert+ *)

(* +member1+ *)
let rec member equiv e list = 
   match list with
        []   ->    false
     | (a::l) ->  equiv(a,e) or member equiv e l;;
(* +member1+ *)


(* +make_set1+ *)
let rec rem_from_list equiv e list =
    match list with
          []    ->     []
     | (a::l) ->  let l' = rem_from_list equiv e l in
                  if equiv(a,e) then l'
                                   else a::l';;
let rec make_set equiv liste = 
   match liste with
          []    ->     []
     |  (a::l)  ->    a:: make_set equiv (rem_from_list equiv a l);;
(* +make_set1+ *)


(* +rem_from__set1+ *)
let rec rem_from_set equiv e list = 
   match list with
        []    ->     []
     | (a::l) ->  if equiv(a,e) then l
                                else a::rem_from_set equiv e l;;
(* +rem_from__set1+ *)


(* +add_to_set1+ *)
let add_to_set equiv e l =
 if member equiv e l then l else e::l;;
(* +add_to_set1+ *)


(* +union_inter1+ *)
let rec union equiv (l1,l2) = list_it (add_to_set equiv) l1 l2;;
let inter equiv (l1,l2)= filter (fun x -> member equiv x l2) l1;;
(* +union_inter1+ *)


(* +member2+ *)
let rec member (order,equiv) e list = 
  match list with
        []   ->    false
     | (a::l) ->  if order(a,e)  then member (order,equiv) e l
                  else equiv(a,e);;
(* +member2+ *)


(* +add_to_set2+ *)
let rec add_to_set (order,equiv) elem list =  
    match list with
           []    ->    [elem]
     | (a::l)   ->   if order(elem,a) then elem::a::l else 
                      if equiv(elem,a) then a::l
                      else a::add_to_set (order,equiv) elem l;;
(* +add_to_set2+ *)


(* +union_inter2+ *)
let rec inter (order,equiv) = fun
       ([],_)   ->     []
     | (_,[])   ->     []
     | ((a1::l1 as ll1),(a2::l2 as ll2))
                ->  if equiv(a1,a2) then a1::inter (order,equiv) (l1,l2)  else
                    if order(a1,a2) then inter (order,equiv) (l1,ll2)
                    else inter (order,equiv) (ll1,l2);;
let rec union (order,equiv) = fun
       ([],l2)   ->     l2
     | (l1,[])   ->     l1
     | ((a1::l1 as ll1),(a2::l2 as ll2))
                ->  if equiv(a1,a2) then a1::union (order,equiv) (l1,l2)  else
                    if order(a1,a2) then a1::union(order,equiv) (l1,ll2)
                    else a2::union(order,equiv) (ll1,l2);;
(* +union_inter2+ *)


(* +find+ *)
let rec find prop list =  
  match list with
      []     -> None
   | (a::l)  -> if prop a then Some a else find prop l;;
(* +find+ *)


(* +associate+ *)
let associate v l = 
  match find (fun (x,y) -> x=v) l with
      None  ->   None
   |  Some (x,y) -> Some y;;
(* +associate+ *)



