#open "prelude";;
#open "orders";;

(* +all_file+ *)
(* +nat_base+ *)

let nat_base = 10000;;

(* +nat_base+ *)
(* +nat_of_int+ *)

let rec nat_of_int n =
  if n=0 then [] else
  if n < nat_base then [n] else
  (n mod nat_base) :: nat_of_int (n/nat_base);;

(* +nat_of_int+ *)
(* +null+ *)

let zero_nat n= n=[];;

(* +null+ *)
(* +add_sub_mult_carry+ *)

let add_carry c (m,n) =
  let s = m+n+c in (s/nat_base, s mod nat_base);;

let sub_carry c (m,n) =
  let s = m-(n+c) in
  if s>=0 then (0,s) else (1,nat_base+s);;

let mult_carry c (m,n) =
  let p = m*n + c in (p/nat_base, p mod nat_base);;

(* +add_sub_mult_carry+ *)
(* +add_sub_mult_digit_nat+ *)

let rec add_digit_nat d dl =
  if d = 0 then dl else
  match dl with
    [] -> [d]
  | (a::dl') -> let (c,n) = add_carry 0 (a,d)
                in n :: add_digit_nat c dl';;

let rec sub_digit_nat d dl =
  if d = 0 then dl else
  match dl with
    [] -> failwith "sub_digit_nat:neg result"
  | [a] -> let (c,n) = sub_carry 0 (a,d) in
           if c = 0 then if n = 0 then []
                         else [n]
           else failwith "sub_digit_nat:neg result"
  | (a::dl') -> let (c,n) = sub_carry 0 (a,d) in
                if c = 0 then n::dl'
                else n :: sub_digit_nat c dl';;

let mult_digit_nat d dl =
  let rec mult_rec c = fun
    [] -> if c = 0 then [] else [c]
  | (a::dl') -> let (c',n) = mult_carry c (a,d)
                in n :: mult_rec c' dl'
  in if d = 0 then [] else mult_rec 0 dl;;

(* +add_sub_mult_digit_nat+ *)
(* +add_sub_mult_nat+ *)

let add_nat dl1 dl2 =
  let rec add_rec c = fun
    ([],[]) -> if c = 0 then [] else [c]
  | ([],l) -> add_digit_nat c l
  | (l,[]) -> add_digit_nat c l
  | (a1::dl1,a2::dl2) -> let (c',n) = add_carry c (a1,a2)
                         in n :: add_rec c' (dl1,dl2)
  in add_rec 0 (dl1,dl2);;

let sub_nat dl1 dl2 =
  let rec sub_rec c = fun
    ([],[]) -> if c = 0 then []
               else failwith "sub_nat:neg result"
  | ([],l) -> failwith "sub_nat:neg result"
  | (l,[]) -> sub_digit_nat c l
  | (a1::dl1,a2::dl2) -> let (c',n) = sub_carry c (a1,a2) in
                         let r = sub_rec c' (dl1,dl2) in
                         if n = 0 & r = [] then [] else n::r
  in sub_rec 0 (dl1,dl2);;

let mult_nat dl1 dl2 =
  let rec mult_rec l' = fun
    [] -> []
  | [a] -> mult_digit_nat a l'
  | (a::l) -> add_nat (mult_digit_nat a l')
                (mult_rec (0::l') l) in
  if list_length dl1 > list_length dl2 then mult_rec dl1 dl2
  else mult_rec dl2 dl1;;

(* +add_sub_mult_nat+ *)
(* +comp_nat+ *)

let comp_nat dl1 dl2 =
  comp_rec Equiv dl1 dl2
  where rec comp_rec comp dl1 dl2 =
    if dl1 = [] then
      if dl2 = [] then comp else Smaller else
    if dl2 = [] then Greater else
    if hd dl1 < hd dl2 then comp_rec Smaller (tl dl1) (tl dl2) else
    if hd dl1 = hd dl2 then comp_rec comp (tl dl1) (tl dl2)
    else comp_rec Greater (tl dl1) (tl dl2);;

(* +comp_nat+ *)
(* +eq_lt_le_nat+ *)

let eq_nat l1 l2 = (comp_nat l1 l2 = Equiv);;

let lt_nat l1 l2 = (comp_nat l1 l2 = Smaller);;

let le_nat l1 l2 = match comp_nat l1 l2 with
  ((Smaller|Equiv)) -> true
| _ -> false;;

(* +eq_lt_le_nat+ *)
(* +icomp_nat+ *)

let rec icomp_nat l1 l2 =
  if l1 = [] then
    if l2 = [] then Equiv else Smaller else
  if l2 = [] then Greater
  else let rec icomp_rec comp (l1,l2) =
         let (n1,n2) = list_length l1,list_length l2 in
         if n1 < n2 then Smaller else
         if n1 = n2 then comp else Greater in
       if hd l1 < hd l2 then icomp_rec Smaller (tl l1,tl l2) else
       if hd l1 = hd l2 then icomp_nat (tl l1) (tl l2)
       else icomp_rec Greater (tl l1,tl l2)
;;

(* +icomp_nat+ *)
(* +ieq_lt_le_nat+ *)

let ieq_nat l1 l2 = (icomp_nat l1 l2 = Equiv);;

let ilt_nat l1 l2 = (icomp_nat l1 l2 = Smaller);;

let ile_nat l1 l2 = match icomp_nat l1 l2 with
  ((Smaller|Equiv)) -> true
| _ -> false;;

(* +ieq_lt_le_nat+ *)
(* 
   let rec lt_nat l1 l2 =
   let (n1,n2) = list_length l1, list_length l2
   in (n1 < n2) or
   (n2 = n1 &
   let rec lt_rec =
   fun ([],[]) -> false
   | (a1::l1,a2::l2)
   ->  (a1<a2) or (a1 = a2 & lt_rec (l1, l2))
   in lt_rec (l1,l2));;
   let rec le_nat l1 l2 =
   let (n1,n2) = list_length l1, list_length l2
   in (n1 < n2) or
   (n2 = n1 &
   let rec leq_rec =
   fun ([],[]) -> true
   | (a1::l1,a2::l2)
   ->  (a1<a2) or (a1 = a2 & leq_rec (l1, l2))
   in leq_rec (l1,l2));;
 *)
(* 
   let int_of_nat =
   fun [] -> 0
   |  [d] -> d
   | [d1;d2] ->  d1*nat_base+d2
   |     _  ->   failwith "int_of_nat: too big";;
 *)
(* +div_digit_nat+ *)

let idiv_digit_nat dl d =
  if d = 0 then failwith "div_digit_nat: division by 0" else
  match d with
    1 -> (dl,[])
  | _ -> div_rec dl
         where rec div_rec = fun
           [] -> [],[]
         | [a] -> let q = a/d
                  and r = a mod d in
                  ((if q=0 then [] else [q]), (if r=0 then [] else [r]))
         | (a1::((a2::dl) as dl'))
           -> if a1 < d
              then let m = a1*nat_base + a2 in
                   let (q,r) = (m/d, m mod d) in
                   let (q',r') = div_rec (r::dl) in
                   (q::q', r')
              else let (q,r) = a1/d,a1 mod d in
                   let (q',r') = div_rec (r::dl') in
                   (q::q', r');;

(* +div_digit_nat+ *)
(* vieille version *)
(* 
   let div_digit_nat l d =
   let rec div_rec =
   fun  [] ->  ([],[])
   |  [a] -> if a < d then ([],[a]) else ( [a/d], [a mod d])
   | (a1::a2::l)
   -> if a1 < d
   then let m = int_of_nat [a1;a2]
   in let q,r = m/d,m mod d
   in let (q',r') = div_rec (r::l)
   in (q::q',r')
   else let q,r = a1/d,a1 mod d
   in let (q',r') = div_rec  (r::a2::l)
   in (q::q',r')
   in match d
   with 1 -> (l,[])
   |  _ ->  let (q,r)  = div_rec l
   in  normalize q, normalize r;;
 *)
(* +cut_normalize+ *)

let cut n (dl:int list) =
  let rec cut_rec (n,dl1,dl2) =
    if n = 0 then (dl1,dl2) else cut_rec(n-1,(hd dl2)::dl1,tl dl2) in
  let (dl,dl') = cut_rec (n,[],dl) in
  (rev dl,dl');;

let rec normalize = fun
  [] -> []
| (0::dl) -> normalize dl
| dl -> dl;;

(* +cut_normalize+ *)
(* +inv_ops+ *)

let imult_digit_nat d l = rev(mult_digit_nat d (rev l));;

let imult_nat l1 l2 = rev(mult_nat (rev l1) (rev l2));;

let isub_nat l1 l2 = rev(sub_nat (rev l1) (rev l2));;

let iadd_nat l1 l2 = rev(add_nat (rev l1) (rev l2));;

let iadd_digit_nat d l = rev(add_digit_nat d (rev l));;

let isub_digit_nat d l = rev(sub_digit_nat d (rev l));;

(* +inv_ops+ *)
(* +first23+ *)

let first2 l =
  if list_length l < 2 then failwith "first2: list too short"
  else (hd l ,hd(tl l));;

let first3 l =
  if list_length l < 3 then failwith "first2: list too short"
  else (hd l, hd(tl l), hd(tl(tl l)));;

(* +first23+ *)
(* +find_first_digit+ *)

let find_first_digit u0 u1 u2 v1 v2 =
  let q0 =
    if u0 = v1 then nat_base-1 else (u0*nat_base+u1)/v1 in
  let rec find_q q =
    if v2*q>(u0*nat_base+u1-q*v1)*nat_base+u2
    then find_q (q-1) else q in
  find_q q0;;

(* +find_first_digit+ *)
(* +div_nat+ *)

let rec idiv_nat dl1 dl2 =
  if ilt_nat dl1 dl2 then ([], dl1) else
  let n2 = list_length dl2 in
  match n2 with
    0 -> failwith "Div_nat: divisor is 0"
  | 1 -> idiv_digit_nat dl1 (hd dl2)
  | _ -> let d = nat_base / (hd(dl2)+1) in
         let dl2 = imult_digit_nat d dl2 in
         let (v1,v2) = first2 dl2 in
         (let (q,r) = div_rec [] [] (imult_digit_nat d dl1)
          in (q, fst(idiv_digit_nat r d)))
         where rec div_rec quot lg ld =
           if ilt_nat lg dl2 then
             if ld = [] then (normalize quot,normalize lg) else
             let lg' = normalize(lg@[hd ld]) in
             if ilt_nat lg' dl2 then div_rec (quot@[0]) lg' (tl ld)
             else div_rec quot lg' (tl ld) else
           if list_length lg = n2 then
             let r1 = isub_nat lg dl2 in
             div_rec (quot@[1]) r1 ld
           else let (u0,u1,u2) = first3 lg in
                let q0 = find_first_digit u0 u1 u2 v1 v2 in
                let p = imult_digit_nat q0 dl2 in
                let (q,p) =
                  if ilt_nat lg p then q0-1,isub_nat p dl2
                  else (q0,p) in
                let r1 = isub_nat lg p in
                div_rec (quot@[q]) r1 ld;;

(* +div_nat+ *)
(* +compute_frac_part+ *)

let compute_frac_part dvd dvs n =
  cfp (normalize (dvd@[0])) n
  where rec cfp dvd =
    fun 0 -> []
      | n -> let (q,r) = idiv_nat dvd dvs in
             match r with
               [] -> q
             | _ -> match q with
                      [] -> 0::cfp (normalize(r@[0])) (n-1)
                    | [d] -> d::cfp (normalize(r@[0])) (n-1)
                    | _ -> failwith "compute_frac_part: wrong arg";;

(* +compute_frac_part+ *)
(* +pgcd+ *)

let rec pgcd dl1 dl2 =
  if dl1 = dl2 then dl1 else
  if ilt_nat dl1 dl2 then pgcd dl2 dl1
  else let (q,r) = idiv_nat dl1 dl2 in
       if zero_nat r then dl2 else pgcd dl2 r ;;

(* +pgcd+ *)
(* +type_nat+ *)

type nat = Left_nat of int list | Right_nat of int list;;

(* +type_nat+ *)
(* +nat_ops+ *)

let nat_of_int n = Left_nat (nat_of_int n);;

let zero_nat = fun ((Left_nat [])|(Right_nat [])) -> true | _ -> false;;

let eq_nat = fun
  (Left_nat m) (Left_nat n) -> m = n
| (Left_nat m) (Right_nat n) -> m = rev n
| (Right_nat m) (Left_nat n) -> m = rev n
| (Right_nat m) (Right_nat n) -> m = n;;

let lt_nat = fun
  (Left_nat m) (Left_nat n) -> lt_nat m n
| (Left_nat m) (Right_nat n) -> lt_nat m (rev n)
| (Right_nat m) (Left_nat n) -> ilt_nat m (rev n)
| (Right_nat m) (Right_nat n) -> ilt_nat m n;;

let le_nat = fun
  (Left_nat m) (Left_nat n) -> le_nat m n
| (Left_nat m) (Right_nat n) -> le_nat m (rev n)
| (Right_nat m) (Left_nat n) -> ile_nat m (rev n)
| (Right_nat m) (Right_nat n) -> ile_nat m n;;

let left_unary_intern_op f = fun
  (Left_nat n) -> Left_nat (f n)
| (Right_nat n) -> Left_nat (f (rev n));;

let right_unary_intern_op f = fun
  (Left_nat n) -> Right_nat (f (rev n))
| (Right_nat n) -> Right_nat (f n);;

let left_unary_extern_op f = fun
  (Left_nat n) -> (f n)
| (Right_nat n) -> (f (rev n));;

let right_unary_extern_op f = fun
  (Left_nat n) -> (f (rev n))
| (Right_nat n) -> (f n);;

let add_digit_nat d = left_unary_intern_op (add_digit_nat d);;

let sub_digit_nat d = left_unary_intern_op (sub_digit_nat d);;

let mult_digit_nat d = left_unary_intern_op (mult_digit_nat d);;

let div_digit_nat n d =
  match n with
    (Left_nat n) -> let q,r = idiv_digit_nat (rev n) d
                    in Right_nat q,Right_nat r
  | (Right_nat n) -> let q,r = idiv_digit_nat n d
                     in Right_nat q,Right_nat r;;

let left_binary_intern_op f = fun
  (Left_nat m) (Left_nat n) -> Left_nat(f m n)
| (Left_nat m) (Right_nat n) -> Left_nat(f m (rev n))
| (Right_nat m) (Left_nat n) -> Left_nat(f (rev m) n)
| (Right_nat m) (Right_nat n) -> Left_nat(f (rev m) (rev n));;

let left_binary_extern_op f = fun
  (Left_nat m) (Left_nat n) -> (f m n)
| (Left_nat m) (Right_nat n) -> (f m (rev n))
| (Right_nat m) (Left_nat n) -> (f (rev m) n)
| (Right_nat m) (Right_nat n) -> (f (rev m) (rev n));;

let add_nat = left_binary_intern_op add_nat;;

let sub_nat = left_binary_intern_op sub_nat;;

let mult_nat = left_binary_intern_op mult_nat;;

let comp_nat = left_binary_extern_op comp_nat;;

let right_binary_intern_op f = fun
  (Left_nat m) (Left_nat n) -> Right_nat (f (rev m) (rev n))
| (Left_nat m) (Right_nat n) -> Right_nat (f (rev m) n)
| (Right_nat m) (Left_nat n) -> Right_nat (f m (rev n))
| (Right_nat m) (Right_nat n) -> Right_nat (f m n);;

let right_binary_extern_op f = fun
  (Left_nat m) (Left_nat n) -> (f (rev m) (rev n))
| (Left_nat m) (Right_nat n) -> (f (rev m) n)
| (Right_nat m) (Left_nat n) -> (f m (rev n))
| (Right_nat m) (Right_nat n) -> (f m n);;

let div_nat m n =
  let (m',n') =
    match (m,n) with
      (Left_nat m,Left_nat n) -> (rev m,rev n)
    | (Left_nat m,Right_nat n) -> (rev m,n)
    | (Right_nat m,Left_nat n) -> ( m,rev n)
    | (Right_nat m,Right_nat n) -> ( m, n) in
  let q,r = idiv_nat m' n' in
  (Right_nat q, Right_nat r);;

let pgcd = right_binary_intern_op pgcd;;

(* 
   let exp_nat = right_binary_intern_op exp_nat;;
   let sqrt_nat =  right_unary_intern_op sqrt_nat;;
 *)

let compute_frac_part =
  right_binary_extern_op compute_frac_part;;

(* +mult_nat_list+ *)

let mult_nat_list = it_list mult_nat (nat_of_int 1);;

(* +mult_nat_list+ *)
(* +nat_ops+ *)
(* +gen_list+ *)

let rec gen_list x = fun
  0 -> []
| n -> x:: gen_list x (n-1);;

(* 
   let bit_length_nat = fun
   (Right_nat n) -> bit_length_nat n
   |  (Left_nat n) -> bit_length_nat (rev n);;

   let power2_nat k = Right_nat (power2_nat k);;
 *)
(* +gen_list+ *)
(* +guess_root+ *)

let guess_root n =
  let rec guess_rec n = fun
    [] -> (0,n)
  | [x] -> (1+int_of_float(sqrt(float_of_int x)),n)
  | [x1;x2] -> (1 + int_of_float(sqrt(float_of_int (x1 + nat_base*x2))), n)
  | (_::_::l) -> guess_rec (n+1) l in
  let (r,len) =
    guess_rec 0
      (match n with (Left_nat l) -> l
                  | (Right_nat l) -> (rev l)) in
  Right_nat (r:: gen_list 0 len);;

(* +guess_root+ *)

(* +sqrt_nat+ *)

let sqrt_nat n =
  let x = guess_root n in
  let y = add_digit_nat 1 x in
  sqrt_rec x y
  where rec sqrt_rec x y =
    if le_nat y x then y else
    sqrt_rec (fst (div_digit_nat (add_nat x (fst (div_nat n x))) 2)) x;;

(* +sqrt_nat+ *)
(* +exp_nat+ *)

let rec exp_nat a n =
  if n = 0 then nat_of_int 1 else
  let q = n/2
  and r = n mod 2 in
  let p = exp_nat a q in
  mult_nat (mult_nat p p) (if r = 0 then nat_of_int 1 else a) ;;

(* +exp_nat+ *)
(* 
   let rec power2_nat n =
   if n = 0 then [1] else
   let q = n/2
   and r = n mod 2 in
   let p = power2_nat q in
   imult_digit_nat (if r = 0 then 1 else 2) (imult_nat p p) ;;

   let rec bit_length_nat n =
   let (q,r) = div_digit_nat n 2 in
   if zero_nat q then 1 else
   1 + (bit_length_nat q);;


   let sqrt_nat n =
   let k = bit_length_nat n in
   let x =  power2_nat k in
   let y =  iadd_digit_nat 1 x in
   sqrt_rec x y
   where rec sqrt_rec x y =
   if le_nat y x then y else
   sqrt_rec (fst (div_digit_nat (iadd_nat x (fst (div_nat n x))) 2))  x;;
 *)
(* +nat_parser+ *)

let nat_of_digit = function
  `0`..`9` as c
  -> (int_of_char c)-(int_of_char `0`)
| _ -> failwith "Not a Digit";;

let rec nat_parser n = function
  [< ' `0`..`9` as c;
     (nat_parser (add_digit_nat (nat_of_digit c)
                    (mult_digit_nat 10 n) )) r>]
  -> r
| [<>] -> n;;

let parse_nat s =
  (function [<(nat_parser (Left_nat [])) n >] -> n) (stream_of_string s);;

let nat_of_string = parse_nat;;

(* +nat_parser+ *)
(* +nat_printer+ *)

let print_digit_nat n =
  let s = string_of_int n in
  match string_length s with
    1 -> print_string ("000" ^ s)
  | 2 -> print_string ("00" ^ s)
  | 3 -> print_string ("0" ^ s)
  | 4 -> print_string s
  | _ -> failwith "print_digit_nat: wrong digit";;

let rec print_nat l =
  let rec print_rec = fun
    [] -> ()
  | (a::l) -> print_digit_nat a ; print_rec l in
  match
    (match l with (Left_nat l) -> rev l | (Right_nat l) -> l)
  with [] -> print_int 0
     | [a] -> print_int a
     | (a::l) -> print_int a ; print_rec l;;

(* +nat_printer+ *)
(* +nat_new_printer+ *)
(*
new_printer "nat" print_nat;;
*)
(* +nat_new_printer+ *)


(* +all_file+ *)
