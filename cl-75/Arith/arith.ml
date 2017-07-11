(* +type_big_int+ *)

type sign = Neg | Pos;;

type big_int = {big_sign:sign; big_val:nat};;

(* +type_big_int+ *)
(* +big_int_utilities+ *)

let prod_sign = fun
  (Neg,Neg) -> Pos
| (Neg,Pos) -> Neg
| (Pos,Neg) -> Neg
| (Pos,Pos) -> Pos;;

let big_int_of_int n =
  {big_sign= if n >= 0 then Pos else Neg;
   big_val= nat_of_int (abs n)};;

let big_int_of_nat n = {big_sign=Pos; big_val=n};;

let lt_big n1 n2 =
  match (n1.big_sign,n2.big_sign) with
    (Neg,Pos) -> true
  | (Pos,Neg) -> false
  | (Neg,Neg) -> lt_nat n2.big_val n1.big_val
  | (Pos,Pos) -> lt_nat n1.big_val n2.big_val;;

let le_big n1 n2 =
  match (n1.big_sign,n2.big_sign) with
    (Neg,Pos) -> true
  | (Pos,Neg) -> false
  | (Neg,Neg) -> le_nat n2.big_val n1.big_val
  | (Pos,Pos) -> le_nat n1.big_val n2.big_val;;

let zero_big n = n.big_val=[];;

let pos_big n= n.big_sign=Pos;;

let neg_big n= n.big_sign=Neg;;

let add_big n1 n2 =
  let (l1,l2) = n1.big_val,n2.big_val in
  match (n1.big_sign, n2.big_sign) with
    (Neg,Pos) -> if l1=l2
                 then {big_sign=Pos;big_val=[]}
                 else if ilt_nat l1 l2
                      then {big_sign=Pos;big_val=sub_nat l2 l1}
                      else {big_sign=Neg;big_val=sub_nat l1 l2}
  | (Pos,Neg) -> if l1=l2
                 then {big_sign=Pos;big_val=[]}
                 else if ilt_nat l1 l2
                      then {big_sign=Neg;big_val=sub_nat l2 l1}
                      else {big_sign=Pos;big_val=sub_nat l1 l2}
  | (Neg,Neg) -> {big_sign=Neg;big_val=add_nat l1 l2}
  | (Pos,Pos) -> {big_sign=Pos;big_val=add_nat l1 l2};;

let sub_big n1 n2 =
  let (l1,l2) = n1.big_val,n2.big_val in
  match (n1.big_sign, n2.big_sign) with
    (Neg,Pos) -> {big_sign=Neg;big_val=add_nat l1 l2}
  | (Pos,Neg) -> {big_sign=Pos;big_val=add_nat l1 l2}
  | (Neg,Neg) -> if l1=l2
                 then {big_sign=Pos;big_val=[]}
                 else if lt_nat l1 l2
                      then {big_sign=Pos;big_val=sub_nat l2 l1}
                      else {big_sign=Neg;big_val=sub_nat l1 l2}
  | (Pos,Pos) -> if l1=l2
                 then {big_sign=Pos;big_val=[]}
                 else if lt_nat l1 l2
                      then {big_sign=Neg;big_val=sub_nat l2 l1}
                      else {big_sign=Pos;big_val=sub_nat l1 l2};;

let mult_big n1 n2 =
  {big_sign=prod_sign (n1.big_sign,n2.big_sign);
   big_val= mult_nat n1.big_val n2.big_val};;

let div_big n1 n2 =
  let dvd = rev n1.big_val and dvs= rev n2.big_val in
  let quot,rmd = div_nat dvd dvs in
  match n1.big_sign, n2.big_sign with
    Pos,Pos -> {big_sign=Pos; big_val= rev quot},
               {big_sign=Pos; big_val= rev rmd}
  | Pos,Neg -> {big_sign=Neg; big_val= rev quot},
               {big_sign=Pos; big_val= rev rmd}
  | Neg,Pos -> {big_sign=Neg; big_val= rev quot},
               {big_sign=Neg; big_val= rev rmd}
  | Neg,Neg -> {big_sign=Pos; big_val= rev quot},
               {big_sign=Neg; big_val= rev rmd};;

let exp_big = fun
  {big_sign=s1; big_val=n1} {big_sign=s2; big_val=n2}
  -> if s1=Pos & s2=Pos
     then {big_sign=Pos; big_val= rev(exp_nat (rev n1) (rev n2))}
     else failwith "exp_big: args must be positive";;

let sqrt_big = fun
  {big_sign=s; big_val=n}
  -> if s=Pos
     then {big_sign=Pos; big_val=rev(sqrt_nat(rev n))}
     else failwith "sqrt_big: arg must be positive";;

(* +big_int_utilities+ *)
(* +big_int_parser+ *)

let int_of_digit = function
  `0`..`9` as c -> (int_of_char c)-(int_of_char `0`)
| _ -> failwith "Not a Digit";;

let rec nat_parser n = function
  [< ' `0`..`9` as c;
     (nat_parser (add_digit_nat (int_of_digit c)
                    (mult_digit_nat 10 n) )) r>]
  -> r
| [<>] -> n;;

let parse_nat s =
  (function [<(nat_parser []) n >] -> n) (stream_of_string s);;

let nat_of_string = parse_nat;;

let rec big_parser = function
  [<'`-`; (nat_parser []) n >] -> {big_sign=Neg ;big_val=n}
| [<(nat_parser []) n >] -> {big_sign=Pos ;big_val=n};;

let parse_big_int s =
  big_parser (stream_of_string s);;

let big_int_of_string = parse_big_int;;

(* +big_int_parser+ *)
(* +big_int_printer+ *)

let print_digit_nat n =
  let s= string_of_int n in
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
  match rev l with
    [] -> print_int 0
  | [a] -> print_int a
  | (a::l) -> print_int a ; print_rec l;;

let print_big_int {big_sign=s; big_val=l}=
  print_newline ();
  (match s with Neg -> print_string "-" | Pos -> ()) ; print_nat l;;

(* +big_int_printer+ *)

new_printer "big_int" print_big_int;;

let n= parse_big_int "999999999999999"
in mult_big n n;;

let rec fact n =
  if n = 0 then big_int_of_int 1
  else mult_big (big_int_of_int n) (fact (n-1));;

fact 50;;

(* +type_rat+ *)

type rat = {rat_sign:sign; rat_num:nat ; rat_den:nat};;

(* +type_rat+ *)
(* +rat_utilities+ *)

let rat_of_int n = {rat_sign=Pos ;
                    rat_num=nat_of_int n;
                    rat_den= [1]};;

let rat_of_nat n = {rat_sign=Pos ;rat_num=n; rat_den=[1]};;

let rat_of_big_int {big_sign=s; big_val=n} =
  {rat_sign=s ;rat_num=n; rat_den=[1]};;

let null_rat {rat_sign=s ;rat_num=n; rat_den=d} =
  n=[];;

let eq_rat = fun
  ({rat_sign=s1 ;rat_num=n1; rat_den=d1} as r1)
  ({rat_sign=s2 ;rat_num=n2; rat_den=d2} as r2)
  -> s1=s2 & mult_nat n1 d2 = mult_nat n2 d1 ;;

let lt_rat = fun
  ({rat_sign=s1 ;rat_num=n1; rat_den=d1} as r1)
  ({rat_sign=s2 ;rat_num=n2; rat_den=d2} as r2)
  -> match (s1,s2) with
       Neg,Pos -> true
     | Pos,Neg -> false
     | Pos,Pos -> lt_nat (mult_nat n1 d2) (mult_nat n2 d1)
     | Neg,Neg -> lt_nat (mult_nat n2 d1) (mult_nat n1 d2) ;;

let rec add_rat = fun
  ({rat_sign=s1 ;rat_num=n1; rat_den=d1} as r1)
  ({rat_sign=s2 ;rat_num=n2; rat_den=d2} as r2)
  -> if s1=s2 then {rat_sign=s1 ;
                    rat_num= add_nat (mult_nat n1 d2) (mult_nat n2 d1);
                    rat_den= mult_nat d1 d2}
     else
       if s1=Pos then
         let x= mult_nat n1 d2
         and y= mult_nat n2 d1 in
         if lt_nat x y then
           {rat_sign=Neg ;
            rat_num= sub_nat y x;
            rat_den= mult_nat d1 d2}
         else
           {rat_sign=Pos;
            rat_num= sub_nat x y;
            rat_den= mult_nat d1 d2}
       else
         add_rat r2 r1;;

let sub_rat = fun
  r1 {rat_sign=s2 ;rat_num=n2; rat_den=d2}
  -> let s = if s2=Pos then Neg else Pos in
     add_rat r1 {rat_sign=s ;rat_num=n2; rat_den=d2};;

let mult_rat = fun
  ({rat_sign=s1 ;rat_num=n1; rat_den=d1} as r1)
  ({rat_sign=s2 ;rat_num=n2; rat_den=d2} as r2)
  -> {rat_sign=prod_sign (s1,s2) ;
      rat_num=mult_nat n1 n2;
      rat_den=mult_nat d1 d2};;

let div_rat = fun
  r1 {rat_sign=s2 ;rat_num=n2; rat_den=d2} ->
    mult_rat r1 {rat_sign=s2 ;rat_num=d2; rat_den=n2};;

(* +rat_utilities+ *)
(* +pgcd+ *)

let rec pgcd l1 l2 =
  if l1=l2 then l1 else
  if lt_nat l1 l2 then pgcd l2 l1
  else
    let (q,r) = div_nat l1 l2 in
    if r=[] then l2 else pgcd l2 r ;;

(* +pgcd+ *)
(* +reduce+ *)

let reduce {rat_sign=s; rat_num=num; rat_den=den}=
  let rnum= rev num and rden= rev den in
  let p= pgcd rnum rden in
  {rat_sign=s; rat_num=rev(fst (div_nat rnum p));
   rat_den=rev(fst(div_nat rden p))};;

(* +reduce+ *)
(* Tests
   reduce
   {rat_sign=Pos; rat_num=(fact 40).big_val; rat_den=(fact 50).big_val};;
   reduce
   {rat_sign=Pos; rat_num=(fact 38).big_val; rat_den=(fact 40).big_val};;
   reduce
   {rat_sign=Pos; rat_num=(fact 40).big_val; rat_den=(fact 50).big_val};;


   let add_rrat r1 r2= reduce (add_rat r1 r2);;
   let sub_rrat r1 r2= reduce (sub_rat r1 r2);;
   let mult_rrat r1 r2= reduce (mult_rat r1 r2);;
   let div_rrat r1 r2= reduce (div_rat r1 r2);;
 *)
(* +rat_parser+ *)

let rec lexer = function
  [<'`-`; (nat []) n; rest_lexer d >]
    -> {rat_sign=Neg ;rat_num= n; rat_den= d}
| [<(nat []) n; rest_lexer d >]
    -> {rat_sign=Pos ;rat_num= n; rat_den= d}
and rest_lexer =
  function [<'`/`; (nat []) d >] -> d
         | [< >] -> [1] ;;

let parse_rat s = reduce(lexer (stream_of_string s));;

let rat_of_string = parse_rat;;

(* +rat_parser+ *)
(* +rat_printer+ *)

let max_int m n = if m>n then m else n;;

let print_rat = fun
  {rat_sign=s; rat_num=num; rat_den=den}
  -> (match s with Neg -> print_string "-" | Pos -> ()) ;
     print_nat num;
     print_string "/";
     print_nat den;;

new_printer "rat" print_rat;;

(* +rat_printer+ *)
(* Tests
   parse_rat "4446464/32";;
   parse_rat "4446464/3257";;
   parse_rat "4446464/3256";;
 *)
(* +rat_printer+ *)

let default_frac_length= ref 3;;

let set_frac_length n = default_frac_length:= n;;

let print_frac_rat = fun
  {rat_sign=s; rat_num=num; rat_den=den}
  -> (match s with Neg -> print_string "-" | Pos -> ()) ;
     let q,r= div_nat (rev num) (rev den) in
     let frac_part= compute_frac_part r (rev den) !default_frac_length in
     begin print_nat (rev q); print_string "."; print_nat (rev frac_part) end;;

new_printer "rat" print_frac_rat;;

(* +rat_printer+ *)
(* 
   CONCOURS CACHOUS LAJAUNIE
   let day= parse_rat "86400";;
   let year=  mult_rat day (parse_rat "365");;
   let years= mult_rat (parse_rat "114") year;;
   let yearb= mult_rat  day (parse_rat "366");;
   let yearsb= mult_rat (parse_rat "28") yearb;;
   let year94= mult_rat day (parse_rat "243");;
   let total= add_rat years (add_rat yearsb year94);;
   let res= div_rat (parse_rat "25000000000") total;;
 *)
(* +nombre_e+ *)

let rat0= {rat_sign=Pos; rat_num=[]; rat_den=[1]};;

let rec sigma f (a,b)=
  if a>b then rat0
  else add_rat (f a) (sigma f (a+1,b));;

let inv_fact n =
  {rat_sign=Pos; rat_num=[1]; rat_den=(fact n).big_val};;

let e n = sigma inv_fact (0,n);;

(* +nombre_e+ *)
(* Calcul de E
   a) sans effectuer les reductions a chaque operation

   let rat0= {rat_sign=Pos; rat_num=[]; rat_den=[1]};;
   let rec sigma f (a,b)=
   if a>b then rat0
   else add_rat (f a) (sigma f (a+1,b));;

   let inv_fact n = {rat_sign=Pos; rat_num=[1];
   rat_den=(fact n).big_val};;

   let E n = sigma inv_fact (0,n);;


   b) en effectuant les reductions

   let rec sigma f (a,b)=
   if a>b then rat0
   else add_rrat (f a) (sigma f (a+1,b));;
   let E n = reduce (sigma (fun n -> {rat_sign=Pos; rat_num=[1];
   rat_den=(fact n).big_val})
   (0,n));;

   C'est en fait plus long: 43s au lieu de 39s pour E 20 sur PwB
 *)

let sqrt640320 digits =
  let pow = rev (exp_nat [10] [digits]) in
  (rev(sqrt_nat (rev (mult_nat (mult_nat [320;64] pow) pow))), pow);;

let size_nat = list_length;;

let test (x,y,z,t) = size_nat x + size_nat y + z > size_nat t;;

let mult_nat_list = it_list mult_nat [1];;

let approx_pi digits =
  let prod = [12]
  and sum = [1409;1359]
  and D= [320;64]
  and N= mult_nat [1409;1359] [12]
  and sn = []
  and binom= [1]
  and pown3 = []
  (* and sqrt,pow = sqrt640320 (digits-2) *)
  and pow3= rev(exp_nat (rev [320;64]) [3]) in
  let sizeB= 1+ size_nat pow in
  approx_rec prod sum pown3 D N sn binom true
  where rec approx_rec prod sum pown3 D N sn binom pos=
    if test(prod,sum,sizeB,D)
    then let prod = mult_nat_list [[8];
                                   add_digit_nat 1 sn;
                                   add_digit_nat 3 sn;
                                   add_digit_nat 5 sn;
                                   prod] in
         let sum = add_nat [134;4514;5] sum in
         let pown3 = add_nat binom pown3 in
         let D = mult_nat_list [pown3;pow3;D] in
         let N = (if pos then sub_nat else add_nat)
                   (mult_nat_list [pown3;pow3;N])
                   (mult_nat_list [prod; sum]) in
         let sn = add_digit_nat 6 sn in
         let binom = add_nat sn binom in
         approx_rec prod sum pown3 D N sn binom (not pos)
    else div_rat (rat_of_nat (mult_nat sqrt D))
           (rat_of_nat (mult_nat N pow));;

let sqrt640320' digits=
  let pow = exp_big (big_int_of_nat [10]) (big_int_of_nat [digits]) in
  (sqrt_big (mult_big (mult_big (big_int_of_string "640320") pow) pow), pow);;

let size_big n = list_length n.big_val;;

let test' (x,y,z,t) = size_big x + size_big y + z > size_big t;;

let mult_big_list = it_list mult_big (big_int_of_nat [1]);;

let approx_pi' digits =
  let prod = big_int_of_string "12"
  and sum = big_int_of_string "13591409"
  and D = big_int_of_string "640320"
  and N = mult_big (big_int_of_string "13591409")
            (big_int_of_string "12")
  and sn = big_int_of_string "0"
  and binom = big_int_of_string "1"
  and pown3 = big_int_of_string "0"
  and (sqrt,pow) = sqrt640320' (digits-2)
  and pow3 = exp_big (big_int_of_string "640320") (big_int_of_string "3") in
  let sizeB = 1+ size_big pow in
  approx_rec prod sum pown3 D N sn binom
  where rec approx_rec prod sum pown3 D N sn binom =
    if test'(prod,sum,sizeB,D)
    then let prod= mult_big_list [big_int_of_string "-8";
                                  add_big (big_int_of_string "1") sn;
                                  add_big (big_int_of_string "3") sn;
                                  add_big (big_int_of_string "5") sn;
                                  prod] in
         let sum = add_big (big_int_of_string "545140134") sum in
         let pown3 = add_big binom pown3 in
         let D = mult_big_list [pown3;pow3;D] in
         let N = add_big (mult_big_list [pown3;pow3;N])
                   (mult_big_list [prod; sum]) in
         let sn = add_big (big_int_of_string "6") sn in
         let binom = add_big sn binom in
         approx_rec prod sum pown3 D N sn binom
    else div_rat (rat_of_big_int (mult_big sqrt D))
           (rat_of_big_int (mult_big N pow));;
