#open "arith_list_nat";;

#open "arith_big_int";;

(* +type_rat+ *)

type rat = {rat_sign: sign; rat_num: nat ; rat_den: nat};;

(* +type_rat+ *)
(* +rat_utilities+ *)

let rat_of_int n =
  {rat_sign=Pos;
   rat_num=nat_of_int n;
   rat_den= nat_of_int 1};;

let rat_of_nat n =
  {rat_sign=Pos;
   rat_num=n;
   rat_den=nat_of_int 1};;

let rat_of_big_int {big_sign=s; big_val=n}=
  {rat_sign=s; rat_num=n; rat_den=nat_of_int 1};;

let null_rat {rat_sign=s ;rat_num=n; rat_den=d}= zero_nat n;;

let eq_rat = fun
  ({rat_sign=s1 ;rat_num=n1; rat_den=d1} as r1)
  ({rat_sign=s2 ;rat_num=n2; rat_den=d2} as r2)
  -> s1=s2 & eq_nat (mult_nat n1 d2) (mult_nat n2 d1) ;;

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
                    rat_den= mult_nat d1 d2} else
     if s1=Pos then
       let x = mult_nat n1 d2
       and y = mult_nat n2 d1 in
       if lt_nat x y
       then {rat_sign=Neg ;
             rat_num= sub_nat y x;
             rat_den= mult_nat d1 d2}
       else
         {rat_sign=Pos;
          rat_num = sub_nat x y;
          rat_den = mult_nat d1 d2}
     else add_rat r2 r1;;

let sub_rat = fun
  r1 {rat_sign=s2; rat_num=n2; rat_den=d2}
  -> let s = if s2=Pos then Neg else Pos in
     add_rat r1 {rat_sign=s; rat_num=n2; rat_den=d2};;

let mult_rat = fun
  ({rat_sign=s1; rat_num=n1; rat_den=d1} as r1)
  ({rat_sign=s2; rat_num=n2; rat_den=d2} as r2)
  -> {rat_sign = prod_sign (s1,s2) ;
      rat_num = mult_nat n1 n2;
      rat_den = mult_nat d1 d2};;

let div_rat = fun
  r1 {rat_sign=s2; rat_num=n2; rat_den=d2}
  -> mult_rat r1 {rat_sign=s2; rat_num=d2; rat_den=n2};;

(* +rat_utilities+ *)
(* +mult_div_rat+ *)

let mult_rat = fun
  {rat_sign=s1; rat_num=n1; rat_den=d1}
  {rat_sign=s2; rat_num=n2; rat_den=d2}
  -> {rat_sign=prod_sign (s1,s2) ;
      rat_num=mult_nat n1 n2;
      rat_den=mult_nat d1 d2};;

let div_rat = fun
  r1 {rat_sign=s2 ;rat_num=n2; rat_den=d2}
  -> mult_rat r1 {rat_sign=s2 ;rat_num=d2; rat_den=n2};;

(* +mult_div_rat+ *)
(* +reduce+ *)

let reduce {rat_sign=s; rat_num=num; rat_den=den}=
  let p = pgcd num den in
  {rat_sign=s; rat_num = fst (div_nat num p);
   rat_den= fst (div_nat den p)};;

(* +reduce+ *)
(* Tests
   reduce
   {rat_sign=Pos; rat_num=(fact 40).big_val; rat_den=(fact 50).big_val};;
   reduce
   {rat_sign=Pos; rat_num=(fact 38).big_val; rat_den=(fact 40).big_val};;
   reduce
   {rat_sign=Pos; rat_num=(fact 40).big_val; rat_den=(fact 50).big_val};;


   let add_rrat r1 r2 = reduce (add_rat r1 r2);;
   let sub_rrat r1 r2 = reduce (sub_rat r1 r2);;
   let mult_rrat r1 r2 = reduce (mult_rat r1 r2);;
   let div_rrat r1 r2 = reduce (div_rat r1 r2);;
 *)
(* +rat_parser+ *)

let rec rat_parser = function
  [<'`-`; (nat_parser (nat_of_int 0)) n; rest_rat_parser d >]
  -> {rat_sign=Neg ;rat_num= n; rat_den= d}
| [<(nat_parser (nat_of_int 0)) n; rest_rat_parser d >]
  -> {rat_sign=Pos ;rat_num= n; rat_den= d}
and rest_rat_parser = function
  [<'`/`; (nat_parser (nat_of_int 0)) d >] -> d
| [< >] -> nat_of_int 1 ;;

let parse_rat s = reduce(rat_parser (stream_of_string s));;

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
(* +rat_printer+ *)
(* Tests
   rat_of_string "4446464/32";;
   rat_of_string "4446464/3257";;
   rat_of_string "4446464/3256";;
 *)
(* +rat_frac_printer+ *)

let default_frac_length= ref 3;;

let set_frac_length n = default_frac_length:= n;;

let print_frac_rat = fun
  {rat_sign=s; rat_num=num; rat_den=den}
  -> (match s with Neg -> print_string "-" | Pos -> ());
     let q,r= div_nat num den in
     let frac_part= compute_frac_part r den !default_frac_length in
     begin
       print_nat q;
       print_string ".";
       print_nat (Right_nat frac_part)
     end;;
(*
new_printer "rat" print_frac_rat;;
*)
(* +rat_frac_printer+ *)
(* 
   CONCOURS CACHOUS LAJAUNIE
   let day= rat_of_string "86400";;
   let year=  mult_rat day (rat_of_string "365");;
   let years= mult_rat (rat_of_string "114") year;;
   let yearb= mult_rat  day (rat_of_string "366");;
   let yearsb= mult_rat (rat_of_string "28") yearb;;
   let year94= mult_rat day (rat_of_string "243");;
   let total= add_rat years (add_rat yearsb year94);;
   let res= div_rat (rat_of_string "25000000000") total;;
 *)
(* +nombre_e+ *)

let rat0= {rat_sign=Pos; rat_num=nat_of_int 0; rat_den= nat_of_int 1};;

let rec sigma f (a,b)=
  if a>b then rat0
  else add_rat (f a) (sigma f (a+1,b));;

let inv_fact n = {rat_sign=Pos; rat_num=nat_of_int 1;
                  rat_den=(fact n).big_val};;

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


