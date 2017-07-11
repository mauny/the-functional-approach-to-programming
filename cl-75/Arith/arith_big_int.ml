#open "arith_list_nat";;

(* +type_big_int+ *)

type sign = Neg | Pos;;

type big_int = {big_sign: sign; big_val: nat};;

(* +type_big_int+ *)
(* +big_int_of_nat+ *)

let big_int_of_nat n = {big_sign=Pos; big_val=n};;

(* +big_int_of_nat+ *)
(* +big_int_utilities+ *)

let prod_sign= fun
  (Neg,Neg) -> Pos
| (Neg,Pos) -> Neg
| (Pos,Neg) -> Neg
| (Pos,Pos) -> Pos;;

let big_int_of_int n =
  {big_sign = if n >= 0 then Pos else Neg;
   big_val = nat_of_int (abs n)};;

let big_int_of_nat n =
  {big_sign=Pos; big_val=n};;

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

let zero_big n = zero_nat n.big_val;;

let pos_big n = n.big_sign=Pos;;

let neg_big n = n.big_sign=Neg;;

let add_big n1 n2 =
  let l1,l2 = n1.big_val,n2.big_val in
  match (n1.big_sign,n2.big_sign) with
    (Neg,Pos)
    -> if l1=l2 then {big_sign=Pos;big_val= nat_of_int 0} else
       if lt_nat l1 l2 then {big_sign=Pos;big_val=sub_nat l2 l1}
       else {big_sign=Neg;big_val=sub_nat l1 l2}
  | (Pos,Neg)
    -> if l1=l2 then {big_sign=Pos;big_val=nat_of_int 0} else
       if lt_nat l1 l2 then {big_sign=Neg;big_val=sub_nat l2 l1}
       else {big_sign=Pos;big_val=sub_nat l1 l2}
  | (Neg,Neg) -> {big_sign=Neg;big_val=add_nat l1 l2}
  | (Pos,Pos) -> {big_sign=Pos;big_val=add_nat l1 l2};;

let sub_big n1 n2 =
  let l1,l2= n1.big_val,n2.big_val in
  match (n1.big_sign,n2.big_sign) with
    (Neg,Pos) -> {big_sign=Neg;big_val=add_nat l1 l2}
  | (Pos,Neg) -> {big_sign=Pos;big_val=add_nat l1 l2}
  | (Neg,Neg)
    -> if l1=l2 then {big_sign=Pos;big_val=nat_of_int 0} else
       if lt_nat l1 l2 then {big_sign=Pos;big_val=sub_nat l2 l1}
       else {big_sign=Neg;big_val=sub_nat l1 l2}
  | (Pos,Pos)
    -> if l1=l2 then {big_sign=Pos;big_val=nat_of_int 0} else
       if lt_nat l1 l2 then {big_sign=Neg;big_val=sub_nat l2 l1}
       else {big_sign=Pos;big_val=sub_nat l1 l2};;

let mult_big n1 n2 =
  {big_sign=prod_sign (n1.big_sign,n2.big_sign);
   big_val= mult_nat n1.big_val n2.big_val};;

let mult_big_list = it_list mult_big (big_int_of_int 1);;

let div_big n1 n2 =
  let dvd = n1.big_val and dvs= n2.big_val in
  let (quot,rmd) = div_nat dvd dvs in
  match n1.big_sign, n2.big_sign with
    Pos,Pos -> {big_sign=Pos; big_val= quot},
               {big_sign=Pos; big_val= rmd}
  | Pos,Neg -> {big_sign=Neg; big_val= quot},
               {big_sign=Pos; big_val= rmd}
  | Neg,Pos -> {big_sign=Neg; big_val= quot},
               {big_sign=Neg; big_val= rmd}
  | Neg,Neg -> {big_sign=Pos; big_val= quot},
               {big_sign=Neg; big_val= rmd};;

let exp_big {big_sign=s; big_val=a} n=
  let s' = if n>=0 then Pos else Neg in
  match (s,s') with
    (Pos,Pos) -> {big_sign=Pos; big_val= exp_nat a n}
  | (Neg,Pos)
    -> let abs_n =abs n in
       if abs_n mod 2 = 0
       then {big_sign=Pos; big_val = exp_nat a abs_n}
       else {big_sign=Neg; big_val= exp_nat a abs_n}
  | _ -> failwith "exp_big: args must be positive";;

let sqrt_big = fun
  {big_sign=s; big_val=n} ->
    if s=Pos then {big_sign = Pos; big_val = sqrt_nat n}
    else failwith "sqrt_big: arg must be positive";;

(* +big_int_utilities+ *)
(* +mult_big+ *)

let prod_sign= fun
  (Neg,Neg) -> Pos
| (Neg,Pos) -> Neg
| (Pos,Neg) -> Neg
| (Pos,Pos) -> Pos;;

let mult_big n1 n2 =
  {big_sign = prod_sign (n1.big_sign,n2.big_sign);
   big_val = mult_nat n1.big_val n2.big_val};;

(* +mult_big+ *)
(* +big_int_parser+ *)

let rec big_parser = function
  [<'`-`; (nat_parser (nat_of_int 0)) n >]
  -> {big_sign=Neg ;big_val=n}
| [<(nat_parser (nat_of_int 0)) n >]
  -> {big_sign=Pos ;big_val=n};;

let parse_big_int s = big_parser (stream_of_string s);;

let big_int_of_string = parse_big_int;;

(* +big_int_parser+ *)
(* +big_int_printer+ *)

let print_big_int = fun
  {big_sign=s; big_val=l} ->
    (match s with Neg -> print_string "-" | Pos -> ()) ; print_nat l;;

(* +big_int_printer+ *)

let n= parse_big_int "999999999999999"
in mult_big n n;;

let rec fact n =
  if n = 0 then big_int_of_int 1
  else mult_big (big_int_of_int n) (fact (n-1));;

fact 50;;
