#open "../Util/prelude";;

#open "arith_list_nat";;

#open "arith_big_int";;

#open "arith_rat";;

(* 
   let sqrt_dicho_nat n = sqr (nat_of_int 0) n
   where rec sqr a b=
   let p= fst(div_digit_nat (add_nat a b) 2) in
   let p2= mult_nat p p in
   match comp_nat p2 n with
   Equiv -> p
   | Smaller -> if comp_nat (add_digit_nat 1 p) b= Equiv
   then p else sqr p b
   | Greater -> if comp_nat (add_digit_nat 1 a) p= Equiv
   then p else sqr a p;;
 *)
(* +sqrt640320+ *)

let sqrt640320 digits =
  let pow = exp_nat (nat_of_int 10) digits in
  {rat_sign=Pos;
   rat_num=sqrt_nat (mult_nat_list [nat_of_string "640320"; pow;pow]);
   rat_den= pow};;

(* +sqrt640320+ *)
(* +size_nat+ *)

let size_nat = fun
  (Left_nat l) -> list_length l
| (Right_nat l) -> list_length l;;

(* +size_nat+ *)
(* +test+ *)

let test (x,y,z,t) = size_nat x + size_nat y + z < size_nat t;;

(* +test+ *)
(* +approx_pi+ *)

let approx_pi digits =
  let a0 = nat_of_string "12"
  and b0 = nat_of_string "13591409"
  and d0 = nat_of_string "640320"
  and n0 = mult_nat (nat_of_string "13591409")
            (nat_of_string "12")
  and s0 = nat_of_string "0"
  and v0 = nat_of_string "1"
  and t0 = nat_of_string "0"
  and sqrt = sqrt640320 (digits-2)
  and pow3 = exp_nat (nat_of_string "640320") 3
  in approx_rec a0 b0 t0 d0 n0 s0 v0 true
     where rec approx_rec a b t d n s v pos=
       if test(a,b,digits,d)
       then mult_rat sqrt
              (div_rat (rat_of_nat d) (rat_of_nat n))
       else let a =
              mult_nat_list [nat_of_int 8;
                             add_digit_nat 1 s;
                             add_digit_nat 3 s;
                             add_digit_nat 5 s;
                             a] in
            let b = add_nat (nat_of_string "545140134") b in
            let t = add_nat v t in
            let d = mult_nat_list [t;pow3;d] in
            let n = (if pos then sub_nat else add_nat)
                      (mult_nat_list [t;pow3;n])
                      (mult_nat_list [a; b]) in
            let s = add_digit_nat 6 s in
            let v = add_nat s v in
               approx_rec a b t d n s v (not pos);;

(* +approx_pi+ *)


(*
set_frac_length 10;;
approx_pi 40;;
*)
