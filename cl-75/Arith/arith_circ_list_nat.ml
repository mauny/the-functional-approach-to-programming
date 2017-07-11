(* REPRESENTATION DES GRANDS NOMBRES PAR DES LISTES CIRCULAIRES *)
(* DOUBLEMENT CHAINEES *)


(* +nat_base+ *)

let nat_base = 10000;;

(* +nat_base+ *)

(* +add_sub_mult_carry+ *)

let add_carry c (m,n) =
  let s = m+n+c in (s/nat_base, s mod nat_base);;

let sub_carry c (m,n) =
  let s = m-(n+c) in
  if s>=0 then (0,s) else (1,nat_base+s);;

let mult_carry c (m,n) =
  let p = m*n + c in (p/nat_base, p mod nat_base);;

(* +add_sub_mult_carry+ *)

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
  match l
  with [] -> print_int 0
     | [a] -> print_int a
     | (a::l) -> print_int a ; print_rec l;;

(* +nat_printer+ *)


(* +type_dlnode+ *)

type 'a dlnode =
     { mutable info: 'a;
       mutable prev: 'a dlnode;
       mutable next: 'a dlnode
     };;

(* +type_dlnode+ *)
(* +dlnode_utilities+ *)

let mk_dbl_circular_list e =
  let rec x = {info = e; prev = x; next = x}
  in x;;

let insert_before e l =
  let lprev = l.prev in
  let x={info=e; prev=lprev; next=l} in
  lprev.next <- x; l.prev <-x ;;

let insert_after e l =
  let lnext = l.next in
  let x = {info = e; prev = l; next = lnext} in
  lnext.prev<-x; l.next<-x;;

let list_of_dll dl =
  lodll dl
  where rec lodll dl' =
    dl'.info::(if dl'.next == dl then [] else lodll dl'.next);;

let dll_length dl =
  length dl
  where rec length dl' =
    1+(if dl'.next == dl then 0 else length dl'.next);;

(* +dlnode_utilities+ *)
(* +zero_nat+ *)

let zero_nat n =
  n.info = 0 & n.next == n;;

(* +zero_nat+ *)
(* +nat_convs+ *)

let rec nat_of_int n =
  let n0 = n mod nat_base and r=n/nat_base in
  let accu= mk_dbl_circular_list n0 in
  nat_rec accu r
  where rec nat_rec dll p =
    if p = 0 then accu else
    begin
      insert_after (p mod nat_base) dll;
      nat_rec dll.next (p/nat_base)
    end;;

let int_of_nat n =
  int_rec n.prev n.prev.info
  where rec int_rec dll accu =
    if dll.prev==n.prev then accu else
    int_rec dll.prev (accu*nat_base+dll.prev.info);;

(* +nat_convs+ *)
(* +copy+ *)

let copy n =
  let accu= mk_dbl_circular_list n.info in
  copy_rec accu n.next
  where rec copy_rec dl1 dl2=
    if dl2 == n then accu else
    begin
      insert_after dl2.info dl1;
      copy_rec dl1.next dl2.next
    end;;

(* +copy+ *)
(* +add_nat+ *)

let add_nat m n =
  if zero_nat m then n else
  if zero_nat n then m else
  let accu = copy m in
  add_rec 0 accu n
  where rec add_rec c dll dll'=
    let (c',n') = add_carry c (dll.info, dll'.info) in
    begin
      dll.info<-n';
      if dll'.next==n then propagate_carry c' dll else
      if dll.next==accu
      then begin insert_after c' dll; add_rec 0 dll.next dll'.next end
      else add_rec c' dll.next dll'.next
    end
  and propagate_carry c dll =
    if c = 0 then accu else
    if dll.next == accu then begin insert_after c dll; accu end
    else let (c',n')= add_carry 0 (c,(dll.next).info) in
         begin
           (dll.next).info<-n';
           if c'=0 then accu else propagate_carry c' dll.next
         end;;

(* +add_nat+ *)
(* +mult_nat+ *)

let un_nat n =
  n.info = 1 & n.next == n;;

let mult_nat m n =
  if zero_nat m then m else
  if zero_nat n then n else
  if un_nat m then n else
  if un_nat n then m else
  let accu = nat_of_int 0 in
  mult_rec1 accu n
  where rec mult_rec1 dllaccu dlln =
    mult_rec2 dllaccu m dlln.info;
    if dlln.next==n then accu else
    begin
      if dllaccu.next==accu then insert_after 0 dllaccu else ();
      mult_rec1 dllaccu.next dlln.next
    end
  and mult_rec2 dllaccu dllm d=
    let (c,n) = mult_carry dllaccu.info (dllm.info ,d) in
    begin
      dllaccu.info<-n;
      if dllm.next==m then
        if dllaccu.next==accu & c<>0 then insert_after c dllaccu
        else () else
      begin
        (if dllaccu.next==accu then insert_after c dllaccu
         else (dllaccu.next).info <- (dllaccu.next).info + c);
        mult_rec2 dllaccu.next dllm.next d
      end
    end;;

(* +mult_nat+ *)
(* +lt_nat+ *)

let lt_nat m n =
  let rec lt_rec dll1 dll2 =
    dll1.info < dll2.info or
    dll1.info = dll2.info & not(dll1.prev==m) &
    lt_rec dll1.prev dll2.prev in
  let lm=dll_length m and ln=dll_length n in
  lm < ln or lm = ln & lt_rec m n;;

(* +lt_nat+ *)
(* ancienne version
   let lt_nat m n=
   let lm=dll_length m and ln=dll_length n in
   lm < ln or lm=ln & lt_rec m n
   where rec lt_rec dll1 dll2=
   dll1.info < dll2.info
   or  dll1.info = dll2.info  & not(dll1.prev==m)
   &  lt_rec dll1.prev dll2.prev;;
 *)

let rec nat_of_int n =
  let n0 = n mod nat_base and r = n/nat_base in
  let accu = mk_dbl_circular_list n0 in
  nat_rec accu r
  where rec nat_rec dll p =
    if p=0 then accu else
    begin
      insert_after (p mod nat_base) dll;
      nat_rec dll.next (p/nat_base)
    end;;

let rec fact2 n =
  if n=0 then nat_of_int 1
  else mult_nat (nat_of_int n) (fact2 (n-1));;

(* 
   fact2 50;;
 *)

let print_dll l = print_nat (list_of_dll l);;

let M m n= print_dll (mult_nat (nat_of_int m) (nat_of_int n));;

let M2 m n= print_dll (mult_nat m (nat_of_int n));;

let M3 m n= print_dll (mult_nat (nat_of_int m) n);;
