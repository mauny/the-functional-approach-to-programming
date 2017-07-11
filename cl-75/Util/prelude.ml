(* +type_option+ *)

type 'a option = None | Some of 'a;;

(* +type_option+ *)

let print_newline () =
  print_string "\026\n\026"; flush std_out;;

let rec interval m n =
  if m > n then [] else m::interval (m+1) n;;

(* +filter+ *)

let rec filter p = fun
  [] -> []
| (a::l) -> if p a then a::filter p l
            else filter p l ;;

(* +filter+ *)

let max_int x1 x2 = if x1 < x2 then x2 else x1;;

let max_float x y = if x >. y then x else y;;

let min_float x y = if x <. y then x else y;;

let cons x y = x::y;;

let xcons x y = y::x;;

let id x=x;;

let I x=x;;

let K x y = x;;

let K2 x y = y;;

#infix "o";;

let prefix o f g x = f(g x);;

let rec find p = fun
  [] -> failwith "not_found"
| (a::l) -> if (p a) then a else find p l;;

let rec loop p f x =
  if (p x) then x else loop p f (f x);;

(* +partition+ *)

let partition test l =
  let switch elem (l1,l2) =
    if test elem then (l1, elem::l2) else (elem::l1, l2)
  in list_it switch l ([],[]);;

(* +partition+ *)
(* +select+ *)

let rec select p =fun
  [] -> []
| (a::l) -> if p a then a::select p l
            else select p l;;

(* +select+ *)

let n = ref (-1);;

let rec loopn p f x =
  if (p x) then x
  else (n:=!n+1;print_int !n;
        print_newline();loopn p f (f x));;
