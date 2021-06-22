let ( % ) = fun f g x -> f (g (x)) ;;

(* let o f g x = f(g x) *)

(*
For the record, flambda optimizes correctly with the following definition of the operators
(but the non-flambda optimizer does not):
let (%) g f = (); (fun x -> g (f x))
let (%>) f g = (); (fun x -> g (f x))
*)

let print_newline () =
  print_string "\026\n\026"; flush stdout ;;

let rec interval m n =
  if m > n then [] else m :: interval (m + 1) n ;;

let maxint x1 x2 = if x1 < x2 then x2 else x1 ;;

let maxfloat x y = if x > y then x else y ;;

let minfloat x y = if x < y then x else y ;;

let xcons x y = y :: x ;;

let id x = x ;;

let i x = x ;;

let k x _y = x ;;

let k2 _x y = y ;;


let rec find p = function
  | [] -> failwith "not_found"
  | (a :: l) -> if (p a) then a else find p l ;;

let rec loop p f x =
  if (p x) then x else loop p f (f x) ;;

(** +partition+ *)
let partition test l =
  let switch elem (l1, l2) =
    if test elem then (l1, elem :: l2) else (elem :: l1, l2)
  in List.fold_right switch l ([], []) ;;

(* REM:
   List.partition (fun x -> x > 2) [1;2;3;4];;
   - : int list * int list = ([3; 4], [1; 2])

   partition (fun x -> x > 2) [1;2;3;4];;
   - : int list * int list = ([1; 2], [3; 4])
*)

(** +select+ *)
let rec select p =function
  | [] -> []
  | (a :: l) -> if p a then a :: select p l
    else select p l ;;

let n = ref (-1) ;;

let rec loopn p f x =
  if (p x) then x
  else (n := !n + 1; print_int !n ;
        print_newline (); loopn p f (f x)) ;;

let union l1 l2 =
  List.fold_right (fun e u -> if List.mem e u then u else e :: u) l1 l2 ;;
