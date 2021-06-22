(* #use "load.ml" ;; *)

(** +sentence+ *)
let sentence f =
  print_string "Note d'ecrit: ";
  let n1 = float_of_string (read_line ()) in
  print_string "Note d'oral: ";
  let n2 = float_of_string (read_line ()) in
  f(n1, n2) ;;

(** +mem_vect+ *)
let mem_vect c e v =
  let rec find m n =
    if m > n then false else
      let p = (m + n) / 2 in
      if v.(p) = e then true else
      if c (v.(p)) e then find (p + 1) n
      else find m (p - 1)
  in find 0 (Array.length v) ;;

(** +swap+ *)
let swap v i j =
  let x = v.(i) in
  begin
    v.(i) <- v.(j);
    v.(j) <- x
  end ;;

(** +place_quicksort+ *)
let place c v i j =
  let rec place_rec i' j' =
    let rec move_right p =
      if c (v.(i)) (v.(p)) || p = j' then p else move_right (p+1)
    and move_left p =
      if c (v.(p)) (v.(i)) || p = i' then p else move_left (p-1) in
    let k = move_right i' and l = move_left j' in
    if k > l then (swap v i l; l) else
    if k = l then
      if c (v.(l)) (v.(i)) then (swap v i l; l) else i
    else (swap v k l; place_rec (k+1) (l-1))
  in place_rec (i+1) j ;;

let quicksort c v =
  let rec quick i j =
    if i < j then
      let p = place c v i j in
      quick i (p-1);
      quick (p+1) j
  in quick 0 (Array.length v-1) ;;

(** +type_planar_point+ *)
type planar_point = { mutable xcoord: float; mutable ycoord: float } ;;

(** +translate+ *)
let translate (dx, dy) pt =
  pt.xcoord <- pt.xcoord +. dx; pt.ycoord <- pt.ycoord +. dy; pt ;;

(** +gensym+ *)
let gensym =
  let count = ref (-1) in
  fun () -> count := !count + 1;
    "ident" ^ (string_of_int !count) ;;

(* open Circular_list ;; *)
(* first ;; *)

(* open Double_circular_list ;; *)
(* last ;; *)
