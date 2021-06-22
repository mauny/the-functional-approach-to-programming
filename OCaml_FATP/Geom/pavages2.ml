(* #use "load.ml" ;; *)

open Mlgraph
open Permutations

(** +make_tiling+ *)
let make_tiling p trans =
  group_pictures
    (List.map (fun t -> transform_picture t p) trans) ;;

(** +point_translation+ *)
let point_translation = fun
  { xc = x1; yc = y1 } { xc = x2; yc = y2 } -> translation (x2 -. x1, y2 -. y1) ;;

(** +comp_trans+ *)
let comp_trans t1 t2 = compose_transformations [t2; t1] ;;

let pairing f g = fun
  (x, y) (x', y') -> (f x x', g y y') ;;

(** +tgroup+ *)
type 'a tgroup =
  { tgroup_gens: 'a list;
    tgroup_op: 'a -> 'a list -> 'a list } ;;

(** +make_tgroup+ *)
let make_tgroup trl top =
  { tgroup_gens = trl;
    tgroup_op = top } ;;

(** +group_gen_exc+ *)
exception Group_gen_exc ;;

let rec map_try f = function
  | []       -> []
  | (a :: l) -> if (try f a; true with _ -> false)
    then (f a) :: map_try f l
    else map_try f l ;;

let rec product f l1 l2 =
  match l1 with
  | []       -> []
  | (a :: l) -> map_try (f a) l2 @ product f l l2 ;;

(** +power+ *)
let power f x l =
  let rec pwr ll = function
    | 0 -> ll
    | n -> ll @ pwr (product f l ll) (n - 1) in
  pwr [x] ;;

(** +generate_transformations_group+ *)
let generate_transformations_group tgroup=
  power tgroup.tgroup_op [] tgroup.tgroup_gens ;;

(** +ctgroup+ *)
type 'a ctgroup =
  { ctgroup_tgens: 'a list ;
    ctgroup_colgens: int array list ;
    ctgroup_top: 'a  -> 'a list  -> 'a list } ;;

let make_ctgroup trl ctrl top =
  if List.length trl <> List.length ctrl
  || List.length trl=0
  || (let n = Array.length (List.hd ctrl)
      in List.exists (fun v -> Array.length v <> n) (List.tl ctrl))
  then failwith "make_ctgroup: wrong ctgroup"
  else  { ctgroup_tgens = trl ;
          ctgroup_colgens = ctrl ;
          ctgroup_top = top } ;;

(** +generate_coltrans_group+ *)
let generate_coltrans_group ctgroup =
  let k = Array.length (List.hd ctgroup.ctgroup_colgens)
  in
  power (pairing cpermut ctgroup.ctgroup_top)
    (id_permut k, [])
    (List.combine ctgroup.ctgroup_colgens ctgroup.ctgroup_tgens) ;;

let rec intervalstep x y s =
  if x > y then [] else x :: intervalstep (x +. s) y s ;;

(** +points_of_arc+ *)
let points_of_arc arc n = match (arc, n) with
  | (Arc (center,radius,a1,a2)), n
    -> let a = (a2 -. a1) /. (float_of_int n) in
    let pt1 = transform_point
        (point_translation origin center)
        { xc = radius *. (cosinus a1); yc = radius *. (sinus a1) }  in
    (List.map (fun a -> transform_point (rotation center a) pt1)
       (intervalstep a1 a2 a))
  | _, _ -> failwith "points_of_arc" ;;

let xunit = { xc = 1.0; yc = 0.0 } ;;

(** +transform_points+ *)
let transform_points t pts =
  List.map (transform_point t) pts ;;
