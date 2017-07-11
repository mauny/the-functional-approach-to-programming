#open "../Util/prelude";;
#directory "../MLGRAPH.DIR";;
#open "MLgraph";;
#open "permutations";;
#infix "ctrans";;

(* +make_tiling+ *)
let make_tiling  p  trans  =
 group_pictures
  (map (fun t -> transform_picture t p) trans);;

(* +make_tiling+ *)

(* +point_translation+ *)
let point_translation = fun
  {xc=x1; yc=y1} {xc=x2; yc=y2} -> translation (x2-.x1, y2-.y1);;
(* +point_translation+ *)


(* +comp_trans+ *)
let comp_trans t1 t2 = compose_transformations [t2;t1];;
(* +comp_trans+ *)

let pairing f g = fun
  (x,y) (x',y') -> (f x x', g y y');;

(* +tgroup+ *)
type 'a tgroup =
  {tgroup_gens:'a list;
   tgroup_op: 'a -> 'a list -> 'a list};;
(* +tgroup+ *)
(* +make_tgroup+ *)
let make_tgroup trl top =
  {tgroup_gens=trl;
   tgroup_op=top};;
(* +make_tgroup+ *)

(* +group_gen_exc+ *)
exception Group_gen_exc;;
(* +group_gen_exc+ *)





(* +power2+ *)
let rec map_try f = fun
     [] -> [] 
   | (a::l) -> if (try f a; true with _ -> false)
                  then (f a) :: map_try f l
                  else map_try f l;;

let rec product f l1 l2 =
  match l1 with
        []  -> []
    | (a::l) -> map_try (f a) l2 @ product f l l2;;
    
let power f x l = 
pwr [x]
where rec pwr ll = fun
          0   ->  ll
   |      n   ->  ll@ pwr (product f l ll) (n-1);;

(* +power2+ *)


(* +generate_transformations_group2+ *)
let generate_transformations_group tgroup=
   power tgroup.tgroup_op [] tgroup.tgroup_gens;;

(* +generate_transformations_group2+ *)





(* +ctgroup+ *)
type 'a ctgroup =
  {ctgroup_tgens: 'a list;
   ctgroup_colgens: int vect list;
   ctgroup_top: 'a  -> 'a list  -> 'a list};;
   
let make_ctgroup trl ctrl top =
 if list_length trl <> list_length ctrl
    or list_length trl=0
    or (let n = vect_length (hd ctrl)
        in exists (fun v -> vect_length v <> n) (tl ctrl))
  then failwith "make_ctgroup: wrong ctgroup"
  else  {ctgroup_tgens=trl;
   ctgroup_colgens=ctrl;
   ctgroup_top=top};;

(* +ctgroup+ *)

(* +generate_coltrans_group2+ *)
let generate_coltrans_group ctgroup =
  let k= vect_length (hd ctgroup.ctgroup_colgens)
  in
     power (pairing cpermut ctgroup.ctgroup_top)
         (id_permut k,[])
         (combine (ctgroup.ctgroup_colgens, ctgroup.ctgroup_tgens));;

(* +generate_coltrans_group2+ *)


(* +points_of_arc+ *)
let rec intervalstep x y s =
 if x>.y then [] else x::intervalstep (x+.s) y s;;

let points_of_arc = fun
  (Arc (center,radius,a1,a2)) n
  -> let a = (a2 -. a1) /. (float_of_int n) in
     let pt1= transform_point
                (point_translation origin center)
                {xc= radius *. (cosinus a1); yc= radius *. (sinus a1)}     in
 (map (fun a -> transform_point (rotation center a) pt1)
           (intervalstep a1 a2 a))
| _ _ -> failwith "points_of_arc";;


(* +points_of_arc+ *)

(* +transform_points+ *)
let xunit = {xc=1.0; yc=0.0};;
let rec transform_points t pts =
 map (transform_point t) pts;;

(* +transform_points+ *)
