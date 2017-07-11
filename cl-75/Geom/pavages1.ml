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




(* +set_of_list+ *)

let rec set_of_list = fun
  [] -> []
| (a::l) -> if mem a l then set_of_list l
            else a::set_of_list l;;


(* +set_of_list+ *)
(* +power+ *)

let rec map_append f = fun
  [] -> [] 
| (a::l) -> (f a) @ map_append f l;;

let product f l1 l2 =
  map_append  (fun x ->  (map (f x) l2)) l1;;

let power f x l =
  pwr [x]
  where rec pwr ll = fun
    0 -> ll
  | n -> (set_of_list ll) @ pwr (product f l ll) (n-1);;

(* +power+ *)


(* +generate_transformations_group+ *)
let generate_transformations_group =
   power comp_trans id_trans;;

(* +generate_transformations_group+ *)


(* +generate_coltrans_group+ *)

let pairing f g = fun
  (x,y) (x',y') -> (f x x', g y y');;

let generate_coltrans_group trl n =
  match trl with
    [] -> []
  | _ -> let k = vect_length (fst(hd trl)) in
         set_of_list
           (power (pairing cpermut comp_trans)
              (id_permut k,id_trans) trl n);;

(* +generate_coltrans_group+ *)
(* +make_colored_tiling+ *)

let make_colored_tiling (c,lsty,sk) col trans =
  group_pictures
    (map (fun (v,t) ->
                (transform_picture
                   t (group_pictures
                        [make_fill_picture (Nzfill,col v.(c)) sk;
                         make_closed_draw_picture (lsty,black) sk])))
       trans);;

(* +make_colored_tiling+ *)



(* +transform_points+ *)
let xunit = {xc=1.0; yc=0.0};;
let rec transform_points t pts =
 map (transform_point t) pts;;

(* +transform_points+ *)

(* +points_of_arc+ *)
let rec intervalstep x y s =
 if x>.y then [] else x::intervalstep (x+.s) y s;;

let points_of_arc = fun
  (Arc (center,radius,a1,a2)) n
  -> let a = (a2 -. a1) /. (float_of_int n) in
     let pt1= transform_point
                (point_translation origin center)
                {xc= radius *. (cosinus a1); yc= radius *. (sinus a1)} in
     (map (fun a -> transform_point (rotation center a) pt1)
        (intervalstep a1 a2 a))
| _ _ -> failwith "points_of_arc";;


(* +points_of_arc+ *)





