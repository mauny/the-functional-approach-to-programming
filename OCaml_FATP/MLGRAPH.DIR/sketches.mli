(* $Id: sketches.mlip,v 1.1 1997/08/14 11:34:50 emmanuel Exp $ *)


open Geometry
open Frames

type path = Spath of  geom_element list
          | Tpath of  transformation * path 
          | Cpath of path list;;

type sketch = {path: path;
               mutable frame: frame; 
               mutable exact_frame: bool;
               mutable hull: point list;
	       mutable size:int};;


val compute_path_hull : path -> point list;;
val compute_path_size : path -> int;;
val recompute_sketch_hull : sketch -> sketch;;
val sketch_frame : sketch -> frame;;
val has_exact_frame_sketch : sketch -> bool;;
val sketch_hull : sketch -> point list;;
val sketch_center : sketch -> point;;
val sketch_height : sketch -> float;;
val sketch_width : sketch -> float;;
val compute_size : geom_element list -> int;;
val recompute_sketch_hull : sketch -> sketch;;
val make_sketch : geom_element list -> sketch;;
val frame_sketch : frame -> sketch;;
val hull_sketch : point list -> sketch;;
val sketch_center : sketch -> point;;

(*val join_sketches : sketch -> sketch -> sketch;;*)
(*val JSK : sketch -> sketch -> sketch;;*)
(*val join_sketch_list : sketch list -> sketch;;*)

val group_sketches : sketch list -> sketch;;
val ungroup_sketch : sketch -> sketch list;;
val transform_sketch : transformation -> sketch -> sketch;;
val center_sketch : sketch -> point -> sketch;;
val fit_sketch_in_frame : sketch -> frame -> sketch;;
val force_sketch_in_frame : frame -> sketch -> sketch;;
val scale_sketch : float * float -> sketch -> sketch;;
val scale_and_center_sketch : float * float -> sketch -> sketch;;
val translate_sketch : float * float -> sketch -> sketch;;
val vflip_sketch : sketch -> sketch;;
val hflip_sketch : sketch -> sketch;;
val rotate_sketch : float -> sketch -> sketch;;
val besides_sketch : sketch -> sketch -> sketch;;
val over_sketch : sketch -> sketch -> sketch;;
(*
val BSK : sketch -> sketch -> sketch;;
val OSK : sketch -> sketch -> sketch;;
*)
val extend_sketch_frame : extension -> float -> sketch -> sketch;;
