(* $Id: frames.mlip,v 1.1 1997/08/14 11:34:50 emmanuel Exp $ *)


open Geometry

type frame = {mutable xmin:float; mutable xmax:float;  
              mutable ymin:float; mutable ymax:float};;  

type extension = All_ext | Horiz_ext | Vertic_ext | Left_ext
               | Right_ext | Top_ext | Bottom_ext;;



val frame_center: frame -> point;;


val extend_frame : extension -> float -> frame -> frame;;
val point_frame : point -> frame;;
val seg_frame : point list -> frame;;
val ordered_angles : float * float * float -> bool;;
val arc_frame : point * float * float * float -> frame;;
val curve_frame : point * point * point * point -> frame;;
val merge_frames : frame list -> frame;;
val compute_geom_elem_frame : geom_element -> frame;;
val compute_frame : geom_element list -> frame;;
val transform_frame : transformation -> frame -> frame;;
val frame_to_frame_transform : frame -> frame -> transformation;;

val set_exact_frame_mode : bool -> unit;;
val get_exact_frame_mode : unit -> bool;;
val set_discrete_curve_number : int -> unit;;
val get_discrete_curve_number : unit -> int;;
val set_discrete_circle_number : int -> unit;;
val get_discrete_circle_number : unit -> int;;
val compute_geom_elem_convex_hull : geom_element -> point list;;
val compute_geom_elem_list_convex_hull : geom_element list -> point list;;
val merge_convex_hulls : point list list -> point list;;
val frame_of_convex_hull : point list -> frame;;
val convex_hull_of_frame : frame -> point list;;

