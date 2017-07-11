
#open "MLgraph";;


value frame_center: frame -> point;;


value extend_frame : extension -> float -> frame -> frame;;
value point_frame : point -> frame;;
value seg_frame : point list -> frame;;
value ordered_angles : float * float * float -> bool;;
value arc_frame : point * float * float * float -> frame;;
value curve_frame : point * point * point * point -> frame;;
value merge_frames : frame list -> frame;;
value compute_geom_elem_frame : geom_element -> frame;;
value compute_frame : geom_element list -> frame;;
value transform_frame : transformation -> frame -> frame;;
value frame_to_frame_transform : frame -> frame -> transformation;;

value set_exact_frame_mode : bool -> unit;;
value get_exact_frame_mode : unit -> bool;;
value set_discrete_curve_number : int -> unit;;
value get_discrete_curve_number : unit -> int;;
value set_discrete_circle_number : int -> unit;;
value get_discrete_circle_number : unit -> int;;
value compute_geom_elem_convex_hull : geom_element -> point list;;
value compute_geom_elem_list_convex_hull : geom_element list -> point list;;
value merge_convex_hulls : point list list -> point list;;
value frame_of_convex_hull : point list -> frame;;
value convex_hull_of_frame : frame -> point list;;

