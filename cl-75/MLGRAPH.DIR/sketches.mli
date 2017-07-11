
#open  "MLgraph";;

type path = Spath of    MLgraph__geom_element list
          | Tpath of  MLgraph__transformation * path 
          | Cpath of path list;;

type sketch = {path: path;
               mutable frame:MLgraph__frame; 
               mutable exact_frame: bool;
               mutable hull: MLgraph__point list;
	       mutable size:int};;


value compute_path_hull : path -> point list;;
value compute_path_size : path -> int;;
value recompute_sketch_hull : sketch -> sketch;;
value sketch_frame : sketch -> frame;;
value has_exact_frame_sketch : sketch -> bool;;
value sketch_hull : sketch -> point list;;
value sketch_center : sketch -> point;;
value sketch_height : sketch -> float;;
value sketch_width : sketch -> float;;
value compute_size : geom_element list -> int;;
value recompute_sketch_hull : sketch -> sketch;;
value make_sketch : geom_element list -> sketch;;
value frame_sketch : frame -> sketch;;
value hull_sketch : point list -> sketch;;
value sketch_center : sketch -> point;;

(*value join_sketches : sketch -> sketch -> sketch;;*)
(*value JSK : sketch -> sketch -> sketch;;*)
(*value join_sketch_list : sketch list -> sketch;;*)

value group_sketches : sketch list -> sketch;;
value ungroup_sketch : sketch -> sketch list;;
value transform_sketch : transformation -> sketch -> sketch;;
value center_sketch : sketch -> point -> sketch;;
value fit_sketch_in_frame : sketch -> frame -> sketch;;
value force_sketch_in_frame : frame -> sketch -> sketch;;
value scale_sketch : float * float -> sketch -> sketch;;
value scale_and_center_sketch : float * float -> sketch -> sketch;;
value translate_sketch : float * float -> sketch -> sketch;;
value vflip_sketch : sketch -> sketch;;
value hflip_sketch : sketch -> sketch;;
value rotate_sketch : float -> sketch -> sketch;;
value besides_sketch : sketch -> sketch -> sketch;;
value over_sketch : sketch -> sketch -> sketch;;
(*
value BSK : sketch -> sketch -> sketch;;
value OSK : sketch -> sketch -> sketch;;
*)
value extend_sketch_frame : extension -> float -> sketch -> sketch;;
