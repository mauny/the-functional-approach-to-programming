
#open "MLgraph";;


#open "sketches";;
#open "bitmaps";;
#open "texts";;




type pict =  Blank of frame * color
          |  Draw of path * linestyle * color * bool * int   (* the int here is the *)
          |  Fill of path * fillstyle * color * int   (* path length which is*)
          |  Clip of clipstyle * path * pict * int    (* subject to display  *)
          |  Bitmap of bitmap                         (* limitations         *)
          |  Bitmapmask of bitmap * color * bool                        
          |  Text of text * color
          |  Tpict of transformation * pict
          |  Cpict of pict list;;

type picture = {pict : pict ; 
                mutable frameP : frame; 
                mutable exact_frameP : bool; 
                mutable hullP: point list;
                input_interface:interface; output_interface:interface};;


value recompute_picture_hull : picture -> picture;;
value picture_frame : picture -> frame;;
value has_exact_frame_picture : picture -> bool;;
value has_exact_frame : picture -> bool;;
value picture_hull : picture -> point list;;
value picture_center : picture -> point;;
value picture_input_interface : picture -> interface;;
value picture_output_interface : picture -> interface;;
value picture_height : picture -> float;;
value picture_width : picture -> float;;
(*value picture_color : picture -> color;;*)
value change_color_picture : color -> picture -> picture;;
value change_linestyle_picture : linestyle -> picture -> picture;;
value change_linewidth_picture : float -> picture -> picture;;
value set_picture_interfaces : picture -> interface * interface -> picture;;
(*value transform_interface : transformation -> interface -> interface;;*)
(*value find_handle : string -> interface -> point * point;;*)
(*value rem_assoc : string -> (string * 'a) list -> (string * 'a) list;;*)
(*value rem_port : string -> interface -> interface;;*)
(*value join_interfaces : interface * interface -> interface;;*)

value make_blank_picture : float * float -> picture;;
value blank_rectangle : float * float -> picture;;
value blank_square : float -> picture;;
value make_draw_picture : linestyle * color -> sketch -> picture;;
value make_closed_draw_picture : linestyle * color -> sketch -> picture;;
value make_default_draw_picture : sketch -> picture;;
value make_default_closed_draw_picture : sketch -> picture;;
value make_fill_picture : fillstyle * color -> sketch -> picture;;
value make_default_fill_picture : sketch -> picture;;
value clip_picture : clipstyle -> sketch -> picture -> picture;;
value make_bitmap_picture : bitmap -> picture;;
value make_bitmap_mask_picture : bitmap -> color -> bool -> picture;;
value make_default_bitmap_mask_picture : bitmap -> picture;;
value make_text_picture : font -> color -> string -> picture;;
value make_default_text_picture : string -> picture;;
value make_frame_picture : linestyle * color -> frame-> picture;;
value make_default_frame_picture : frame-> picture;;
value make_hull_picture : linestyle * color -> point list -> picture;;
value make_default_hull_picture : point list -> picture;;
value get_picture_frame_as_picture_with_lsty_and_color : picture -> linestyle * color -> picture;;
value get_picture_frame_as_picture : picture ->  picture;;
value get_picture_hull_as_picture_with_lsty_and_color :picture -> linestyle * color -> picture;;
value get_picture_hull_as_picture :picture ->  picture;;


value group_pictures : picture list -> picture;;
value ungroup_picture : picture -> picture list;;
value transform_picture : transformation -> picture -> picture;;
value center_picture : picture -> point -> picture;;
value translate_picture : float * float -> picture -> picture;;
value translate : float * float -> picture -> picture;;
(*value rotate_picture : point -> float -> picture -> picture;;*)
(*value rotate : point -> float -> picture -> picture;;*)
value scale_picture : float * float -> picture -> picture;;
value scale_and_center_picture : float * float -> picture -> picture;;
value scale : float * float -> picture -> picture;;
value fit_picture_in_frame : picture -> frame-> picture;;
value fit_in_frame : picture -> frame-> picture;;
value force_picture_in_frame : frame-> picture -> picture;;
value force_in_frame : frame-> picture -> picture;;
value add_frame_to_picture : picture -> picture;;
value add_frame : picture -> picture;;
value vflip_picture : picture -> picture;;
value vflip : picture -> picture;;
value hflip_picture : picture -> picture;;
value hflip : picture -> picture;;
value rotate_picture : float -> picture -> picture;;
value rotate : float -> picture -> picture;;
value subpicture_transformations : picture -> picture -> transformation list;;
value subpicture_colors : picture -> picture -> color list;;
value subpicture_clips : picture -> picture -> path list;;
value subpictures : picture -> picture list;;

(*value besides_picture' : alignment -> picture -> picture -> picture;;*)
(*value BPICT' : picture -> picture -> picture;;*)

value align_horizontally_picture_list : alignment -> picture list -> picture;;
value align_horizontally : alignment -> picture list -> picture;;

(*value over_picture' : alignment -> picture -> picture -> picture;;*)
(*value OPICT' : picture -> picture -> picture;;*)

value align_vertically_picture_list : alignment -> picture list -> picture;;
value align_vertically : alignment -> picture list -> picture;;

value besides_picture : picture -> picture -> picture;;
value over_picture : picture -> picture -> picture;;
(*value BPICT : picture -> picture -> picture;;*)
(*value OPICT : picture -> picture -> picture;;*)
value besides : picture -> picture -> picture;;
value over : picture -> picture -> picture;;

value compose_horizontally_picture_list : picture list -> picture;;
value compose_horizontally : picture list -> picture;;
value compose_vertically_picture_list : picture list -> picture;;
value compose_vertically : picture list -> picture;;
value extend_picture_frame : extension -> float -> picture -> picture;;
value named_attach_pictures : picture * picture -> string * string -> picture;;
value attach_pictures : picture * picture -> picture;;
(*value APICT : picture -> picture -> picture;;*)


value make_textblock_picture : alignment -> float -> font -> color -> string list -> picture;;

