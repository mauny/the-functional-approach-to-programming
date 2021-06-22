(* $Id: pictures.mlip,v 1.1 1997/08/14 11:34:50 emmanuel Exp $ *)

open Geometry
open Frames
open Paint
open Sketches
open Bitmaps
open Fonts
open Texts
open Graphics_defaults

type interface = No_handle | One_handle of point * point
                    | Handles of (string * (point*point)) list;;



type alignment = Align_Right | Align_Left | Align_Center
               | Align_Top | Align_Bottom;;



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


val recompute_picture_hull : picture -> picture;;
val picture_frame : picture -> frame;;
val has_exact_frame_picture : picture -> bool;;
val has_exact_frame : picture -> bool;;
val picture_hull : picture -> point list;;
val picture_center : picture -> point;;
val picture_input_interface : picture -> interface;;
val picture_output_interface : picture -> interface;;
val picture_height : picture -> float;;
val picture_width : picture -> float;;
(*val picture_color : picture -> color;;*)
val change_color_picture : color -> picture -> picture;;
val change_linestyle_picture : linestyle -> picture -> picture;;
val change_linewidth_picture : float -> picture -> picture;;
val set_picture_interfaces : picture -> interface * interface -> picture;;
(*val transform_interface : transformation -> interface -> interface;;*)
(*val find_handle : string -> interface -> point * point;;*)
(*val rem_assoc : string -> (string * 'a) list -> (string * 'a) list;;*)
(*val rem_port : string -> interface -> interface;;*)
(*val join_interfaces : interface * interface -> interface;;*)

val make_blank_picture : float * float -> picture;;
val blank_rectangle : float * float -> picture;;
val blank_square : float -> picture;;
val make_draw_picture : linestyle * color -> sketch -> picture;;
val make_closed_draw_picture : linestyle * color -> sketch -> picture;;
val make_default_draw_picture : sketch -> picture;;
val make_default_closed_draw_picture : sketch -> picture;;
val make_fill_picture : fillstyle * color -> sketch -> picture;;
val make_default_fill_picture : sketch -> picture;;
val clip_picture : clipstyle -> sketch -> picture -> picture;;
val make_bitmap_picture : bitmap -> picture;;
val make_bitmap_mask_picture : bitmap -> color -> bool -> picture;;
val make_default_bitmap_mask_picture : bitmap -> picture;;
val make_text_picture : font -> color -> string -> picture;;
val make_default_text_picture : string -> picture;;
val make_frame_picture : linestyle * color -> frame-> picture;;
val make_default_frame_picture : frame-> picture;;
val make_hull_picture : linestyle * color -> point list -> picture;;
val make_default_hull_picture : point list -> picture;;
val get_picture_frame_as_picture_with_lsty_and_color : picture -> linestyle * color -> picture;;
val get_picture_frame_as_picture : picture ->  picture;;
val get_picture_hull_as_picture_with_lsty_and_color :picture -> linestyle * color -> picture;;
val get_picture_hull_as_picture :picture ->  picture;;


val group_pictures : picture list -> picture;;
val ungroup_picture : picture -> picture list;;
val transform_picture : transformation -> picture -> picture;;
val center_picture : picture -> point -> picture;;
val translate_picture : float * float -> picture -> picture;;
val translate : float * float -> picture -> picture;;
(*val rotate_picture : point -> float -> picture -> picture;;*)
(*val rotate : point -> float -> picture -> picture;;*)
val scale_picture : float * float -> picture -> picture;;
val scale_and_center_picture : float * float -> picture -> picture;;
val scale : float * float -> picture -> picture;;
val fit_picture_in_frame : picture -> frame-> picture;;
val fit_in_frame : picture -> frame-> picture;;
val force_picture_in_frame : frame-> picture -> picture;;
val force_in_frame : frame-> picture -> picture;;
val add_frame_to_picture : picture -> picture;;
val add_frame : picture -> picture;;
val vflip_picture : picture -> picture;;
val vflip : picture -> picture;;
val hflip_picture : picture -> picture;;
val hflip : picture -> picture;;
val rotate_picture : float -> picture -> picture;;
val rotate : float -> picture -> picture;;
val subpicture_transformations : picture -> picture -> transformation list;;
val subpicture_colors : picture -> picture -> color list;;
val subpicture_clips : picture -> picture -> path list;;
val subpictures : picture -> picture list;;

(*val besides_picture' : alignment -> picture -> picture -> picture;;*)
(*val BPICT' : picture -> picture -> picture;;*)

val align_horizontally_picture_list : alignment -> picture list -> picture;;
val align_horizontally : alignment -> picture list -> picture;;

(*val over_picture' : alignment -> picture -> picture -> picture;;*)
(*val OPICT' : picture -> picture -> picture;;*)

val align_vertically_picture_list : alignment -> picture list -> picture;;
val align_vertically : alignment -> picture list -> picture;;

val besides_picture : picture -> picture -> picture;;
val over_picture : picture -> picture -> picture;;
(*val BPICT : picture -> picture -> picture;;*)
(*val OPICT : picture -> picture -> picture;;*)
val besides : picture -> picture -> picture;;
val over : picture -> picture -> picture;;

val compose_horizontally_picture_list : picture list -> picture;;
val compose_horizontally : picture list -> picture;;
val compose_vertically_picture_list : picture list -> picture;;
val compose_vertically : picture list -> picture;;
val extend_picture_frame : extension -> float -> picture -> picture;;
val named_attach_pictures : picture * picture -> string * string -> picture;;
val attach_pictures : picture * picture -> picture;;
(*val APICT : picture -> picture -> picture;;*)


val make_textblock_picture : alignment -> float -> font -> color -> string list -> picture;;

