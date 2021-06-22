(* $Id: texts.mlip,v 1.1 1997/08/14 11:34:50 emmanuel Exp $ *)



open Frames
open Fonts


type text = { t_string : string ;  t_font : font }
;;

(*val font_list : (font_style * font_description) list ref;;*)

val add_font : font_style * font_description -> unit;;
val remove_font : font_style * font_description -> unit;;
val print_font_list : unit -> unit;;
val print_info_font : font_description -> unit;;
val print_info_all_fonts : unit -> unit;;

val find_font_description : font -> font_description;;
val value_default_font : font;;
val gs_default_font : font ref;;
val default_font : unit -> font;;
val set_default_font : font -> unit;;
val reset_default_font : unit -> unit;;
val make_font : font_style -> float -> font;;

val make_text : string -> font -> text;;
val make_default_text : string -> text;;
val change_size_text : text -> float -> text;;
val change_font_text : text -> font -> text;;
val full_char_width : font -> int -> float;;

val text_frame : font -> string -> frame;;
val text_width : font -> string -> float;;
val text_height : font -> string -> float;;

