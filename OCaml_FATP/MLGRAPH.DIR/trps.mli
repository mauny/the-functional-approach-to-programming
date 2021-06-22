(* $Id: trps.mlip,v 1.1 1997/08/14 11:34:50 emmanuel Exp $ *)


open  Compatibility ;;
open  Prelude ;;
open  Bitmaps ;;
open  Fonts ;;
open  Texts ;;
open  Frames ;;
open  Paint ;;
open  Geometry ;;
open  Graphics_defaults ;;
open  Sketches ;;
open  Pictures ;;
open  Cps ;;



(*val PATH_SIZE_LIMIT : int ref;;*)
(*val SYMBOL_LIMIT : int ref;;*)
(*val GSAVE_LIMIT : int ref;;*)
(*val DASH_SIZE_LIMIT : int ref;;*)
val set_path_size_limit : int -> unit;;
val set_symbol_max : int -> unit;;
val set_gsave_max : int -> unit;;
val set_dash_size_limit : int -> unit;;

(*exception OutSymbols of int*)
(*;;*)
(*val display_first_elt : geom_element -> unit;;*)
(*val display_elt : geom_element -> unit;;*)
(*val PS_matrix : transformation -> ps_matrix;;*)
(*val reset_gensym : unit -> unit;;*)
(*val gensym : unit -> string;;*)
(*val reset_gsave : unit -> unit;;*)
(*val newgrestore_PS : unit -> unit;;*)
(*val newgsave_PS : unit -> unit;;*)
(*val skel_assoc_list : (geom_element list * (bool * string)) list ref;;*)
(*val bitmap_assoc_list : (bitmap * string) list ref;;*)
(*val reset_trps : unit -> unit;;*)
(*val declare_skel_proc_name : bool -> geom_element list -> string;;*)
(*val declare_bitmap_proc_name : (bitmap -> 'a) -> bitmap -> string;;*)
(*val find_skel_proc_name : bool -> geom_element list -> string;;*)
(*val find_bitmap_proc_name : (bitmap -> 'a) -> bitmap -> string;;*)
(*val optimized_mode : bool ref;;*)
(*val use_dash : bool ref;;*)
(*val set_optimized_mode : bool -> unit;;*)
(*val set_use_dash : bool -> unit;;*)
val display_path : bool -> path -> unit;;
val special_display_path : path -> string -> unit;;
(*val transform_linewidth : transformation -> float -> float;;*)
(*val print_bitmap_PS : bitmap -> unit;;*)
(*val print_bitmap_to_hexastring_PS : bitmap -> unit;;*)
(*val display_bitmap_PS : bitmap -> bool -> bool -> unit;;*)
(*val display_bitmap_from_hexastring_PS : bitmap -> bool -> bool -> unit;;*)
(*val actual_font : font ref;;*)
(*val reset_actual_font : unit -> unit;;*)
(*val display_text_PS : text -> color -> unit;;*)
(*val display_linestyle : color -> linestyle -> unit;;*)
(*val display_pict : pict -> unit;;*)
val display_picture : picture -> unit;;

(*
val open_PS: string -> ps_channel;;
val close_PS: bool -> unit;;
val send_comment_PS: string -> unit;;
val copy_prelude_PS: unit -> unit;;
*)
val reset_trps: unit -> unit;;
val reset_actual_font: unit -> unit;;
val set_optimized_mode: bool -> unit;;
