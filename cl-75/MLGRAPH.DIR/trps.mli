


(*#open "MLgraph";;*) 
#open  "sketches";;
(*#open "bitmaps";;*)
(*#open "fonts";;
#open  "texts";;
*)
#open  "pictures";;
(*
#open "cps";;
*)



(*value PATH_SIZE_LIMIT : int ref;;*)
(*value SYMBOL_LIMIT : int ref;;*)
(*value GSAVE_LIMIT : int ref;;*)
(*value DASH_SIZE_LIMIT : int ref;;*)
value set_path_size_limit : int -> unit;;
value set_symbol_max : int -> unit;;
value set_gsave_max : int -> unit;;
value set_dash_size_limit : int -> unit;;

(*exception OutSymbols of int*)
(*;;*)
(*value display_first_elt : geom_element -> unit;;*)
(*value display_elt : geom_element -> unit;;*)
(*value PS_matrix : transformation -> ps_matrix;;*)
(*value reset_gensym : unit -> unit;;*)
(*value gensym : unit -> string;;*)
(*value reset_gsave : unit -> unit;;*)
(*value newgrestore_PS : unit -> unit;;*)
(*value newgsave_PS : unit -> unit;;*)
(*value skel_assoc_list : (geom_element list * (bool * string)) list ref;;*)
(*value bitmap_assoc_list : (bitmap * string) list ref;;*)
(*value reset_trps : unit -> unit;;*)
(*value declare_skel_proc_name : bool -> geom_element list -> string;;*)
(*value declare_bitmap_proc_name : (bitmap -> 'a) -> bitmap -> string;;*)
(*value find_skel_proc_name : bool -> geom_element list -> string;;*)
(*value find_bitmap_proc_name : (bitmap -> 'a) -> bitmap -> string;;*)
(*value optimized_mode : bool ref;;*)
(*value use_dash : bool ref;;*)
(*value set_optimized_mode : bool -> unit;;*)
(*value set_use_dash : bool -> unit;;*)
value display_path : bool -> path -> unit;;
value special_display_path : path -> string -> unit;;
(*value transform_linewidth : transformation -> float -> float;;*)
(*value print_bitmap_PS : bitmap -> unit;;*)
(*value print_bitmap_to_hexastring_PS : bitmap -> unit;;*)
(*value display_bitmap_PS : bitmap -> bool -> bool -> unit;;*)
(*value display_bitmap_from_hexastring_PS : bitmap -> bool -> bool -> unit;;*)
(*value actual_font : font ref;;*)
(*value reset_actual_font : unit -> unit;;*)
(*value display_text_PS : text -> color -> unit;;*)
(*value display_linestyle : color -> linestyle -> unit;;*)
(*value display_pict : pict -> unit;;*)
value display_picture : picture -> unit;;

(*
value open_PS: string -> ps_channel;;
value close_PS: bool -> unit;;
value send_comment_PS: string -> unit;;
value copy_prelude_PS: unit -> unit;;
*)
value reset_trps: unit -> unit;;
value reset_actual_font: unit -> unit;;
value set_optimized_mode: bool -> unit;;
