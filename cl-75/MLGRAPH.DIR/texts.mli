


#open "MLgraph";;



type text = { t_string : string ;  t_font : font }
;;

(*value font_list : (font_style * font_description) list ref;;*)

value add_font : font_style * font_description -> unit;;
value remove_font : font_style * font_description -> unit;;
value print_font_list : unit -> unit;;
value print_info_font : font_description -> unit;;
value print_info_all_fonts : unit -> unit;;

value find_font_description : font -> font_description;;
value value_default_font : font;;
value gs_default_font : font ref;;
value default_font : unit -> font;;
value set_default_font : font -> unit;;
value reset_default_font : unit -> unit;;
value make_font : font_style -> float -> font;;

value make_text : string -> font -> text;;
value make_default_text : string -> text;;
value change_size_text : text -> float -> text;;
value change_font_text : text -> font -> text;;
value full_char_width : font -> int -> float;;

value text_frame : font -> string -> frame;;
value text_width : font -> string -> float;;
value text_height : font -> string -> float;;

