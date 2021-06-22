(* $Id: mlgraph.mlip,v 1.3 1997/08/14 14:26:03 emmanuel Exp $ *)
 
(* BEGIN COMPATIBILITY *)
(*
val lt_int : 'a -> 'a -> bool
val lt_float : 'a -> 'a -> bool
val le_float : 'a -> 'a -> bool
val gt_int : 'a -> 'a -> bool
val gt_float : 'a -> 'a -> bool
val mult_float : float -> float -> float
val add_int : int -> int -> int
val int_of_char : char -> int
val char_of_int : int -> char
val space_char : char
val lf_char : char
val char_0 : char
val comma_char : char
val open_par_char : char
val close_par_char : char
val ascii_0 : int
val ascii_9 : int
val ascii_a : int
val ascii_f : int
val ascii_A : int
val ascii_F : int
val eq_string : string -> string -> bool
val string_length : string -> int
val sub_string : string -> int -> int -> string
*)
val map : ('a -> 'b) -> 'a list -> 'b list
(*
val nth_char : string -> int -> char
val set_nth_char : string -> int -> char -> unit
val create_string : int -> string
val make_string : int -> char -> string
val it_list : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val list_it : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val append : 'a list -> 'a list -> 'a list
val list_length : 'a list -> int
val int_of_float : float -> int
val float_of_int : int -> float
val rev : 'a list -> 'a list
val do_list : ('a -> 'b) -> 'a list -> unit
*)

type 'a vect = 'a Compatibility.vect

(*
val mem : 'a -> 'a list -> bool
val vect_of_list : 'a list -> 'a array
val make_vect : int -> 'a -> 'a array
val except : 'a list -> 'a -> 'a list
val subtract : 'a list -> 'a list -> 'a list
*)
val assoc : 'a -> ('a * 'b) list -> 'b
(*
val index : 'a -> 'a list -> int
val replace_string : string -> string -> int -> unit
val format_float : ('a, unit, string) format -> 'a
val std_out : out_channel
val assq : 'a -> ('a * 'b) list -> 'b
val combine : 'a list -> 'b list -> ('a * 'b) list
val hd : 'a list -> 'a
val tl : 'a list -> 'a list
val vect_length : 'a array -> int
*)

(* open Compatibility;; *)
(* END COMPATIBILITY *)


(* BEGIN PRELUDE *)
(*val o : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b;;*)
(*val min_int : int -> int -> int;;*)
(*val max_int : int -> int -> int;;*)
(*val max : float -> float -> float;;*)
(*val min : float -> float -> float;;*)
(*val Pi : float;;*)
(*val cva : float -> float;;*)
val sinus : float -> float;;
val cosinus : float -> float;;
(*val explode : string -> string list;;*)
(*val explode_ascii : string -> int list;;*)
(*val nth_ascii : int * string -> int;;*)
(*val set_nth_ascii : int * string * int -> unit;;*)
(*val ascii : int -> string;;*)
(*val nth : 'a list -> int -> 'a;;*)
(*val extract_string : string -> int -> int -> string;;*)
(*val string_of_bool : bool -> string;;*)
(*val bool_of_string : string -> bool;;*)
(*val index_string : string -> string -> int;;*)
(*val words : string -> string list;;*)
(*val message : string -> unit;;*)
(*val graphics_directory : string ref;;*)
(*val Graphics_lib_directory : string ref;;*)
(*val Header_lib_directory : string ref;;*)
(*val Font_lib_directory : string ref;;*)
(*val Bin_lib_directory : string ref;;*)


val change_graphics_directory : string -> unit;;
val directory_concat_string : string ref;;

(*val adobe_version : string ref;;*)
(*val MLgraph_version : string ref;;*)
(*val begin_prelude1 : string ref;;*)
(*val begin_prelude2 : string ref;;*)
(*val begin_prelude3 : string ref;;*)
(*val end_prelude : string ref;;*)
(*val body_prelude : string list ref;;*)

val modify_body_prelude : string list -> unit;;

(*val input_line : in_channel -> string;;*)
(*val output_line : out_channel -> string -> unit;;*)
(*val hd : 'a list -> 'a;;*)
(*val tl : 'a list -> 'a list;;*)
(*val last : 'a list -> 'a;;*)
(*val iterate : ('a -> 'a) -> int -> 'a -> 'a;;*)
(* END PRELUDE *)


(* BEGIN GEOMETRY  *)

type point = 
  { xc : float
  ; yc : float
  }
;;

type geom_element = 
    Seg of point list
  | Arc of point * float * float * float
  | Curve of point * point * point * point
;;

type transformation = 
  { m11 : float
  ; m12 : float
  ; m13 : float
  ; m21 : float
  ; m22 : float
  ; m23 : float
  }
;;


val make_point : float * float -> point;;
val origin : point;;
val make_transformation : float * float * float * float * float * float -> transformation;;
val id_trans : transformation;;
val transform_point : transformation -> point -> point;;
val compose_transformation : transformation -> transformation -> transformation;;
val compose_transformations : transformation list -> transformation;;
val ctrans : transformation -> transformation -> transformation;;
val inverse_transformation : transformation -> transformation;;
val handle_transform : point * point -> point * point -> transformation;;
val translation : float * float -> transformation;;
val origin_rotation : float -> transformation;;
val rotation : point -> float -> transformation;;
val scaling : float * float -> transformation;;
val symmetry : float * float -> transformation;;
val vsymmetry : float -> transformation;;
val hsymmetry : float -> transformation;;
val line_symmetry : point * point -> transformation;;
val point_symmetry : point -> transformation;;

(* END GEOMETRY *)


(* BEGIN PAINT *)
type color = Rgb of float * float * float
           | Hsb of float * float * float
           | Gra of float;;

type linecap = Buttcap | Squarecap | Roundcap;;

type linejoin = Beveljoin | Roundjoin | Miterjoin;;

type linestyle = {linewidth:float;
                  linecap:linecap;
                  linejoin:linejoin;
                  dashpattern:int list};;

type fillstyle = Nzfill | Eofill;;
type clipstyle  = Nzclip | Eoclip;;




val black : color;;
val white : color;;
val red : color;;
val green : color;;
val blue : color;;
val yellow : color;;
val cyan : color;;
val magenta : color;;

(* END PAINT *)

(* BEGIN FRAMES *)

type frame = {mutable xmin:float; mutable xmax:float;  
              mutable ymin:float; mutable ymax:float};;  

type extension = All_ext | Horiz_ext | Vertic_ext | Left_ext
               | Right_ext | Top_ext | Bottom_ext;;

val frame_center : frame -> point;;
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

(* END FRAMES *)

(* BEGIN SKETCHES *)
type path;;
type sketch;;

val recompute_sketch_hull : sketch -> sketch;;
val sketch_frame : sketch -> frame;;
val has_exact_frame_sketch : sketch -> bool;;
val sketch_hull : sketch -> point list;;
val sketch_center : sketch -> point;;
val sketch_height : sketch -> float;;
val sketch_width : sketch -> float;;
val compute_size : geom_element list -> int;;
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

(* END SKETCHES *)


(* BEGIN BITMAPS *)
type bitmap ;;

val bitmap_width : bitmap -> int;;
val bitmap_height : bitmap -> int;;
val bitmap_depth : bitmap -> int;;
val create_bitmap : int -> int -> int -> bitmap;;

(*val ascii_code : char -> int;;*)
(*val ascii_0 : int;;*)
(*val ascii_9 : int;;*)
(*val ascii_a : int;;*)
(*val ascii_f : int;;*)
(*val ascii_A : int;;*)
(*val ascii_F : int;;*)
(*val conv_four_bits : int -> int;;*)
(*val iconv_four_bits : int -> int;;*)
(*val nth_conv : int * string -> int;;*)
(*val set_nth : int * string * int -> unit;;*)
(*val char_map_bitmap : (int -> int) -> bitmap -> bitmap;;*)
val sub_bitmap : bitmap -> int * int -> int * int -> bitmap;;
val copy_bitmap : bitmap -> bitmap;;
(*val mask0001 : int;;*)
(*val mask0010 : int;;*)
(*val mask0100 : int;;*)
(*val mask1000 : int;;*)
(*val mask0011 : int;;*)
(*val mask1100 : int;;*)
(*val mask1110 : int;;*)
(*val mask1101 : int;;*)
(*val mask1011 : int;;*)
(*val mask0111 : int;;*)
(*val mask1111 : int;;*)
(*val lnot : int -> int;;*)

val invert_bitmap : bitmap -> bitmap;;

(*val lshift : int * int -> int;;*)
(*val val1 : string -> int -> int -> int;;*)
(*val val2 : string -> int -> int -> int;;*)
(*val val4 : string -> int -> int;;*)
(*val val8 : string -> int -> int;;*)
(*val val16 : string -> int -> int;;*)
(*val change_val1 : string -> int -> int -> int -> unit;;*)
(*val change_val2 : string -> int -> int -> int -> unit;;*)
(*val change_val4 : string -> int -> int -> unit;;*)
(*val change_val8 : string -> int -> int -> unit;;*)
(*val change_val16 : string -> int -> int -> unit;;*)
(*val set_pixel1 : bitmap -> int -> int -> int -> unit;;*)
(*val set_pixel2 : bitmap -> int -> int -> int -> unit;;*)
(*val set_pixel4 : bitmap -> int -> int -> int -> unit;;*)
(*val set_pixel8 : bitmap -> int -> int -> int -> unit;;*)
(*val set_pixel16 : bitmap -> int -> int -> int -> unit;;*)
val set_pixel : bitmap -> int -> int -> int -> unit;;
(*val get_pixel1 : bitmap -> int -> int -> int;;*)
(*val get_pixel2 : bitmap -> int -> int -> int;;*)
(*val get_pixel4 : bitmap -> int -> int -> int;;*)
(*val get_pixel8 : bitmap -> int -> int -> int;;*)
(*val get_pixel16 : bitmap -> int -> int -> int;;*)
val get_pixel : bitmap -> int -> int -> int;;
(*val map_hexabyte1 : (int -> int) -> int -> int;;*)
(*val map_hexabyte2 : (int -> int) -> int -> int;;*)
(*val map_bitmap8 : (int -> int) -> bitmap -> bitmap;;*)

val convert_bitmap : int * (int -> int) -> bitmap -> bitmap;;
val map_bitmap : (int -> int) -> bitmap -> bitmap;;
val read_bitmap : int -> string -> bitmap;;
val write_bitmap : bitmap -> string -> unit;;
val bitmap_frame : bitmap -> frame;;
val bitmap_hull : bitmap -> point list;;

(* END BITMAPS *)

(* BEGIN FONTS *)
type font_style = Courier
                | Courier_Oblique
                | Courier_Bold
                | Courier_BoldOblique
                | Times_Roman
                | Times_Bold
                | Times_Italic  
                | Times_BoldItalic
                | Helvetica
                | Helvetica_Bold
                | Helvetica_Oblique
                | Helvetica_BoldOblique
                | Symbol
                | Other_font_style of string
;;

type font = { font_style : font_style ;  font_size : float }
;;

type font_description =
     { font_descr_filename : string;
       mutable font_descr_name : string ; 
       mutable font_descr_height : float ; 
       mutable font_descr_width : float ;
       mutable font_descr_descr : float vect;
       mutable font_descr_descr_bbox : ((float * float ) * (float * float)) vect }
;;


(*val Courier_descr : font_description;;*)
(*val Courier_Bold_descr : font_description;;*)
(*val Courier_Oblique_descr : font_description;;*)
(*val Courier_BoldOblique_descr : font_description;;*)
(*val Times_Roman_descr : font_description;;*)
(*val Times_Bold_descr : font_description;;*)
(*val Times_Italic_descr : font_description;;*)
(*val Times_BoldItalic_descr : font_description;;*)
(*val Helvetica_descr : font_description;;*)
(*val Helvetica_Bold_descr : font_description;;*)
(*val Helvetica_Oblique_descr : font_description;;*)
(*val Helvetica_BoldOblique_descr : font_description;;*)
(*val Symbol_descr : font_description;;*)
(*exception Find of int*)
(*;;*)
(*val pos_char_in_string : string -> char -> int -> int -> int;;*)
(*val floatpair_of_string : string -> float * float;;*)
(*val bbox_of_string : string -> (float * float) * (float * float);;*)
(*val load_font : string -> font_description;;*)

(*type text = *)
(*  { t_string : string*)
(*  ; t_font : font*)
(*  }*)
(*;;*)

(*val font_list : (font_style * font_description) list ref;;*)

val add_font : font_style * font_description -> unit;;
val remove_font : font_style * font_description -> unit;;
val print_font_list : unit -> unit;;
val print_info_font : font_description -> unit;;
val print_info_all_fonts : unit -> unit;;

(*val find_font_description : font -> font_description;;*)
(*val value_default_font : font;;*)
(*val gs_default_font : font ref;;*)
val default_font : unit -> font;;
val set_default_font : font -> unit;;
val reset_default_font : unit -> unit;;
val make_font : font_style -> float -> font;;

(* END FONTS *)

(* BEGIN TEXTS *)
(*val make_text : string -> font -> text;;*)
(*val make_default_text : string -> text;;*)
(*val change_size_text : text -> float -> text;;*)
(*val change_font_text : text -> font -> text;;*)
(*val full_char_width : font -> int -> float;;*)

val text_frame : font -> string -> frame;;
val text_width : font -> string -> float;;
val text_height : font -> string -> float;;
(* END TEXTS *)

(* BEGIN GRAPHICS_DEFAULTS *)
(*type graphic_state = *)
(*  { mutable gs_color : color*)
(*  ; mutable gs_linewidthcoef : float*)
(*  ; mutable gs_linecap : linecap*)
(*  ; mutable gs_linejoin : linejoin*)
(*  ; mutable gs_dashpattern : int list*)
(*  ; mutable gs_closed_sketch : bool*)
(*  ; mutable gs_fillstyle : fillstyle*)
(*  ; mutable gs_miterlimit : float*)
(*  }*)
(*;;*)

(* val default_graphic_state : graphic_state;; *)
val default_linewidthcoef : unit -> float;;
val default_linecap : unit -> linecap;;
val default_linejoin : unit -> linejoin;;
val default_dashpattern : unit -> int list;;
val default_color : unit -> color;;
val default_closed_sketch : unit -> bool;;
val default_fillstyle : unit -> fillstyle;;
val default_miterlimit : unit -> float;;
val default_linestyle : frame -> linestyle;;
val set_default_linewidthcoef : float -> unit;;
val set_default_color : color -> unit;;
val set_default_closed_sketch : bool -> unit;;
val set_default_fillstyle : fillstyle -> unit;;
val set_default_linecap : linecap -> unit;;
val set_default_linejoin : linejoin -> unit;;
val set_default_dashpattern : int list -> unit;;
val set_default_miterlimit : float -> unit;;
(* END GRAPHICS_DEFAULTS *)



(* BEGIN PICTURES *)
type interface = 
    No_handle
  | One_handle of point * point
  | Handles of (string * (point * point)) list
;;



(*
type pict = 
    Blank of frame
  | Draw of path * linestyle * color * bool * int
  | Fill of path * fillstyle * color * int
  | Clip of clipstyle * path * pict * int
  | Bitmap of bitmap
  | Bitmapmask of bitmap * color * bool
  | Text of text * color
  | Tpict of transformation * pict
  | Cpict of pict * pict
;;
*)

type picture;;

type alignment = 
    Align_Right
  | Align_Left
  | Align_Center
  | Align_Top
  | Align_Bottom
;;


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
val make_frame_picture : linestyle * color -> frame -> picture;;
val make_default_frame_picture : frame -> picture;;
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
val scale_and_center_picture : float * float -> picture -> picture;;
val translate_picture : float * float -> picture -> picture;;
val translate : float * float -> picture -> picture;;
val rotate_picture : float -> picture -> picture;;
val rotate : float -> picture -> picture;;
val scale_picture : float * float -> picture -> picture;;
val scale : float * float -> picture -> picture;;
val fit_picture_in_frame : picture -> frame -> picture;;
val fit_in_frame : picture -> frame -> picture;;
val force_picture_in_frame : frame -> picture -> picture;;
val force_in_frame : frame -> picture -> picture;;
val add_frame_to_picture : picture -> picture;;
val add_frame : picture -> picture;;
val vflip_picture : picture -> picture;;
val vflip : picture -> picture;;
val hflip_picture : picture -> picture;;
val hflip : picture -> picture;;
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
val circletexttop : font -> string -> point -> float -> picture;;
val circletextbottom : font -> string -> point -> float -> picture;;

(* END PICTURES *)



(* BEGIN TRPS *)

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
(*val display_path : bool -> path -> unit;;*)
(*val special_display_path : path -> string -> unit;;*)
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
(*val display_picture : picture -> unit;;*)
(*type page_defaults = *)
(*  { mutable page_height : float*)
(*  ; mutable page_width : float*)
(*  ; mutable page_hmargin : float*)
(*  ; mutable page_vmargin : float*)
(*  }*)
(*;;*)
(*val default_page : page_defaults;;*)

(* END TRPS *)


(* BEGIN DISPLAY *)

val page_height : unit -> float;;
val page_width : unit -> float;;
val page_hmargin : unit -> float;;
val page_vmargin : unit -> float;;
val set_page_height : float -> unit;;
val set_page_width : float -> unit;;
val set_page_hmargin : float -> unit;;
val set_page_vmargin : float -> unit;;
val compute_display_frame : picture -> frame;;
val center_and_adjust_picture : picture -> picture;;
val center_and_adjust : picture -> picture;;
(*val make_ps_file : picture -> string -> bool -> unit;;*)
val set_frame_extension_coef : float -> unit;;
val ps_file : picture -> string -> unit;;
val eps_file : picture -> string -> unit;;
val ps : picture -> unit;;
val eps : picture -> unit;;

(* END DISPLAY *)




(* BEGIN OPTION *)

type option = 
   SOption of string 
 | IOption of int 
 | FOption of float
 | BOption of bool 
 | POption
 | COption of color
 | LOption of string*float
 | DOption of int list
 | JOption of linejoin
 | CapOption of linecap
 | FillOption of fillstyle
 | FontOption of font_style;;


val string : 'a -> string -> 'a * option;;
val bool : 'a -> bool -> 'a * option;;
val int : 'a -> int -> 'a * option;;
val float : 'a -> float -> 'a * option;;
val option : 'a -> 'a * option;;
val color : 'a -> color -> 'a * option;;
val dashPattern : int list -> string * option;;
val join : linejoin -> string * option;;
val cap : linecap -> string * option;;
val fillStyle : fillstyle -> string * option;;
val lineLabel : string -> float -> string * option;;
val font : font_style -> string * option;;
(*exception OptionError;;*)


val theString : ('a * option) list -> 'a -> string -> string;;
val theInt : ('a * option) list -> 'a -> int -> int;;
val theFloat : ('a * option) list -> 'a -> float -> float;;
val theBool : ('a * option) list -> 'a -> bool -> bool;;
val theOption : ('a * 'b) list -> 'a -> bool;;
val theColor : ('a * option) list -> 'a -> color -> color;;
val theDashPattern : (string * option) list -> int list -> int list;;
val theLineJoin : (string * option) list -> linejoin -> linejoin;;
val theLineCap : (string * option) list -> linecap -> linecap;;
val theFillStyle : (string * option) list -> fillstyle -> fillstyle;;
val theLineLabel : (string * option) list -> option;;
val theFont : (string * option) list -> font_style -> font_style;;
val findOption : ('a * 'b) list -> 'a -> 'b;;

(* END OPTION *)

(* BEGIN GRAPH *)

type dir = Dn | Ds | De | Dw | Dne | Dnw | Dse | Dsw | Deg of float;;

type graph = 
    Graph of string
  | PGraph of graph*(string*point)list
  | LGraph of graph*((string * option) list * geom_element list list) list
  | TGraph of transformation*graph
  | CGraph of graph*graph;;
type line =
    GLine  of (string*option) list*string list
  | GCurve of (string*option) list*(string*dir) list
  | GSCurve of (string*option) list*string*(float*string)list
  | GHull of (string*option) list*string list;;
type tabPos = V of float | L of string;;
type tabStyle = Center|Left|Right;;

type pos = AnyPos | RelPos of int;;

(*
val fold : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list;;
val try_find : ('a -> 'b) -> 'a list -> 'b;;
val nth : int -> 'a list -> 'a;;
val substituteNth : int -> ('a -> 'a) -> 'a list -> 'a list;;
val sqr : float -> float;;
val pSub : point -> point -> point;;
val pAdd : point -> point -> point;;
val pMult : point -> float -> point;;
val pi : float;;
*)
val circlePoint : float -> float -> point;;
val lengthOfLine : point * point -> float;;
val slopeOfLine : point * point -> float;;
val sketchGen : (string * option) list -> float -> sketch -> picture;;
val sketch : float -> sketch -> picture;;
val curvePos : point * point * point * point -> float -> point * float;;
val arrowFormGen : (string * option) list -> point * float -> string -> geom_element list list;;
val textGen : (string * option) list -> string -> picture;;
val text : string -> picture;;
val diagOfFrame : frame -> float;;
val blankSketch : (string * option) list -> sketch -> picture;;
val rectOfFrame : (string * option) list -> frame -> picture;;
val rectangleGen : (string * option) list -> picture -> picture;;
val rectangle : picture -> picture;;
val circOfFrame : (string * option) list -> frame -> picture;;
val circleGen : (string * option) list -> picture -> picture;;
val circle : picture -> picture;;
val ovalOfFrame : (string * option) list -> frame -> picture;;
val ovalGen : (string * option) list -> picture -> picture;;
val oval : picture -> picture;;
val linesOfPoints : (string * option) list -> point list -> ((string * option) list * geom_element list list) list;;
val curveOfPoints : (string * option) list -> (point * float) list -> ((string * option) list * geom_element list list) list;;
val symmetricCurvesOfPoints : (string * option) list -> point * (float * point) list -> ((string * option) list * geom_element list list) list;;
val hullOfPoints : (string * option) list -> point list -> ((string * option) list * geom_element list list) list;;
val degOfDir : dir -> float;;
val transformGraph : transformation -> graph -> graph;;
val graphPoint : string -> graph -> point;;
val graphLineLabel : float * float -> graph -> string -> point;;
val nodeGraph : string -> graph;;
val addLines : graph -> line list -> graph;;
val addPoints : (string * option) list -> graph -> (string * (string * float)) list -> graph;;
val polyGraph : string -> string list -> line list -> graph;;
val tabularGraph : string -> tabStyle -> tabPos list list -> line list -> graph;;
val linkGraphs : graph * string -> graph * string -> line list -> graph;;
val composeGraphs : graph * string * string -> graph * string * string -> line list -> graph;;
val insLineGen : (graph * pos) list -> (string * option) list * string * dir * string -> (graph * pos) list * line;;
val assembleGraphs : graph list -> string list -> ((string * option) list * string * dir * string) list -> graph;;
val pictOfLines : transformation -> float -> (string * option) list -> ((string * option) list * geom_element list list) list -> picture list;;
val skeletonOfGraphGen : (string * option) list -> transformation * float -> graph -> picture list;;
val graphGen : (string * option) list -> graph -> (string * picture) list -> picture;;
val graph : graph -> (string * picture) list -> picture;;

(* END GRAPH *)

(* BEGIN TREE *)

type ('a,'b) tree = Node of ('a,'b) treeRecord | Nil
 and ('a,'b) treeRecord = {info:'a;sons:('a,'b) tree list;label:'b label}
 and 'b label = Nolabel | Label of 'b;;

val tree_it : ('a -> 'b -> 'c label -> 'b) -> ('a, 'c) tree -> 'b -> 'b;;
val tree_map : ('a -> 'b) -> ('a, 'c) tree -> ('b, 'c) tree;;
val drawTree : (string * option) list -> ('a -> picture) * (point * point -> 'b -> picture) -> float * float * float list * point -> ('a, 'b) tree -> picture;;
val widthOfInfo : (picture, 'a) tree -> float;;
val drawProofTree : (string * option) list -> float -> float * float * float list * point -> (picture, picture) tree -> picture;;
(*
val minl : float list list -> float list;;
val recomputeTriples : float list -> (float * float * float) list -> (float * float * float) list;;
val computeHeadCoef : ('a * float * float) list * (float * 'b * 'c) list -> float;;
val combineTriples : float -> float -> (float * float * 'a) list * (float * float * 'a) list -> (float * float * 'a) list;;
val scaleTriple : float -> float * float * float -> float * float * float;;
val computeCoefList : ('a, 'b) tree -> float list;;
*)
val makeTreePictureGen : (string * option) list -> ('a -> picture) * (point * point -> 'b -> picture) -> float * float * point -> ('a, 'b) tree -> picture;;
val makeTreePicture : ('a -> picture) * (point * point -> 'b -> picture) -> float * float * point -> ('a, 'b) tree -> picture;;
val treeLabelPos : float * float -> point * point -> point;;
val treeGen : (string * option) list -> (picture, picture) tree -> picture;;
val tree : (picture, picture) tree -> picture;;
val makeProofTreePictureGen : (string * option) list -> ('a -> picture) -> float -> float * float * point -> ('a, picture) tree -> picture;;
val makeProofTreePicture : ('a -> picture) -> float -> float * float * point -> ('a, picture) tree -> picture;;
val proofTreeGen : (string * option) list -> (picture, picture) tree -> picture;;
val proofTree : (picture, picture) tree -> picture;;
val treeGraphGen : (string * option) list -> string -> (string, 'a) tree -> line list -> graph;;
val treeGraph : string -> (string, 'a) tree -> line list -> graph;;

(* END TREE *)

(* BEGIN MLGLATEX *)

(*
val oRotation : float -> transformation
val arrowSketch : sketch
*)
val arrowPict : picture
val picDir : string ref
(*
val transOfPict :
  picture -> picture -> transformation list
val cvw : string -> float
val fEq : float -> float -> bool
val stringTexOfFloat : float -> string
*)
val colorPict : float -> float -> float -> color  -> color -> picture;;
val latexPictureGen :
  (string * option) list ->
  'a * 'b * string * string * string -> 'a * (picture * 'b);;
val latexPicture :
  'a * 'b * string * string * string -> 'a * (picture * 'b);;
val inverseTransAngle : transformation -> transformation;;
val makeLatexPicture :
  picture -> ('a * (picture * int)) list -> string -> unit;;
val latexBoxTable : (string * (picture * int)) list ref;;
val add_latexBox : string * int * string * string * string -> unit;;
val empty_latexBoxTable : unit -> unit;;
val latexBox : string -> picture;;
val latexBoxGen : (string * option) list -> string -> picture;;
val latex_mode : unit -> bool;;
val set_latex_mode : unit -> unit;;

(* END MLGLATEX *)


