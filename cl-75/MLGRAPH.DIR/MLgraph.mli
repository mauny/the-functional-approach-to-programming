

(* BEGIN PRELUDE *)
(*value o : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b;;*)
(*value min_int : int -> int -> int;;*)
(*value max_int : int -> int -> int;;*)
(*value max : float -> float -> float;;*)
(*value min : float -> float -> float;;*)
(*value Pi : float;;*)
(*value cva : float -> float;;*)
value sinus : float -> float;;
value cosinus : float -> float;;
(*value explode : string -> string list;;*)
(*value explode_ascii : string -> int list;;*)
(*value nth_ascii : int * string -> int;;*)
(*value set_nth_ascii : int * string * int -> unit;;*)
(*value ascii : int -> string;;*)
(*value nth : 'a list -> int -> 'a;;*)
(*value extract_string : string -> int -> int -> string;;*)
(*value string_of_bool : bool -> string;;*)
(*value bool_of_string : string -> bool;;*)
(*value index_string : string -> string -> int;;*)
(*value words : string -> string list;;*)
(*value message : string -> unit;;*)
(*value graphics_directory : string ref;;*)
(*value Graphics_lib_directory : string ref;;*)
(*value Header_lib_directory : string ref;;*)
(*value Font_lib_directory : string ref;;*)
(*value Bin_lib_directory : string ref;;*)


value change_graphics_directory : string -> unit;;
value directory_concat_string : string ref;;

(*value adobe_version : string ref;;*)
(*value MLgraph_version : string ref;;*)
(*value begin_prelude1 : string ref;;*)
(*value begin_prelude2 : string ref;;*)
(*value begin_prelude3 : string ref;;*)
(*value end_prelude : string ref;;*)
(*value body_prelude : string list ref;;*)

value modify_body_prelude : string list -> unit;;

(*value input_line : in_channel -> string;;*)
(*value output_line : out_channel -> string -> unit;;*)
(*value hd : 'a list -> 'a;;*)
(*value tl : 'a list -> 'a list;;*)
(*value last : 'a list -> 'a;;*)
(*value iterate : ('a -> 'a) -> int -> 'a -> 'a;;*)
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


value make_point : float * float -> point;;
value origin : point;;
value make_transformation : float * float * float * float * float * float -> transformation;;
value id_trans : transformation;;
value transform_point : transformation -> point -> point;;
value compose_transformation : transformation -> transformation -> transformation;;
value compose_transformations : transformation list -> transformation;;
value ctrans : transformation -> transformation -> transformation;;
value inverse_transformation : transformation -> transformation;;
value handle_transform : point * point -> point * point -> transformation;;
value translation : float * float -> transformation;;
value origin_rotation : float -> transformation;;
value rotation : point -> float -> transformation;;
value scaling : float * float -> transformation;;
value symmetry : float * float -> transformation;;
value vsymmetry : float -> transformation;;
value hsymmetry : float -> transformation;;
value line_symmetry : point * point -> transformation;;
value point_symmetry : point -> transformation;;

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




value black : color;;
value white : color;;
value red : color;;
value green : color;;
value blue : color;;
value yellow : color;;
value cyan : color;;
value magenta : color;;

(* END PAINT *)

(* BEGIN FRAMES *)

type frame = {mutable xmin:float; mutable xmax:float;  
              mutable ymin:float; mutable ymax:float};;  

type extension = All_ext | Horiz_ext | Vertic_ext | Left_ext
               | Right_ext | Top_ext | Bottom_ext;;

value frame_center : frame -> point;;
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

(* END FRAMES *)

(* BEGIN SKETCHES *)
type path;;
type sketch;;

value recompute_sketch_hull : sketch -> sketch;;
value sketch_frame : sketch -> frame;;
value has_exact_frame_sketch : sketch -> bool;;
value sketch_hull : sketch -> point list;;
value sketch_center : sketch -> point;;
value sketch_height : sketch -> float;;
value sketch_width : sketch -> float;;
value compute_size : geom_element list -> int;;
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

(* END SKETCHES *)


(* BEGIN BITMAPS *)
type bitmap ;;

value bitmap_width : bitmap -> int;;
value bitmap_height : bitmap -> int;;
value bitmap_depth : bitmap -> int;;
value create_bitmap : int -> int -> int -> bitmap;;

(*value ascii_code : char -> int;;*)
(*value ascii_0 : int;;*)
(*value ascii_9 : int;;*)
(*value ascii_a : int;;*)
(*value ascii_f : int;;*)
(*value ascii_A : int;;*)
(*value ascii_F : int;;*)
(*value conv_four_bits : int -> int;;*)
(*value iconv_four_bits : int -> int;;*)
(*value nth_conv : int * string -> int;;*)
(*value set_nth : int * string * int -> unit;;*)
(*value char_map_bitmap : (int -> int) -> bitmap -> bitmap;;*)
value sub_bitmap : bitmap -> int * int -> int * int -> bitmap;;
value copy_bitmap : bitmap -> bitmap;;
(*value mask0001 : int;;*)
(*value mask0010 : int;;*)
(*value mask0100 : int;;*)
(*value mask1000 : int;;*)
(*value mask0011 : int;;*)
(*value mask1100 : int;;*)
(*value mask1110 : int;;*)
(*value mask1101 : int;;*)
(*value mask1011 : int;;*)
(*value mask0111 : int;;*)
(*value mask1111 : int;;*)
(*value lnot : int -> int;;*)

value invert_bitmap : bitmap -> bitmap;;

(*value lshift : int * int -> int;;*)
(*value val1 : string -> int -> int -> int;;*)
(*value val2 : string -> int -> int -> int;;*)
(*value val4 : string -> int -> int;;*)
(*value val8 : string -> int -> int;;*)
(*value val16 : string -> int -> int;;*)
(*value change_val1 : string -> int -> int -> int -> unit;;*)
(*value change_val2 : string -> int -> int -> int -> unit;;*)
(*value change_val4 : string -> int -> int -> unit;;*)
(*value change_val8 : string -> int -> int -> unit;;*)
(*value change_val16 : string -> int -> int -> unit;;*)
(*value set_pixel1 : bitmap -> int -> int -> int -> unit;;*)
(*value set_pixel2 : bitmap -> int -> int -> int -> unit;;*)
(*value set_pixel4 : bitmap -> int -> int -> int -> unit;;*)
(*value set_pixel8 : bitmap -> int -> int -> int -> unit;;*)
(*value set_pixel16 : bitmap -> int -> int -> int -> unit;;*)
value set_pixel : bitmap -> int -> int -> int -> unit;;
(*value get_pixel1 : bitmap -> int -> int -> int;;*)
(*value get_pixel2 : bitmap -> int -> int -> int;;*)
(*value get_pixel4 : bitmap -> int -> int -> int;;*)
(*value get_pixel8 : bitmap -> int -> int -> int;;*)
(*value get_pixel16 : bitmap -> int -> int -> int;;*)
value get_pixel : bitmap -> int -> int -> int;;
(*value map_hexabyte1 : (int -> int) -> int -> int;;*)
(*value map_hexabyte2 : (int -> int) -> int -> int;;*)
(*value map_bitmap8 : (int -> int) -> bitmap -> bitmap;;*)

value convert_bitmap : int * (int -> int) -> bitmap -> bitmap;;
value map_bitmap : (int -> int) -> bitmap -> bitmap;;
value read_bitmap : int -> string -> bitmap;;
value write_bitmap : bitmap -> string -> unit;;
value bitmap_frame : bitmap -> frame;;
value bitmap_hull : bitmap -> point list;;

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


(*value Courier_descr : font_description;;*)
(*value Courier_Bold_descr : font_description;;*)
(*value Courier_Oblique_descr : font_description;;*)
(*value Courier_BoldOblique_descr : font_description;;*)
(*value Times_Roman_descr : font_description;;*)
(*value Times_Bold_descr : font_description;;*)
(*value Times_Italic_descr : font_description;;*)
(*value Times_BoldItalic_descr : font_description;;*)
(*value Helvetica_descr : font_description;;*)
(*value Helvetica_Bold_descr : font_description;;*)
(*value Helvetica_Oblique_descr : font_description;;*)
(*value Helvetica_BoldOblique_descr : font_description;;*)
(*value Symbol_descr : font_description;;*)
(*exception Find of int*)
(*;;*)
(*value pos_char_in_string : string -> char -> int -> int -> int;;*)
(*value floatpair_of_string : string -> float * float;;*)
(*value bbox_of_string : string -> (float * float) * (float * float);;*)
(*value load_font : string -> font_description;;*)

(*type text = *)
(*  { t_string : string*)
(*  ; t_font : font*)
(*  }*)
(*;;*)

(*value font_list : (font_style * font_description) list ref;;*)

value add_font : font_style * font_description -> unit;;
value remove_font : font_style * font_description -> unit;;
value print_font_list : unit -> unit;;
value print_info_font : font_description -> unit;;
value print_info_all_fonts : unit -> unit;;

(*value find_font_description : font -> font_description;;*)
(*value value_default_font : font;;*)
(*value gs_default_font : font ref;;*)
value default_font : unit -> font;;
value set_default_font : font -> unit;;
value reset_default_font : unit -> unit;;
value make_font : font_style -> float -> font;;

(* END FONTS *)

(* BEGIN TEXTS *)
(*value make_text : string -> font -> text;;*)
(*value make_default_text : string -> text;;*)
(*value change_size_text : text -> float -> text;;*)
(*value change_font_text : text -> font -> text;;*)
(*value full_char_width : font -> int -> float;;*)

value text_frame : font -> string -> frame;;
value text_width : font -> string -> float;;
value text_height : font -> string -> float;;
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

(* value default_graphic_state : graphic_state;; *)
value default_linewidthcoef : unit -> float;;
value default_linecap : unit -> linecap;;
value default_linejoin : unit -> linejoin;;
value default_dashpattern : unit -> int list;;
value default_color : unit -> color;;
value default_closed_sketch : unit -> bool;;
value default_fillstyle : unit -> fillstyle;;
value default_miterlimit : unit -> float;;
value default_linestyle : frame -> linestyle;;
value set_default_linewidthcoef : float -> unit;;
value set_default_color : color -> unit;;
value set_default_closed_sketch : bool -> unit;;
value set_default_fillstyle : fillstyle -> unit;;
value set_default_linecap : linecap -> unit;;
value set_default_linejoin : linejoin -> unit;;
value set_default_dashpattern : int list -> unit;;
value set_default_miterlimit : float -> unit;;
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
value make_frame_picture : linestyle * color -> frame -> picture;;
value make_default_frame_picture : frame -> picture;;
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
value scale_and_center_picture : float * float -> picture -> picture;;
value translate_picture : float * float -> picture -> picture;;
value translate : float * float -> picture -> picture;;
value rotate_picture : float -> picture -> picture;;
value rotate : float -> picture -> picture;;
value scale_picture : float * float -> picture -> picture;;
value scale : float * float -> picture -> picture;;
value fit_picture_in_frame : picture -> frame -> picture;;
value fit_in_frame : picture -> frame -> picture;;
value force_picture_in_frame : frame -> picture -> picture;;
value force_in_frame : frame -> picture -> picture;;
value add_frame_to_picture : picture -> picture;;
value add_frame : picture -> picture;;
value vflip_picture : picture -> picture;;
value vflip : picture -> picture;;
value hflip_picture : picture -> picture;;
value hflip : picture -> picture;;
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
value circletexttop : font -> string -> point -> float -> picture;;
value circletextbottom : font -> string -> point -> float -> picture;;

(* END PICTURES *)



(* BEGIN TRPS *)

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
(*value display_path : bool -> path -> unit;;*)
(*value special_display_path : path -> string -> unit;;*)
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
(*value display_picture : picture -> unit;;*)
(*type page_defaults = *)
(*  { mutable page_height : float*)
(*  ; mutable page_width : float*)
(*  ; mutable page_hmargin : float*)
(*  ; mutable page_vmargin : float*)
(*  }*)
(*;;*)
(*value default_page : page_defaults;;*)

(* END TRPS *)


(* BEGIN DISPLAY *)

value page_height : unit -> float;;
value page_width : unit -> float;;
value page_hmargin : unit -> float;;
value page_vmargin : unit -> float;;
value set_page_height : float -> unit;;
value set_page_width : float -> unit;;
value set_page_hmargin : float -> unit;;
value set_page_vmargin : float -> unit;;
value compute_display_frame : picture -> frame;;
value center_and_adjust_picture : picture -> picture;;
value center_and_adjust : picture -> picture;;
(*value make_ps_file : picture -> string -> bool -> unit;;*)
value set_frame_extension_coef : float -> unit;;
value ps_file : picture -> string -> unit;;
value eps_file : picture -> string -> unit;;
value ps : picture -> unit;;
value eps : picture -> unit;;

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


value string : 'a -> string -> 'a * option;;
value bool : 'a -> bool -> 'a * option;;
value int : 'a -> int -> 'a * option;;
value float : 'a -> float -> 'a * option;;
value option : 'a -> 'a * option;;
value color : 'a -> color -> 'a * option;;
value dashPattern : int list -> string * option;;
value join : linejoin -> string * option;;
value cap : linecap -> string * option;;
value fillStyle : fillstyle -> string * option;;
value lineLabel : string -> float -> string * option;;
value font : font_style -> string * option;;
(*exception OptionError;;*)


value theString : ('a * option) list -> 'a -> string -> string;;
value theInt : ('a * option) list -> 'a -> int -> int;;
value theFloat : ('a * option) list -> 'a -> float -> float;;
value theBool : ('a * option) list -> 'a -> bool -> bool;;
value theOption : ('a * 'b) list -> 'a -> bool;;
value theColor : ('a * option) list -> 'a -> color -> color;;
value theDashPattern : (string * option) list -> int list -> int list;;
value theLineJoin : (string * option) list -> linejoin -> linejoin;;
value theLineCap : (string * option) list -> linecap -> linecap;;
value theFillStyle : (string * option) list -> fillstyle -> fillstyle;;
value theLineLabel : (string * option) list -> option;;
value theFont : (string * option) list -> font_style -> font_style;;
value findOption : ('a * 'b) list -> 'a -> 'b;;

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
value fold : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list;;
value try_find : ('a -> 'b) -> 'a list -> 'b;;
value nth : int -> 'a list -> 'a;;
value substituteNth : int -> ('a -> 'a) -> 'a list -> 'a list;;
value sqr : float -> float;;
value pSub : point -> point -> point;;
value pAdd : point -> point -> point;;
value pMult : point -> float -> point;;
value pi : float;;
*)
value circlePoint : float -> float -> point;;
value lengthOfLine : point * point -> float;;
value slopeOfLine : point * point -> float;;
value sketchGen : (string * option) list -> float -> sketch -> picture;;
value sketch : float -> sketch -> picture;;
value curvePos : point * point * point * point -> float -> point * float;;
value arrowFormGen : (string * option) list -> point * float -> string -> geom_element list list;;
value textGen : (string * option) list -> string -> picture;;
value text : string -> picture;;
value diagOfFrame : frame -> float;;
value blankSketch : (string * option) list -> sketch -> picture;;
value rectOfFrame : (string * option) list -> frame -> picture;;
value rectangleGen : (string * option) list -> picture -> picture;;
value rectangle : picture -> picture;;
value circOfFrame : (string * option) list -> frame -> picture;;
value circleGen : (string * option) list -> picture -> picture;;
value circle : picture -> picture;;
value ovalOfFrame : (string * option) list -> frame -> picture;;
value ovalGen : (string * option) list -> picture -> picture;;
value oval : picture -> picture;;
value linesOfPoints : (string * option) list -> point list -> ((string * option) list * geom_element list list) list;;
value curveOfPoints : (string * option) list -> (point * float) list -> ((string * option) list * geom_element list list) list;;
value symmetricCurvesOfPoints : (string * option) list -> point * (float * point) list -> ((string * option) list * geom_element list list) list;;
value hullOfPoints : (string * option) list -> point list -> ((string * option) list * geom_element list list) list;;
value degOfDir : dir -> float;;
value transformGraph : transformation -> graph -> graph;;
value graphPoint : string -> graph -> point;;
value graphLineLabel : float * float -> graph -> string -> point;;
value nodeGraph : string -> graph;;
value addLines : graph -> line list -> graph;;
value addPoints : (string * option) list -> graph -> (string * (string * float)) list -> graph;;
value polyGraph : string -> string list -> line list -> graph;;
value tabularGraph : string -> tabStyle -> tabPos list list -> line list -> graph;;
value linkGraphs : graph * string -> graph * string -> line list -> graph;;
value composeGraphs : graph * string * string -> graph * string * string -> line list -> graph;;
value insLineGen : (graph * pos) list -> (string * option) list * string * dir * string -> (graph * pos) list * line;;
value assembleGraphs : graph list -> string list -> ((string * option) list * string * dir * string) list -> graph;;
value pictOfLines : transformation -> float -> (string * option) list -> ((string * option) list * geom_element list list) list -> picture list;;
value skeletonOfGraphGen : (string * option) list -> transformation * float -> graph -> picture list;;
value graphGen : (string * option) list -> graph -> (string * picture) list -> picture;;
value graph : graph -> (string * picture) list -> picture;;

(* END GRAPH *)

(* BEGIN TREE *)

type ('a,'b) tree = Node of ('a,'b) treeRecord | Nil
 and ('a,'b) treeRecord = {info:'a;sons:('a,'b) tree list;label:'b label}
 and 'b label = Nolabel | Label of 'b;;

value tree_it : ('a -> 'b -> 'c label -> 'b) -> ('a, 'c) tree -> 'b -> 'b;;
value tree_map : ('a -> 'b) -> ('a, 'c) tree -> ('b, 'c) tree;;
value drawTree : (string * option) list -> ('a -> picture) * (point * point -> 'b -> picture) -> float * float * float list * point -> ('a, 'b) tree -> picture;;
value widthOfInfo : (picture, 'a) tree -> float;;
value drawProofTree : (string * option) list -> float -> float * float * float list * point -> (picture, picture) tree -> picture;;
(*
value minl : float list list -> float list;;
value recomputeTriples : float list -> (float * float * float) list -> (float * float * float) list;;
value computeHeadCoef : ('a * float * float) list * (float * 'b * 'c) list -> float;;
value combineTriples : float -> float -> (float * float * 'a) list * (float * float * 'a) list -> (float * float * 'a) list;;
value scaleTriple : float -> float * float * float -> float * float * float;;
value computeCoefList : ('a, 'b) tree -> float list;;
*)
value makeTreePictureGen : (string * option) list -> ('a -> picture) * (point * point -> 'b -> picture) -> float * float * point -> ('a, 'b) tree -> picture;;
value makeTreePicture : ('a -> picture) * (point * point -> 'b -> picture) -> float * float * point -> ('a, 'b) tree -> picture;;
value treeLabelPos : float * float -> point * point -> point;;
value treeGen : (string * option) list -> (picture, picture) tree -> picture;;
value tree : (picture, picture) tree -> picture;;
value makeProofTreePictureGen : (string * option) list -> ('a -> picture) -> float -> float * float * point -> ('a, picture) tree -> picture;;
value makeProofTreePicture : ('a -> picture) -> float -> float * float * point -> ('a, picture) tree -> picture;;
value proofTreeGen : (string * option) list -> (picture, picture) tree -> picture;;
value proofTree : (picture, picture) tree -> picture;;
value treeGraphGen : (string * option) list -> string -> (string, 'a) tree -> line list -> graph;;
value treeGraph : string -> (string, 'a) tree -> line list -> graph;;

(* END TREE *)

(* BEGIN MLGLATEX *)

(*
value oRotation : float -> transformation
value arrowSketch : sketch
*)
value arrowPict : picture;;
value picDir : string ref;;
(*
value transOfPict :
  picture -> picture -> transformation list
value cvw : string -> float
value fEq : float -> float -> bool
value stringTexOfFloat : float -> string
*)
value colorPict : float -> float -> float -> color -> color -> picture;;
value latexPictureGen :
  (string * option) list ->
  'a * 'b * string * string * string -> 'a * (picture * 'b);;
value latexPicture :
  'a * 'b * string * string * string -> 'a * (picture * 'b);;
value inverseTransAngle : transformation -> transformation;;
value makeLatexPicture :
  picture -> ('a * (picture * int)) list -> string -> unit;;
value latexBoxTable : (string * (picture * int)) list ref;;
value add_latexBox : string * int * string * string * string -> unit;;
value empty_latexBoxTable : unit -> unit;;
value latexBox : string -> picture;;
value latexBoxGen : (string * option) list -> string -> picture;;
value latex_mode : unit -> bool;;
value set_latex_mode : unit -> unit;;
(* END MLGLATEX *)


