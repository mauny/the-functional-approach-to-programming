(* 
 * $Id: MLgraph.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ 
 *
 *)

#open  "prelude";;
#open  "geometry";;
#open  "paint";;
#open  "frames";;
#open  "sketches";;
#open  "bitmaps";;
#open  "fonts";;
#open  "texts";;
#open  "graphics_defaults";;
#open  "pictures";;
#open  "circletext";;
#open  "cps";;
#open  "trps";;
#open  "display";;
#open  "option";;
#open  "graph";;
#open  "tree";;
#open  "mlglatex";;



(* BEGIN PRELUDE *)
let ( sinus) =sinus ;;
let ( cosinus) =cosinus ;;

let ( change_graphics_directory) =change_graphics_directory ;;
let ( directory_concat_string) =directory_concat_string ;;
let ( modify_body_prelude) = modify_body_prelude ;;
(* END PRELUDE *)


(* BEGIN GEOMETRY  *)


let ( make_point : float * float -> point ) = make_point ;;
let ( origin:point ) = origin ;;
let ( make_transformation
 : float * float * float * float * float * float -> transformation 
) = make_transformation ;;
let ( id_trans : transformation ) = id_trans ;;
let ( transform_point : transformation -> point -> point
) = transform_point ;;
let ( compose_transformation
 : transformation -> transformation -> transformation
) = compose_transformation ;;
let ( compose_transformations : transformation list -> transformation
) = compose_transformations ;;
let ( ctrans : transformation -> transformation -> transformation
) = ctrans ;;
let ( inverse_transformation : transformation -> transformation
) = inverse_transformation ;;
let ( handle_transform : point * point -> point * point -> transformation
) = handle_transform ;;
let ( translation : float * float -> transformation ) = translation ;;
let ( origin_rotation : float -> transformation ) = origin_rotation ;;
let ( rotation : point -> float -> transformation ) = rotation ;;
let ( scaling : float * float -> transformation ) = scaling ;;
let ( symmetry : float * float -> transformation ) = symmetry ;;
let ( vsymmetry : float -> transformation ) = vsymmetry ;;
let ( hsymmetry : float -> transformation ) = hsymmetry ;;
let ( line_symmetry : point * point -> transformation ) = line_symmetry ;;
let ( point_symmetry : point -> transformation ) = point_symmetry ;;

(* END GEOMETRY *)


(* BEGIN PAINT *)



let ( black : color ) = black ;;
let ( white : color ) = white ;;
let ( red : color ) = red ;;
let ( green : color ) = green ;;
let ( blue : color ) = blue ;;
let ( yellow : color ) = yellow ;;
let ( cyan : color ) = cyan ;;
let ( magenta : color ) = magenta ;;

(* END PAINT *)

(* BEGIN FRAMES *)



let ( frame_center : frame -> point ) = frame_center ;;
let ( extend_frame : extension -> float -> frame -> frame
) = extend_frame ;;
let ( point_frame : point -> frame ) = point_frame ;;
let ( seg_frame : point list -> frame ) = seg_frame ;;
let ( ordered_angles : float * float * float -> bool
) = ordered_angles ;;
let ( arc_frame : point * float * float * float -> frame
) = arc_frame ;;
let ( curve_frame : point * point * point * point -> frame
) = curve_frame ;;
let ( merge_frames : frame list -> frame ) = merge_frames ;;
let ( compute_geom_elem_frame : geom_element -> frame
) = compute_geom_elem_frame ;;
let ( compute_frame : geom_element list -> frame
) = compute_frame ;;
let ( transform_frame : transformation -> frame -> frame
) = transform_frame ;;
let ( frame_to_frame_transform : frame -> frame -> transformation
) = frame_to_frame_transform ;;

let ( set_exact_frame_mode ) = set_exact_frame_mode;;
let ( get_exact_frame_mode ) = get_exact_frame_mode;;
let ( set_discrete_curve_number ) = set_discrete_curve_number;;
let ( get_discrete_curve_number ) = get_discrete_curve_number;;
let ( set_discrete_circle_number ) = set_discrete_circle_number;;
let ( get_discrete_circle_number ) = get_discrete_circle_number;;
let ( compute_geom_elem_convex_hull : geom_element -> point list
 ) = compute_geom_elem_convex_hull;;
let ( compute_geom_elem_list_convex_hull : geom_element list -> point list
 ) = compute_geom_elem_list_convex_hull;;
let ( merge_convex_hulls : point list list -> point list
 ) = merge_convex_hulls;;
let ( frame_of_convex_hull : point list -> frame
 ) = frame_of_convex_hull;;
let ( convex_hull_of_frame : frame -> point list
 ) = convex_hull_of_frame;;

(* END FRAMES *)

(* BEGIN SKETCHES *)


type path == sketches__path;;
type sketch == sketches__sketch;;


let ( recompute_sketch_hull : sketch -> sketch
 ) = recompute_sketch_hull;;
let ( sketch_frame : sketch -> frame ) = sketch_frame;;
let ( has_exact_frame_sketch : sketch -> bool
 ) = has_exact_frame_sketch;;
let ( sketch_hull : sketch -> point list ) = sketch_hull;;
let ( sketch_center : sketch -> point ) = sketch_center;;
let ( sketch_height : sketch -> float ) = sketch_height;;
let ( sketch_width : sketch -> float ) = sketch_width;;
let ( compute_size : geom_element list -> int ) = compute_size;;
let ( make_sketch : geom_element list -> sketch ) = make_sketch;;
let ( frame_sketch : frame -> sketch ) = frame_sketch;;
let ( hull_sketch : point list -> sketch ) = hull_sketch;;
let ( sketch_center : sketch -> point ) = sketch_center;;


let ( group_sketches : sketch list -> sketch ) = group_sketches ;;
let ( ungroup_sketch : sketch -> sketch list ) = ungroup_sketch ;;
let ( transform_sketch : transformation -> sketch -> sketch
) = transform_sketch ;;
let ( center_sketch : sketch -> point -> sketch ) = center_sketch;;
let ( fit_sketch_in_frame : sketch -> frame -> sketch
) = fit_sketch_in_frame ;;
let ( force_sketch_in_frame : frame -> sketch -> sketch
) = force_sketch_in_frame ;;
let ( scale_sketch : float * float -> sketch -> sketch
) = scale_sketch ;;
let ( scale_and_center_sketch : float * float -> sketch -> sketch
 ) = scale_and_center_sketch;;
let ( translate_sketch : float * float -> sketch -> sketch
) =translate_sketch ;;
let ( vflip_sketch : sketch -> sketch ) = vflip_sketch ;;
let ( hflip_sketch : sketch -> sketch ) = hflip_sketch ;;
let ( rotate_sketch : float -> sketch -> sketch) =rotate_sketch ;;
let ( besides_sketch : sketch -> sketch -> sketch
) = besides_sketch ;;
let ( over_sketch : sketch -> sketch -> sketch
) = over_sketch ;;
(*
let ( BSK) =BSK ;;
let ( OSK) =OSK ;;
*)
let ( extend_sketch_frame : extension -> float -> sketch -> sketch
) = extend_sketch_frame ;;

(* END SKETCHES *)


(* BEGIN BITMAPS *)


type bitmap == bitmaps__bitmap;;


let ( bitmap_width : bitmap -> int ) = bitmap_width;;
let ( bitmap_height : bitmap -> int ) = bitmap_height ;;
let ( bitmap_depth : bitmap -> int ) = bitmap_depth ;;
let ( create_bitmap : int -> int -> int -> bitmap
) = create_bitmap ;;
let ( sub_bitmap : bitmap -> int * int -> int * int -> bitmap
) = sub_bitmap;;
let ( copy_bitmap : bitmap -> bitmap ) = copy_bitmap;;

let ( invert_bitmap : bitmap -> bitmap ) = invert_bitmap ;;
let ( set_pixel : bitmap -> int -> int -> int -> unit
) = set_pixel ;;
let ( get_pixel : bitmap -> int -> int -> int
) = get_pixel ;;

let ( convert_bitmap : int * (int -> int) -> bitmap -> bitmap
) = convert_bitmap ;;
let ( map_bitmap : (int -> int) -> bitmap -> bitmap 
) = map_bitmap ;;
let ( read_bitmap : int -> string -> bitmap ) = read_bitmap ;;
let ( write_bitmap : bitmap -> string -> unit ) = write_bitmap ;;
let ( bitmap_frame : bitmap -> frame ) = bitmap_frame ;;
let ( bitmap_hull : bitmap -> point list ) = bitmap_hull;;

(* END BITMAPS *)

(* BEGIN FONTS *)




let ( add_font : font_style * font_description -> unit
) = add_font ;;
let ( remove_font : font_style * font_description -> unit
) = remove_font ;;
let ( print_font_list) =print_font_list ;;
let ( print_info_font : font_description -> unit
) = print_info_font ;;
let ( print_info_all_fonts) =print_info_all_fonts ;;

let ( default_font : unit -> font ) = default_font ;;
let ( set_default_font : font -> unit ) = set_default_font ;;
let ( reset_default_font) =reset_default_font ;;
let ( make_font : font_style -> float -> font ) = make_font;;

(* END FONTS *)

(* BEGIN TEXTS *)

let ( text_frame : font -> string -> frame ) = text_frame ;;
let ( text_width : font -> string -> float ) = text_width ;;
let ( text_height : font -> string -> float ) = text_height ;;

(* END TEXTS *)

(* BEGIN GRAPHICS_DEFAULTS *)

(*let ( default_graphic_state) =default_graphic_state ;;*)
let ( default_linewidthcoef) =default_linewidthcoef ;;
let ( default_linecap : unit -> linecap ) =default_linecap ;;
let ( default_linejoin : unit -> linejoin) =default_linejoin ;;
let ( default_dashpattern) =default_dashpattern ;;
let ( default_color : unit -> color ) =default_color ;;
let ( default_closed_sketch) =default_closed_sketch ;;
let ( default_fillstyle : unit -> fillstyle) =default_fillstyle ;;
let ( default_miterlimit) =default_miterlimit ;;
let ( default_linestyle : frame -> linestyle) =default_linestyle ;;
let ( set_default_linewidthcoef) =set_default_linewidthcoef ;;
let ( set_default_color : color -> unit) =set_default_color ;;
let ( set_default_closed_sketch) =set_default_closed_sketch ;;
let ( set_default_fillstyle : fillstyle -> unit) =set_default_fillstyle ;;
let ( set_default_linecap : linecap -> unit) =set_default_linecap ;;
let ( set_default_linejoin : linejoin -> unit) =set_default_linejoin ;;
let ( set_default_dashpattern) =set_default_dashpattern ;;
let ( set_default_miterlimit) =set_default_miterlimit ;;
(* END GRAPHICS_DEFAULTS *)



(* BEGIN PICTURES *)


type picture == pictures__picture;;


let ( recompute_picture_hull : picture -> picture
 ) = recompute_picture_hull;;
let ( picture_frame : picture -> frame ) =  picture_frame;;
let ( has_exact_frame_picture : picture -> bool
 ) = has_exact_frame_picture;;
let ( has_exact_frame : picture -> bool ) = has_exact_frame;;
let ( picture_hull : picture -> point list ) = picture_hull;;
let ( picture_center : picture -> point) =picture_center ;;
let ( picture_input_interface : picture -> interface
) = picture_input_interface ;;
let ( picture_output_interface : picture -> interface
) = picture_output_interface ;;
let ( picture_height : picture -> float ) =picture_height ;;
let ( picture_width : picture -> float ) =picture_width ;;
(*let ( picture_color) =picture_color;;*)
let ( change_color_picture : color -> picture -> picture
) = change_color_picture ;;
let ( change_linestyle_picture : linestyle -> picture -> picture
) = change_linestyle_picture ;;
let ( change_linewidth_picture : float -> picture -> picture
) = change_linewidth_picture ;;
let ( set_picture_interfaces : picture -> interface * interface -> picture
) = set_picture_interfaces ;;

let ( make_blank_picture : float * float -> picture
) = make_blank_picture ;;
let ( blank_rectangle : float * float -> picture)
= blank_rectangle;;
let ( blank_square : float -> picture)
= blank_square;;

let ( make_draw_picture : linestyle * color -> sketch -> picture
) =make_draw_picture ;;
let ( make_closed_draw_picture : linestyle * color -> sketch -> picture
) = make_closed_draw_picture ;;
let ( make_default_draw_picture : sketch -> picture
) =make_default_draw_picture ;;
let ( make_default_closed_draw_picture : sketch -> picture
) =make_default_closed_draw_picture ;;
let ( make_fill_picture : fillstyle * color -> sketch -> picture
) =make_fill_picture ;;
let ( make_default_fill_picture : sketch -> picture
) =make_default_fill_picture ;;
let ( clip_picture : clipstyle -> sketch -> picture -> picture
) =clip_picture ;;
let ( make_bitmap_picture : bitmap -> picture ) =make_bitmap_picture ;;
let ( make_bitmap_mask_picture : bitmap -> color -> bool -> picture
) =make_bitmap_mask_picture ;;
let ( make_default_bitmap_mask_picture : bitmap -> picture
) =make_default_bitmap_mask_picture ;;
let ( make_text_picture : font -> color -> string -> picture
) =make_text_picture ;;
let ( make_default_text_picture : string -> picture
) =make_default_text_picture ;;
let ( make_frame_picture : linestyle * color -> frame -> picture
) =make_frame_picture ;;
let ( make_default_frame_picture : frame -> picture
) =make_default_frame_picture ;;
let ( make_hull_picture : linestyle * color -> point list -> picture
 ) = make_hull_picture;;
let ( make_default_hull_picture : point list -> picture
 ) = make_default_hull_picture;;
let ( get_picture_frame_as_picture_with_lsty_and_color
 : picture -> linestyle * color -> picture
 ) = get_picture_frame_as_picture_with_lsty_and_color;;
let ( get_picture_frame_as_picture : picture ->  picture
 ) = get_picture_frame_as_picture;;
let ( get_picture_hull_as_picture_with_lsty_and_color
 :picture -> linestyle * color -> picture
 ) = get_picture_hull_as_picture_with_lsty_and_color;;
let ( get_picture_hull_as_picture :picture ->  picture
 ) = get_picture_hull_as_picture;;


let ( group_pictures : picture list -> picture ) =group_pictures ;;
let ( ungroup_picture : picture -> picture list ) =ungroup_picture ;;
let ( transform_picture : transformation -> picture -> picture
) =transform_picture ;;
let ( center_picture : picture -> point -> picture
) =center_picture ;;
let ( scale_and_center_picture : float * float -> picture -> picture
 ) = scale_and_center_picture;;
let ( translate_picture : float * float -> picture -> picture
) =translate_picture ;;
let ( translate : float * float -> picture -> picture ) =translate ;;
let ( rotate_picture : float -> picture -> picture
) =rotate_picture ;;
let ( rotate : float -> picture -> picture ) =rotate ;;
let ( scale_picture : float * float -> picture -> picture
) =scale_picture ;;
let ( scale : float * float -> picture -> picture ) =scale ;;
let ( fit_picture_in_frame : picture -> frame -> picture
) =fit_picture_in_frame ;;
let ( fit_in_frame : picture -> frame -> picture ) =fit_in_frame ;;
let ( force_picture_in_frame : frame -> picture -> picture
) =force_picture_in_frame ;;
let ( force_in_frame : frame -> picture -> picture
) =force_in_frame ;;
let ( add_frame_to_picture : picture -> picture
) =add_frame_to_picture ;;
let ( add_frame : picture -> picture ) =add_frame ;;
let ( vflip_picture : picture -> picture ) =vflip_picture ;;
let ( vflip : picture -> picture ) =vflip ;;
let ( hflip_picture : picture -> picture ) =hflip_picture ;;
let ( hflip : picture -> picture ) =hflip ;;
let ( subpicture_transformations : picture -> picture -> transformation list
) =subpicture_transformations;;
let ( subpicture_colors : picture -> picture -> color list
) =subpicture_colors;;
let ( subpicture_clips : picture -> picture -> path list
) =subpicture_clips;;
let ( subpictures : picture -> picture list ) =subpictures;;



let ( align_horizontally_picture_list : alignment -> picture list -> picture
) =align_horizontally_picture_list ;;
let ( align_horizontally : alignment -> picture list -> picture
) =align_horizontally ;;


let ( align_vertically_picture_list : alignment -> picture list -> picture
) =align_vertically_picture_list ;;
let ( align_vertically : alignment -> picture list -> picture
) =align_vertically ;;

let ( besides_picture : picture -> picture -> picture
) =besides_picture ;;
let ( over_picture : picture -> picture -> picture
) =over_picture ;;
let ( besides : picture -> picture -> picture ) =besides ;;
let ( over : picture -> picture -> picture ) =over ;;

let ( compose_horizontally_picture_list : picture list -> picture
) =compose_horizontally_picture_list ;;
let ( compose_horizontally : picture list -> picture
) =compose_horizontally ;;
let ( compose_vertically_picture_list : picture list -> picture
) =compose_vertically_picture_list ;;
let ( compose_vertically : picture list -> picture
) =compose_vertically ;;
let ( extend_picture_frame : extension -> float -> picture -> picture
) =extend_picture_frame ;;
let ( named_attach_pictures : picture * picture -> string * string -> picture
) =named_attach_pictures ;;
let ( attach_pictures : picture * picture -> picture ) =attach_pictures ;;
let ( make_textblock_picture
 : alignment -> float -> font -> color -> string list -> picture
) =make_textblock_picture ;;

let ( circletexttop : font -> string -> point -> float -> picture
) =circletexttop ;;
let ( circletextbottom : font -> string -> point -> float -> picture
) =circletextbottom ;;



(* END PICTURES *)



(* BEGIN TRPS *)


let ( set_path_size_limit) =set_path_size_limit ;;
let ( set_symbol_max) =set_symbol_max ;;
let ( set_gsave_max) =set_gsave_max ;;
let ( set_dash_size_limit) =set_dash_size_limit ;;

(* END TRPS *)


(* BEGIN DISPLAY *)
let ( page_height) =page_height ;;
let ( page_width) =page_width ;;
let ( page_hmargin) =page_hmargin ;;
let ( page_vmargin) =page_vmargin ;;
let ( set_page_height) =set_page_height ;;
let ( set_page_width) =set_page_width ;;
let ( set_page_hmargin) =set_page_hmargin ;;
let ( set_page_vmargin) =set_page_vmargin ;;
let ( compute_display_frame : picture -> frame
) =compute_display_frame ;;
let ( center_and_adjust_picture : picture -> picture
) =center_and_adjust_picture ;;
let ( center_and_adjust : picture -> picture
) =center_and_adjust ;;
let ( set_frame_extension_coef ) = set_frame_extension_coef;;
let ( ps_file : picture -> string -> unit) =ps_file ;;
let ( eps_file : picture -> string -> unit) =eps_file ;;
let ( ps : picture -> unit) =ps;;
let ( eps : picture -> unit) =eps;;


(* END DISPLAY *)




(* BEGIN OPTION *)


let ( string  : 'a -> string -> 'a * option) = string;;
let ( bool : 'a -> bool -> 'a * option ) = bool;;
let ( int : 'a -> int -> 'a * option ) = int;;
let ( float : 'a -> float -> 'a * option ) = float;;
let ( option : 'a -> 'a * option ) = option;;
let ( color : 'a -> color -> 'a * option ) = color;;
let ( dashPattern : int list -> string * option ) = dashPattern;;
let ( join : linejoin -> string * option ) = join;;
let ( cap : linecap -> string * option ) = cap;;
let ( fillStyle  : fillstyle -> string * option) = fillStyle;;
let ( lineLabel : string -> float -> string * option ) = lineLabel;;
let ( font : font_style -> string * option ) = font;;


let ( theString : ('a * option) list -> 'a -> string -> string) 
=  theString;;
let ( theInt : ('a * option) list -> 'a -> int -> int) =  theInt;;
let ( theFloat : ('a * option) list -> 'a -> float -> float) = theFloat;;
let ( theBool : ('a * option) list -> 'a -> bool -> bool) = theBool;;
let ( theOption : ('a * 'b) list -> 'a -> bool) = theOption;;
let ( theColor : ('a * option) list -> 'a -> color -> color) = theColor;;
let ( theDashPattern : (string * option) list -> int list -> int list) 
= theDashPattern;;
let ( theLineJoin : (string * option) list -> linejoin -> linejoin) 
= theLineJoin;;
let ( theLineCap : (string * option) list -> linecap -> linecap) 
= theLineCap;;
let ( theFillStyle : (string * option) list -> fillstyle -> fillstyle) 
= theFillStyle;;
let ( theLineLabel : (string * option) list -> option) = theLineLabel;;
let ( theFont : (string * option) list -> font_style -> font_style) = theFont;;
let ( findOption : ('a * 'b) list -> 'a -> 'b) = findOption;;

(* END OPTION *)

(* BEGIN GRAPH *)



(*
let ( fold ) = fold;;
let ( try_find ) = try_find;;
let ( nth ) = nth;;
let ( substituteNth ) = substituteNth;;
let ( sqr ) = sqr;;
let ( pSub ) = pSub;;
let ( pAdd ) = pAdd;;
let ( pMult : float -> float -> point) = pMult;;
let ( pi ) = pi;;
*)

let ( circlePoint : float -> float -> point) = circlePoint;;
let ( lengthOfLine : point * point -> float) = lengthOfLine;;
let ( slopeOfLine : point * point -> float) = slopeOfLine;;
let ( sketchGen : (string * option) list -> float -> sketch -> picture) 
= sketchGen;;
let ( sketch : float -> sketch -> picture) = sketch;;
let ( curvePos : point * point * point * point -> float -> point * float) 
= curvePos;;
let ( arrowFormGen : (string * option) list -> point * float -> string -> geom_element list list) 
= arrowFormGen;;
let ( textGen : (string * option) list -> string -> picture) = textGen;;
let ( text : string -> picture) = text;;
let ( diagOfFrame : frame -> float) = diagOfFrame;;
let ( blankSketch : (string * option) list -> sketch -> picture) 
= blankSketch;;
let ( rectOfFrame : (string * option) list -> frame -> picture) 
= rectOfFrame;;
let ( rectangleGen : (string * option) list -> picture -> picture) 
= rectangleGen;;
let ( rectangle : picture -> picture) = rectangle;;
let ( circOfFrame : (string * option) list -> frame -> picture) 
= circOfFrame;;
let ( circleGen : (string * option) list -> picture -> picture) 
= circleGen;;
let ( circle : picture -> picture) = circle;;
let ( ovalOfFrame : (string * option) list -> frame -> picture) = ovalOfFrame;;
let ( ovalGen : (string * option) list -> picture -> picture) = ovalGen;;
let ( oval : picture -> picture) = oval;;
let ( linesOfPoints : (string * option) list -> point list -> ((string * option) list * geom_element list list) list) = linesOfPoints;;
let ( curveOfPoints : (string * option) list -> (point * float) list -> ((string * option) list * geom_element list list) list) = curveOfPoints;;
let ( symmetricCurvesOfPoints : (string * option) list -> point * (float * point) list -> ((string * option) list * geom_element list list) list) = symmetricCurvesOfPoints;;
let ( hullOfPoints : (string * option) list -> point list -> ((string * option) list * geom_element list list) list) = hullOfPoints;;
let ( degOfDir : dir -> float) = degOfDir;;
let ( transformGraph : transformation -> graph -> graph) = transformGraph;;
let ( graphPoint : string -> graph -> point) = graphPoint;;
let ( graphLineLabel : float * float -> graph -> string -> point) 
= graphLineLabel;;
let ( nodeGraph : string -> graph) = nodeGraph;;
let ( addLines : graph -> line list -> graph) = addLines;;
let ( addPoints : (string * option) list -> graph -> (string * (string * float)) list -> graph) = addPoints;;
let ( polyGraph : string -> string list -> line list -> graph) = polyGraph;;
let ( tabularGraph : string -> tabStyle -> tabPos list list -> line list -> graph) = tabularGraph;;
let ( linkGraphs : graph * string -> graph * string -> line list -> graph) 
= linkGraphs;;
let ( composeGraphs : graph * string * string -> graph * string * string -> line list -> graph) = composeGraphs;;
let ( insLineGen : (graph * pos) list -> (string * option) list * string * dir * string -> (graph * pos) list * line) = insLineGen;;
let ( assembleGraphs : graph list -> string list -> ((string * option) list * string * dir * string) list -> graph) = assembleGraphs;;
let ( pictOfLines : transformation -> float -> (string * option) list -> ((string * option) list * geom_element list list) list -> picture list) 
= pictOfLines;;
let ( skeletonOfGraphGen : (string * option) list -> transformation * float -> graph -> picture list) = skeletonOfGraphGen;;
let ( graphGen : (string * option) list -> graph -> (string * picture) list -> picture) = graphGen;;
let ( graph : graph -> (string * picture) list -> picture) = graph;;


(* END GRAPH *)

(* BEGIN TREE *)


let ( tree_it : ('a -> 'b -> 'c label -> 'b) -> ('a, 'c) tree -> 'b -> 'b) 
= tree_it;;
let ( tree_map : ('a -> 'b) -> ('a, 'c) tree -> ('b, 'c) tree) 
= tree_map;;
let ( drawTree : (string * option) list -> ('a -> picture) * (point * point -> 'b -> picture) -> float * float * float list * point -> ('a, 'b) tree -> picture) = drawTree;;
let ( widthOfInfo : (picture, 'a) tree -> float) = widthOfInfo;;
let ( drawProofTree : (string * option) list -> float -> float * float * float list * point -> (picture, picture) tree -> picture) = drawProofTree;;

let ( makeTreePictureGen : (string * option) list -> ('a -> picture) * (point * point -> 'b -> picture) -> float * float * point -> ('a, 'b) tree -> picture) 
= makeTreePictureGen;;
let ( makeTreePicture : ('a -> picture) * (point * point -> 'b -> picture) -> float * float * point -> ('a, 'b) tree -> picture) = makeTreePicture;;
let ( treeLabelPos : float * float -> point * point -> point) 
= treeLabelPos;;
let ( treeGen : (string * option) list -> (picture, picture) tree -> picture) = treeGen;;
let ( tree : (picture, picture) tree -> picture) = tree;;
let ( makeProofTreePictureGen : (string * option) list -> ('a -> picture) -> float -> float * float * point -> ('a, picture) tree -> picture) 
= makeProofTreePictureGen;;
let ( makeProofTreePicture : ('a -> picture) -> float -> float * float * point -> ('a, picture) tree -> picture) 
= makeProofTreePicture;;
let ( proofTreeGen : (string * option) list -> (picture, picture) tree -> picture) = proofTreeGen;;
let ( proofTree : (picture, picture) tree -> picture) = proofTree;;
let ( treeGraphGen : (string * option) list -> string -> (string, 'a) tree -> line list -> graph) = treeGraphGen;;
let ( treeGraph : string -> (string, 'a) tree -> line list -> graph) 
= treeGraph;;

(* END TREE *)

(* BEGIN MLGLATEX *)

let (colorPict : float -> float -> float -> color -> color -> picture)
= colorPict;;
let (latexPictureGen :
  (string * option) list ->
  'a * 'b * string * string * string -> 'a * (picture * 'b))
= latexPictureGen;;
let (latexPicture :
  'a * 'b * string * string * string -> 'a * (picture * 'b))
= latexPicture;;
let (inverseTransAngle : transformation -> transformation)
= inverseTransAngle;;
let (makeLatexPicture :
  picture -> ('a * (picture * int)) list -> string -> unit)
= makeLatexPicture;;
let (latexBoxTable : (string * (picture * int)) list ref)
= latexBoxTable;;
let (add_latexBox : string * int * string * string * string -> unit)
= add_latexBox;;
let (empty_latexBoxTable : unit -> unit)
= empty_latexBoxTable;;
let (latexBox : string -> picture) = latexBox;;
let (latexBoxGen : (string * option) list -> string -> picture) = latexBoxGen;;
let (latex_mode : unit -> bool) = latex_mode;;
let (set_latex_mode : unit -> unit) = set_latex_mode;;
let (arrowPict : picture) = arrowPict;;
let (picDir : string ref) = picDir;;
