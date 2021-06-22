(* $Id: fonts.mlip,v 1.1 1997/08/14 11:34:50 emmanuel Exp $ *)


open Compatibility

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




val courier_descr : font_description;;
val courier_Bold_descr : font_description;;
val courier_Oblique_descr : font_description;;
val courier_BoldOblique_descr : font_description;;
val times_Roman_descr : font_description;;
val times_Bold_descr : font_description;;
val times_Italic_descr : font_description;;
val times_BoldItalic_descr : font_description;;
val helvetica_descr : font_description;;
val helvetica_Bold_descr : font_description;;
val helvetica_Oblique_descr : font_description;;
val helvetica_BoldOblique_descr : font_description;;
val symbol_descr : font_description;;
exception Find of int
;;
val pos_char_in_string : string -> char -> int -> int -> int;;
val floatpair_of_string : string -> float * float;;
val bbox_of_string : string -> (float * float) * (float * float);;
val load_font : string -> font_description;;
