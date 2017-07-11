

#open "MLgraph";;



value courier_descr : font_description;;
value courier_Bold_descr : font_description;;
value courier_Oblique_descr : font_description;;
value courier_BoldOblique_descr : font_description;;
value times_Roman_descr : font_description;;
value times_Bold_descr : font_description;;
value times_Italic_descr : font_description;;
value times_BoldItalic_descr : font_description;;
value helvetica_descr : font_description;;
value helvetica_Bold_descr : font_description;;
value helvetica_Oblique_descr : font_description;;
value helvetica_BoldOblique_descr : font_description;;
value symbol_descr : font_description;;
exception Find of int
;;
value pos_char_in_string : string -> char -> int -> int -> int;;
value floatpair_of_string : string -> float * float;;
value bbox_of_string : string -> (float * float) * (float * float);;
value load_font : string -> font_description;;
