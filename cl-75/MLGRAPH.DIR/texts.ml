(*                                                                       *)
(*                     Projet      Formel                                *)
(*                                                                       *)
(* CAML-light: MLgraph library *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            LIENS                                      *)
(*                        45 rue d'Ulm                                   *)
(*                         75005 PARIS                                   *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)


(* $Id: texts.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* texts.ml                                                              *)
(*            Emmanuel Chailloux & Guy Cousineau                         *)





#open "MLgraph";;

#open "compatibility";;
#open "prelude";;
#open "frames";;
#open "fonts";;





(* somes variables of font description *)

let font_list =
ref [Courier,courier_descr;
 Courier_Oblique,courier_Oblique_descr;
 Courier_Bold,courier_Bold_descr; 
 Courier_BoldOblique,courier_BoldOblique_descr;
 Times_Roman,times_Roman_descr; 
 Times_Bold,times_Bold_descr; 
 Times_Italic,times_Italic_descr; 
 Times_BoldItalic,times_BoldItalic_descr; 
 Helvetica,helvetica_descr ; 
 Helvetica_Bold,helvetica_Bold_descr; 
 Helvetica_Oblique,helvetica_Oblique_descr; 
 Helvetica_BoldOblique,helvetica_BoldOblique_descr; 
 Symbol,symbol_descr
]
;;

let add_font fn = 
  font_list:=fn::!font_list
;;

let remove_font fn   = 
  if mem fn !font_list 
  then 
  begin
    font_list := subtract !font_list [fn]
  end
  else failwith ("remove_font : font unknown")
;;


   
let print_font_list () = 
    print_string "Available fonts are: "; print_newline();
    do_list (function (fs,fd) -> 
                 if eq_string fd.font_descr_name  "" 
                 then message ("in file "^fd.font_descr_filename)
                 else message (fd.font_descr_name^" loaded")) 
             !font_list;;

let print_info_font f =
 print_string ("font_descr_name : "^f.font_descr_name);
 print_newline ();print_string ("Max height : "^string_of_float f.font_descr_height);
 print_newline ();print_string ("Max width : "^string_of_float f.font_descr_width);
 print_newline ();
 if vect_length f.font_descr_descr = 0
  then (print_string "font_descr_width fixed : true";print_newline ())
  else (print_string "font_descr_width fixed : false";print_newline ())
;;

let print_info_all_fonts () =
 let pf (s,f) =
  print_info_font f;print_newline () 
  in
    print_newline ();
    do_list pf !font_list
;;
                  
let find_font_description f = 
 let fd = (try assoc f.font_style !font_list with
    find -> failwith ("Text: find_font_description: bad name "))
 in 
   (if vect_length fd.font_descr_descr_bbox = 0 then 
      let nfd = load_font (fd.font_descr_filename) in
       (fd.font_descr_name<-nfd.font_descr_name;
        fd.font_descr_descr_bbox<- nfd.font_descr_descr_bbox;
        fd.font_descr_descr<-nfd.font_descr_descr;
        fd.font_descr_width<-nfd.font_descr_width;
        fd.font_descr_height<-nfd.font_descr_height;()));
    fd
;;

(* create a text structure *)



let value_default_font = {font_style=Courier; font_size=12.0}
;;

let gs_default_font = ref value_default_font
;;

let default_font () = !gs_default_font
;;

let set_default_font f = gs_default_font := f
;;

let reset_default_font () = gs_default_font := value_default_font

;;

let make_font fs sz =
let f = {font_style=fs; font_size=sz} 
in 
  find_font_description f;
  f
;;


let make_text str font  = {t_string=str; t_font=font}
;;

let make_default_text  str = { t_string=str; t_font=default_font()}
;;

let change_size_text {t_string=str; t_font={font_style=n;font_size=_}} sz =
                         {t_string=str; t_font={font_style=n;font_size=sz}}
;;

let change_font_text { t_string=str; t_font=_} fnt  =
                              { t_string=str; t_font=fnt}     
;;


(* Compute text dimensions *)

let full_char_width fnt n=
  let f = find_font_description fnt
  and sca = fnt.font_size /. 12.0
  in
    let constant_width = (vect_length f.font_descr_descr = 0)
    in
     if constant_width
       then f.font_descr_width*.sca
       else f.font_descr_descr.(n)*.sca;;
       
  

let text_frame fnt s =
 if eq_string s "" then {xmin=0.0;xmax=0.0;ymin=0.0;ymax=0.0}
  else
  let sl = explode_ascii s
  and f = find_font_description fnt
  and sca = fnt.font_size /. 12.0
  in
    let constant_width = (vect_length f.font_descr_descr = 0)
    in
      let (ymin,ymax,width) =
         it_list (fun(y1,y2,w) c 
                     -> let descr_bbox= f.font_descr_descr_bbox.(c)
                        in let  descrx= if constant_width 
                                      then f.font_descr_width
                                      else f.font_descr_descr.(c)
                        in
                          let y1' = snd(fst descr_bbox)            
                          and y2' = snd(snd descr_bbox)            
                          in
                           (min_float y1 y1' , max_float y2 y2' ,  w +. descrx))
                 (f.font_descr_height , (-.f.font_descr_height) , 0.0)
                 sl
    in
     let xmin= fst(fst f.font_descr_descr_bbox.(hd sl))
     in
      let xmax = if f.font_descr_descr_bbox.(last sl) = ((0.0,0.0),(0.0,0.0))
                  then width 
		  else width-. (if constant_width 
                                   then f.font_descr_width 
                                   else f.font_descr_descr.(last sl))
		     +. fst( snd f.font_descr_descr_bbox.(last sl))
      in 
        {xmin=(xmin)*.sca; xmax=(xmax)*.sca; 
         ymin=(ymin)*.sca; ymax=(ymax)*.sca}
;;

let text_width fnt str=
  let frame = text_frame fnt str
  in frame.xmax -. frame.xmin;;


let text_height fnt str=
  let frame = text_frame fnt str
  in frame.ymax -. frame.ymin;;


