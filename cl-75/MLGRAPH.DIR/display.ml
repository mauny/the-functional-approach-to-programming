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


(* $Id: display.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* display.ml    The display functions for caml graphics                 *)
(*              Emmanuel Chailloux & Guy Cousineau                       *)
(*              Tue Jan 21  1992                                         *)




#open "MLgraph";;

#open "compatibility";;
#open "prelude";;
#open "bitmaps";;
#open "fonts";;
#open "texts";;
#open "frames";;
#open "paint";;
#open "geometry";;
#open "graphics_defaults";;
#open "sketches";;
#open "pictures";;
#open "cps";;
#open "trps";;




let default_page =
  {page_height=   800.0;
   page_width =   612.0;
   page_hmargin =  20.0;
   page_vmargin =  20.0};;
   
let page_height () = default_page.page_height;;
let page_width () = default_page.page_width;;
let page_hmargin () = default_page.page_hmargin;;
let page_vmargin () = default_page.page_vmargin;;

let set_page_height h = 
  default_page.page_height <- h;;
let set_page_width w = 
  default_page.page_width <- w;;
let set_page_hmargin hm = 
  default_page.page_hmargin <- hm;;
let set_page_vmargin vm = 
  default_page.page_vmargin <- vm;;

let compute_display_frame p =
 let {xmin=a; xmax=b; ymin=c; ymax=d} = picture_frame p
 in
  let w = b-.a and h = d-.c and w' = page_width() -. 2.0*.page_hmargin()
                      and h' = page_height() -.2.0*.page_vmargin()
                      and xx  = page_width()/.2.0
                      and yy  = page_height()/.2.0
  in
     if lt_float (w'/.w)  (h'/.h)
               then {xmin=xx-.w'/.2.0; 
                     xmax=xx+.w'/.2.0;
                     ymin=yy-.h*.w'/.(2.0*.w);
                     ymax=yy+.h*.w'/.(2.0*.w)}
               else {xmin=xx-.w*.h'/.(2.0*.h); 
                     xmax=xx+.w*.h'/.(2.0*.h); 
                     ymin=yy-.h'/.2.0;
                     ymax=yy+.h'/.2.0};;

let center_and_adjust_picture p =  
     fit_picture_in_frame p (compute_display_frame p);;
let center_and_adjust = center_and_adjust_picture;;


let frame_extension_coef = ref 0.02;;
let set_frame_extension_coef x = (frame_extension_coef:=x);;


let make_ps_file pict filename psflag = 
   let {xmin=a;xmax=b;ymin=c;ymax=d}= picture_frame pict in
   let x_offset = (b-.a)*. !frame_extension_coef
   and y_offset = (d-.c)*. !frame_extension_coef
   in
     open_PS filename;
     send_comment_PS !begin_prelude1;
     send_comment_PS !begin_prelude2;
     send_comment_PS !begin_prelude3;
     (if (not psflag)
     then send_comment_PS ("%BoundingBox:"
                     ^ (string_of_int (int_of_float  (a-.x_offset)))
                     ^ " "
                     ^ (string_of_int (int_of_float (c-.y_offset)))
                     ^ " "
                     ^ (string_of_int (int_of_float (b+.x_offset)))
                     ^ " "
                     ^ (string_of_int (int_of_float (d+.y_offset)))));
     send_comment_PS !end_prelude;
     copy_prelude_PS();
     reset_trps();
     reset_actual_font ();
     set_optimized_mode true;
     display_picture pict;
     close_PS psflag
;;

let ps_file  p fn = make_ps_file p fn true
and eps_file p fn = make_ps_file p fn false
and ps p = make_ps_file p "" true
and eps p = make_ps_file p "" false ;;







