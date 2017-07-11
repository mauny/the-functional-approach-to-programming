(*                                                                       *)
(*                     Projet      Formel                                *)
(*                                                                       *)
(*                      MLgraph example                                  *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            LIENS                                      *)
(*                        45 rue d'Ulm                                   *)
(*                         75005 PARIS                                   *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)

(* $Id: visages.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* Visages.ml      Naive picture using the change_line_width function    *)
(*                 Emmanuel Chailloux                                    *)
(*                 Feb 4 1994                                            *)

(* Original picture from : 
\bibitem {Peli87:pip} [Peli87:pip]
``Denis G. Peli''
``Programming in PostScript''
Byte, May 1987.
*)



#open "MLgraph";;


let f1 =  
Seg 
[
{xc=39.0;yc=210.0};
{xc=108.0;yc=210.0};
{xc=155.0;yc=181.0};
{xc=167.0;yc=146.0};
{xc=155.0;yc=117.0};
{xc=155.0;yc=89.0};
{xc=145.0;yc=55.0};
{xc=134.0;yc=27.0};
{xc=116.0;yc=8.0};
{xc=78.0;yc=7.0};
{xc=46.0;yc=10.0};
{xc=23.0;yc=30.0};
{xc=7.0;yc=71.0};
{xc=4.0;yc=110.0};
{xc= -3.0;yc=136.0};
{xc=4.0;yc=174.0};
{xc=39.0;yc=210.0}
]
;;

let f2 = 
Seg
[
{xc=1.0;yc=130.0};
{xc=36.0;yc=143.0};
{xc=57.0;yc=162.0};
{xc=71.0;yc=182.0};
{xc=85.0;yc=160.0};
{xc=112.0;yc=141.0};
{xc=158.0;yc=126.0}
]
;;

let f3 =
Seg
[
{xc=24.0;yc=121.0};
{xc=45.0;yc=127.0};
{xc=60.0;yc=125.0}
];;

let f4 =
Seg
[
{xc=24.0;yc=108.0};
{xc=34.0;yc=116.0};
{xc=52.0;yc=116.0};
{xc=61.0;yc=109.0};
{xc=52.0;yc=101.0};
{xc=35.0;yc=101.0};
{xc=24.0;yc=108.0}
]
;;

let f5 = 
Seg
[
{xc=105.0;yc=107.0};
{xc=115.0;yc=115.0};
{xc=131.0;yc=115.0};
{xc=141.0;yc=107.0};
{xc=134.0;yc=99.0};
{xc=117.0;yc=98.0};
{xc=105.0;yc=107.0}
]
;;

let f6 = 
Seg
[
{xc=131.0;yc=126.0};
{xc=110.0;yc=126.0};
{xc=100.0;yc=122.0};
{xc=92.0;yc=113.0};
{xc=87.0;yc=97.0};
{xc=89.0;yc=83.0};
{xc=92.0;yc=67.0}
]
;;

let f7 = 
Seg
[
{xc=61.0;yc=44.0};
{xc=71.0;yc=37.0};
{xc=79.0;yc=39.0};
{xc=91.0;yc=36.0};
{xc=98.0;yc=40.0};
{xc=107.0;yc=37.0};
{xc=112.0;yc=43.0}
]
;;

let f8 =
Seg
[
{xc=75.0;yc=52.0};
{xc=84.0;yc=56.0};
{xc=94.0;yc=52.0};
{xc=102.0;yc=57.0}
]
;;


let face = group_sketches (map make_sketch [[f1];[f2];[f3];[f4];[f5];[f6];[f7];[f8]])
;;

set_default_linejoin Roundjoin;;
set_default_linecap  Roundcap;;

let draw_face = make_default_draw_picture face;;

let visage =  
  let lw = 10.0 in 
  let p1 = change_linewidth_picture lw draw_face   in
  let p2 = change_color_picture (Gra 1.0) 
              (change_linewidth_picture (lw /. 2.) draw_face)
  in
    group_pictures [p1;p2]
;;

let tr = make_transformation (0.707,0.0,190.0,0.0,0.707,0.0)
;;

let rec visages  =  
      function 1 -> visage
   |           n -> group_pictures [visage;
									(transform_picture tr (visages (n-1)))]
;;

let display_visages n filename = 
  eps_file  
    (translate_picture (72.,72.) 
       (scale_picture (0.95, 2.5) (visages n)))
    filename
;;


display_visages 4 "visages";;

