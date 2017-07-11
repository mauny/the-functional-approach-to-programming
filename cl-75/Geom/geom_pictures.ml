#directory "../MLGRAPH.DIR";;
#open "MLgraph";;
#directory "../Util";;
#directory "../Arbres";;
#open "prelude";;
#infix "o";;
#open "binary_trees";;
#open "lexer";;
#open "binary_trees_parser";;
#open "binary_trees_drawing";;


let eps_file p f =
  printf__fprintf stderr "Doing %s" f;
  flush stderr;
  eps_file p f;
  printf__fprintf stderr ". done.\n";
  flush stderr
;;

(* +preembule+ *)

let triangle_sketch (pt1,pt2,pt3) = 
  make_sketch [Seg [pt1;pt2;pt3;pt1]];;

let line_triangle (pt1,pt2,pt3) lw =
  let LS = {linewidth=lw;linecap=Buttcap;
            linejoin=Miterjoin;dashpattern=[]}
  in make_draw_picture (LS,black) (triangle_sketch (pt1,pt2,pt3));;

let fill_triangle (pt1,pt2,pt3) c =
  make_fill_picture (Nzfill,c) (triangle_sketch (pt1,pt2,pt3));;

let arrow_pict l w l' w' c =
  align_horizontally
    Align_Center
    [make_draw_picture
       ({linewidth=w;linecap=Squarecap;
         linejoin=Miterjoin;dashpattern=[]},c)
       (make_sketch [Seg [origin;{xc=l-.l';yc=0.0}]]);
     fill_triangle({xc=0.0;yc=w'},{xc=l';yc=0.0},{xc=0.0;yc= -.w'}) c];;

let arrow (pt1,pt2) w c =
  let x = abs_float (pt2.xc-.pt1.xc)
  and y = abs_float (pt2.yc-.pt1.yc) in
  let l = sqrt(x*.x+.y*.y) in
  let T = handle_transform (pt1,pt2) (origin ,{xc=l;yc=0.0})
  in transform_picture T
       (arrow_pict l w
          (min_float (max_float (l/.20.0) (3.0*.w)) (l/.3.0))
          (2.0*.w) c);;

let rec intervalstep x y step =
  if y<.x then []
  else x:: intervalstep (x+.step) y step;;

(* +preembule+ *)
(* +coordinates+ *)

let coordinates (x1,x2) (y1,y2) step =
  if not (x1 < x2 & y1 < y2)
  then failwith "mk_coord: wrong interval"
  else
    let x1 = if x1 < 0 then x1 else 0
    and x2 = if x2 > 0 then x2 else 0
    and y1 = if y1 < 0 then y1 else 0
    and y2 = if y2 > 0 then y2 else 0 in
    let [x1;x2;y1;y2;step] = map float_of_int [x1;x2;y1;y2;step] in
    let x_dashes = map (fun y -> make_sketch
                                   [Seg [{xc=x1;yc=y};{xc=x2;yc=y}]])
                     (intervalstep (y1+.1.0) (y2-.1.0) step)
    and y_dashes = map (fun x -> make_sketch
                                   [Seg [{xc=x;yc=y1};{xc=x;yc=y2}]])
                     (intervalstep (x1+.1.0) (x2-.1.0) step) in
    let lsty_dashes = {linewidth=0.005;linecap=Buttcap;
                       linejoin=Miterjoin;dashpattern=[]} in
    let axes = group_pictures
                 [arrow ({xc=x1;yc=0.0},{xc=x2;yc=0.0}) 0.10 black;
                  arrow ({xc=0.0;yc=y1},{xc=0.0;yc=y2}) 0.10 black]
    and dashes = make_draw_picture (lsty_dashes, black)
                   (group_sketches (x_dashes@y_dashes))
    and unit = let p = make_text_picture
                         (make_font Helvetica_Bold (step*.0.5))
                         black
                         ("unit = " ^ string_of_int (int_of_float step))
               and t = translation (x1,(y1+.0.2*.step))
               in transform_picture t p
    in group_pictures [unit;axes;dashes];;

(* +coordinates+ *)
(* +points+ *)

let mk_point_example ptl = 
  let HF =  make_font Helvetica_Bold 0.5  in
  group_pictures
    (map (fun ({xc=x;yc=y} as pt)
              -> group_pictures
                   [make_default_fill_picture
                      (make_sketch [Arc(pt, 0.1, 0.0, 360.0)]);
                    let t = make_text_picture
                              HF
                              black
                              ("("^ (string_of_float pt.xc)^","
                               ^ (string_of_float pt.yc) ^ ")")
                    in transform_picture
                         (let {xc=x';yc=y'}=picture_center t
                          in translation (x-.x',y+.0.4-.y'))
                         t])
       ptl);;

let P = scale_picture (40.0,40.0)
          (group_pictures
             [mk_point_example [{xc= -.2.5 ;yc=3.5 };{xc= -.1.5 ;yc= -.1.5 };
                                {xc=2.0 ;yc=3.0 }];
              coordinates (-4,4) (-3,5) 1]) in
eps_file P "../../PS/points";;

(* +points+ *)
(* +geom+ *)

let mk_geom_example lw c sl1 sl2 = 
  let lsty_pict = {linewidth=lw;linecap=Buttcap;
                   linejoin=Miterjoin;dashpattern=[]}
  in group_pictures
       ( (map (make_closed_draw_picture (lsty_pict , c)) sl1) @
         (map (make_draw_picture (lsty_pict , c)) sl2));;

let P =
  let HF = make_font Helvetica_Bold 0.5 in
  let A = make_sketch [Seg [{xc=1.0;yc=1.0};{xc=1.0;yc=3.0};
                            {xc=3.0;yc=3.0};
                            {xc=3.0;yc=1.0};{xc=1.0;yc=1.0}]]
  and B = make_sketch [Arc ({xc=7.0;yc=4.0},2.0,30.0,290.0)]
  and C = make_sketch [Curve ({xc=1.0;yc=8.0},
                              {xc=2.0;yc=4.0},
                              {xc=4.0;yc=9.0},
                              {xc=5.0;yc=8.0})]
  and A_text = group_pictures
                 [transform_picture (translation (0.5,0.7))
                    (make_text_picture HF black "A" );
                  transform_picture (translation (0.5,3.0))
                    (make_text_picture HF black "B");
                  transform_picture (translation (3.2,3.0))
                    (make_text_picture HF black "C");
                  transform_picture (translation (3.2,0.7))
                    (make_text_picture HF black "D")]
  and B_text = group_pictures
                 [transform_picture (translation (0.5,8.0))
                    (make_text_picture HF black "E");
                  transform_picture (translation (2.5,4.0))
                    (make_text_picture HF black "F");
                  transform_picture (translation (4.2,9.1))
                    (make_text_picture HF black "G");
                  transform_picture (translation (5.2,8.0))
                    (make_text_picture HF black "H")]
  and C_text = transform_picture (translation (7.4,3.8))
                 (make_text_picture HF black "I")
  and B_dashes = let lsty= {linewidth=0.005;linecap=Buttcap;
                            linejoin=Beveljoin;dashpattern=[]}
                 and s= Seg [{xc=1.0;yc=8.0};
                             {xc=2.0;yc=4.0};
                             {xc=4.0;yc=9.0};
                             {xc=5.0;yc=8.0}]
                 in make_draw_picture (lsty,black) (make_sketch [s])
  and C_dashes = let lsty= {linewidth=0.005;linecap=Buttcap;
                            linejoin=Beveljoin;dashpattern=[]}
                 and s= Seg [{xc=(7.0+.2.0*.cosinus 30.0);
                              yc=(4.0+.2.0*.sinus 30.0)};
                             {xc=7.0;yc=4.0};
                             {xc=(7.0+.2.0*.cosinus 290.0);
                              yc=(4.0+.2.0*.sinus 290.0)}]
                 in make_draw_picture (lsty,black) (make_sketch [s])
  and F_dot = make_fill_picture (Nzfill,black)
                (make_sketch [Arc ({xc=2.0;yc=4.0},
                                   0.05, 0.0,360.0)])
  and G_dot = make_fill_picture (Nzfill,black)
                (make_sketch [Arc ({xc=4.0;yc=9.0},
                                   0.05, 0.0,360.0)])
  and I_dot = make_fill_picture (Nzfill,black)
                (make_sketch [Arc ({xc=7.0;yc=4.0},
                                   0.05, 0.0,360.0)])
  and Coord = coordinates (-5,6) (-5,6) 1
  in scale_picture (30.0,30.0)
       (group_pictures
          (map (transform_picture
                  (translation (-.4.0,-.4.0)))
             [mk_geom_example 0.16 (Gra 0.0) [A] [B;C];
              A_text;B_text;C_text;F_dot;G_dot;I_dot;
              B_dashes;C_dashes]
           @ [Coord]))
in eps_file P "../../PS/geom";;

(* +geom+ *)
(* +Ptext+ *)

let S1 = make_sketch
          [Arc({xc=5.0;yc=7.0},2.0,-.90.0,90.0);
           Seg [{xc=5.0;yc=9.0};{xc=3.0;yc=9.0};
                {xc=3.0;yc=1.0};{xc=4.0;yc=1.0};
                {xc=4.0;yc=5.0};{xc=5.0;yc=5.0}]];;

let S2 = make_sketch
          [Arc({xc=5.;yc=7.},1.,-.90.,90.);
           Seg [{xc=5.;yc=8.};{xc=4.;yc=8.};
                {xc=4.;yc=6.};{xc=5.;yc=6.}]];;

let S = group_sketches [S1;S2];;

let P = let lsty = {linewidth=0.2;linecap=Buttcap;
                   linejoin=Beveljoin;dashpattern=[]}
       in make_closed_draw_picture (lsty,Gra 0.0) S;;

let FP = let fsty = Eofill
         in make_fill_picture (fsty,Gra 0.5) S;;

let T1 =
  let CB = make_font Courier_Bold 12.0
  in
  extend_picture_frame Vertic_ext 0.05
    (extend_picture_frame Right_ext 0.1
       (make_textblock_picture Align_Left 12.0 CB black
          ["let skp=";
           "group_sketches";
           " [(make_sketch";
           "   [Arc({xc=5.;yc=7.},2.,-90.,90.);";
           "      Seg [{xc=5.;yc=9.};{xc=3.;yc=9.};";
           "           {xc=3.;yc=1.};{xc=4.;yc=1.};";
           "           {xc=4.;yc=5.};{xc=5.;yc=5.}])];";
           "  (make_sketch";
           "   [Arc({xc=5.;yc=7.},1.,-90.,90.);";
           "      Seg [{xc=5.;yc=8.};{xc=4.;yc=8.};";
           "           {xc=4.;yc=6.};{xc=5.;yc=6.}])])]"
          ] ))
in
let CPT = extend_picture_frame Vertic_ext 0.1
            (extend_picture_frame Horiz_ext 0.025
               (compose_horizontally
                  [T1;group_pictures
                        [P;coordinates (-1,10) (-1,10) 1]]))
in
eps_file CPT "../../PS/Ptext";;

(* +Ptext+ *)
(* +sketch_pictures+ *)

let T2 =
  let CB = make_font Courier_Bold 12.0
  in
  extend_picture_frame Vertic_ext 0.05
    (extend_picture_frame Right_ext 0.1
       (make_textblock_picture Align_Left 12.0 CB black
          [" let P1=     ";
           "   let lsty = {linewidth=0.2;linecap=Buttcap;";
           "             linejoin=Beveljoin;dashpattern=[]}";
           "   in  make_draw_picture (lsty,Gra 0.0) skp;;";
           "      ";
           "      ";
           " let P2=     ";
           "    make_fill_picture (Eofill,Gra 0.5) skp;;"
          ]))
in
let CP1 =group_pictures
           [P;coordinates (-1,10) (-1,10) 1]
and CP2 = group_pictures
            [FP;coordinates (-1,10) (-1,10) 1]
in let CPT = extend_picture_frame Vertic_ext 0.1
               (extend_picture_frame Horiz_ext 0.025
                  (compose_horizontally
                     [T2;extend_picture_frame Right_ext 0.1 CP1;CP2]))
   in
   (* eps_file CP1 "../../PS/Pdraw";
      eps_file CP2 "../../PS/Pfill"; *)
   eps_file CPT "../../PS/sketch_pictures";;

(* +sketch_pictures+ *)
(* +bitmaps+ *)

let B =
  let HF= make_font Helvetica 4.0
  in
  let b1= read_bitmap 1 "../../PS/caml_bitmap4"
  and T1 = (extend_picture_frame Top_ext 3.0
              (make_text_picture HF black"caml_bitmap"))
  and T2 = (extend_picture_frame Top_ext 3.0
              (make_text_picture HF black
                 "map_bitmap (fun 0-> 1| 1->0) caml_bitmap" ))
  in let b2 = map_bitmap (fun 0-> 1| 1->0) b1
     in let B2 = extend_picture_frame Right_ext 0.3
                   (align_vertically Align_Center
                      [make_bitmap_picture b2; T1])
        and B1 = align_vertically Align_Center
                   [make_bitmap_picture b1; T2]
        in compose_horizontally [B2;B1]
in
eps_file (scale_picture (3.0,3.0) B) "../../PS/bitmaps";;

(* +bitmaps+ *)
(* +transformations+ *)

let TP =
  let HF= make_font Helvetica_Bold 0.7
  and BP = group_pictures [P;FP]
  in
  let T1= transform_picture (translation (2.0,-.9.0))
  and T2 = transform_picture (scaling (0.5,0.5))
  and T3 = transform_picture (rotation {xc= -.2.0;yc= -.2.0} (60.0))
  and R= rotation {xc= -.2.0;yc= -.2.0} (60.0)
  in
  let transfos= let lsty = {linewidth=0.05;linecap=Buttcap;
                            linejoin=Beveljoin;dashpattern=[1;1]}
                and segs =
                  group_sketches
                    [make_sketch [Seg [{xc=3.0;yc=9.0};{xc=5.0;yc=0.0}]];
                     make_sketch [Seg [{xc=7.0;yc=7.0};{xc=9.0;yc= -.2.0}]];
                     make_sketch [Seg [{xc=0.0;yc=0.0};{xc=5.0;yc= -.8.0}]];
                     make_sketch [Seg [{xc= -.2.0;yc= -.2.0};{xc=2.5;yc=0.0}]];
                     make_sketch [Seg [{xc= -.2.0;yc= -.2.0};
                                       transform_point R {xc=2.5;yc=0.0}]]]
                in
                make_draw_picture (lsty,Gra 0.0) segs
  and dot = make_fill_picture (Nzfill,black)
              (make_sketch [Arc ({xc= -.2.0;yc= -.2.0},
                                 0.075, 0.0,360.0)])
  and T1_text= transform_picture (translation (7.7,5.4))
                 (make_text_picture HF black "T1")
  and T1_text'= transform_picture (translation (4.8,2.5))
                  (make_text_picture HF black "T1")
  and T2_text= transform_picture (translation (2.0,-.5.0))
                 (make_text_picture HF black "T2")
  and T3_text= transform_picture (translation (-.1.8,-.1.5))
                 (make_text_picture HF black "T3")
  in
  group_pictures
    [BP;
     T1 BP;
     T2 (T1 BP);
     T3(T2 (T1 BP));
     dot; T1_text;T1_text';T2_text;T3_text;
     coordinates (-3,10) (-8,10) 1;
     transfos]
in eps_file (scale_picture (40.0,40.0) TP) "../../PS/transformations";;

(* +transformations+ *)
(* +frames+ *)

let TP =
  let frame = {xmin=1.0;xmax=9.0;ymin=1.0;ymax=4.0}
  and HF = make_font Helvetica 20.0
  in
  let BP = group_pictures [FP;P]
  and FrP = make_draw_picture
              ({linewidth=0.2;linecap=Buttcap;
                linejoin=Miterjoin;dashpattern=[]},
               black)
              (make_sketch [Seg [{xc=1.0;yc=1.0}; {xc=1.0;yc=4.0};
                                 {xc=9.0;yc=4.0}; {xc=9.0;yc=1.0};
                                 {xc=1.0;yc=1.0}]])
  and FrBP = fit_picture_in_frame
               (group_pictures [FP;P])
               frame
  and Coord = coordinates (-1,10) (-1,10) 1
  in
  let P1 = scale_picture (20.0,20.0) (group_pictures [BP;Coord])
  and P2 = scale_picture (20.0,20.0) (group_pictures [FrP;Coord])
  and P3 = scale_picture (20.0,20.0) (group_pictures [FrBP;Coord])
  and BP1_text= extend_picture_frame Top_ext 0.5
                  (make_text_picture HF black "Our Example")
  and BP2_text= extend_picture_frame Top_ext 0.5
                  (make_text_picture HF black "A frame")
  and BP3_text= extend_picture_frame Top_ext 0.5
                  (make_text_picture HF black "Example fit into frame")
  in
  align_horizontally Align_Center
    [extend_picture_frame Right_ext 0.3
       (align_vertically Align_Center [P1;BP1_text]);
     extend_picture_frame Right_ext 0.3
       (align_vertically Align_Center [P2;BP2_text]);
     align_vertically Align_Center [P3;BP3_text]]
in eps_file TP "../../PS/frames";;

(* +frames+ *)
(* +flips+ *)

let TP =
  let BP = group_pictures [FP;P]
  and HF = make_font Helvetica 20.0
  in
  let BP1 = rotate_picture 45.0 BP
  and BP2 = vflip_picture BP
  and BP3= hflip_picture BP
  and Coord = coordinates (-1,10) (-1,10) 1
  and BP1_text= extend_picture_frame Top_ext 0.5
                  (make_text_picture HF black "rotation  by 45")
  and BP2_text= extend_picture_frame Top_ext 0.5
                  (make_text_picture HF black "vertical flip")
  and BP3_text= extend_picture_frame Top_ext 0.5
                  (make_text_picture HF black "horizontal flip")
  in
  let P1 = scale_picture (20.0,20.0) (group_pictures [BP1;Coord])
  and P2 = scale_picture (20.0,20.0) (group_pictures [BP2;Coord])
  and P3 = scale_picture (20.0,20.0) (group_pictures [BP3;Coord])
  in
  align_horizontally Align_Center
    [extend_picture_frame Right_ext 0.3
       (align_vertically Align_Center [P1;BP1_text]);
     extend_picture_frame Right_ext 0.3
       (align_vertically Align_Center [P2;BP2_text]);
     align_vertically Align_Center [P3;BP3_text]]
in eps_file TP "../../PS/flips";;

(* +flips+ *)
(* +alignments+ *)
(* 
   let b= make_bitmap_picture
   (read_bitmap 1 "../../PS/caml_bitmap4" )
   (*          in sub_bitmap cb (0,4) (bitmap_width cb,bitmap_height cb-4))*)
   and small = transform_picture (scaling (0.9,0.9))
   and HF= make_font Helvetica 12.0
   in

   let rec iterate f n x = if n=0 then []
   else x::iterate f (n-1) (f x)
   in


   let B= align_horizontally
   Align_Bottom
   (iterate small 12 b)
   and T=
   extend_picture_frame Top_ext 0.2
   (make_textblock_picture Align_Left 10.0 HF black
   ["     ";
   "let small = transform_picture (scaling (0.9,0.9));;";
   " ";
   "let rec iterate f n x = ";
   "     if n=0 then []";
   "            else x::iterate f (n-1) (f x);;";
   "     ";
   "align_horizontally Align_Bottom (iterate small 12 camel)"

   ])

   in

   eps_file
   (extend_picture_frame All_ext 0.1
   (align_vertically Align_Center [B;T])) "../../PS/alignment";;
 *)
(* +alignments+ *)
(* +composition_example+ *)
(* +composition_example+ *)
(* +circle_camel_prep+ *)

let rec make_list v=
  fun 0 -> [] | n -> v::make_list v (n-1);;

let camel =
  (make_bitmap_picture
     (read_bitmap 1 "../../PS/caml_bitmap4" )
  (* in sub_bitmap cb (0,4) (bitmap_width cb,bitmap_height cb-4)) *)
  );;

(* +circle_camel_prep+ *)
(* +circle_camel1+ *)
(*
let circle_camel1=
  draw_pictures_on_circle 150.0
    (map (fun n -> camel)
       (make_list camel 12)) in
eps_file circle_camel1 "../../PS/circle_camel1";;
*)
(* +circle_camel1+ *)
(* +circle_camel2+ *)
(*
let circle_camel2=
  draw_pictures_on_circle 150.0
    (map (fun n -> (rotate_picture (30.0*.(float_of_int n))
                      (rotate_picture 270.0 camel)))
       (interval 0 11)) in
eps_file circle_camel2 "../../PS/circle_camel2";;
*)
(* +circle_camel2+ *)
(* +squelettes_prep+ *)

let p1 =
  let t1= btree_of_string int_of_string "1(2,3(4(6,7),5))"
  and lsty={linewidth= 1.0;linecap=Buttcap;
            linejoin=Beveljoin;dashpattern=[]}
  and cl1=[1.0;1.0;1.0] in
  let tstyle1= {vdist=50.0; hdist=50.0; coef_list=cl1;
                tlsty=lsty; tcolor=black} in
  draw_btree
    tstyle1 (map_btree (fun x -> make_blank_picture (0.0,0.0)) t1);;

let p2 =
  let t2= btree_of_string int_of_string
            "1(2(4(8,9),5(10,11)),3(6(12,13),7(14,15)))"
  and lsty={linewidth= 1.0;linecap=Buttcap;
            linejoin=Beveljoin;dashpattern=[]}
  and cl2=[1.0;0.5;0.5] in
  let tstyle2= {vdist=50.0; hdist=100.0; coef_list=cl2;
                tlsty=lsty; tcolor=black} in
  draw_btree
    tstyle2 (map_btree (fun x -> make_blank_picture (0.0,0.0)) t2);;

(* +squelettes_prep+ *)
(* +squelettes+ *)

let pp1= translate_picture (100.0,800.0) p1 in
let pp2= translate_picture (100.0,800.0) p2 in
let blank= make_blank_picture (100.0,100.0) in
let p= align_horizontally Align_Center [pp1;blank;pp2] in
printexc__f (eps_file p) "../../PS/squelettes";;

(* +squelettes+ *)
(* +arbres_entiers_prep+ *)

let p1 =
  let t1= btree_of_string int_of_string "1(2,3(4(6,7),5))"
  and lsty= {linewidth= 1.0;linecap=Buttcap;
             linejoin=Beveljoin;dashpattern=[]}
  and cl1= [1.0;1.0;1.0] in
  let tstyle1= {vdist=50.0; hdist=50.0; coef_list=cl1;
                tlsty=lsty; tcolor=black} in
  draw_btree tstyle1 (map_btree (draw_int_node 10.0) t1);;

let p2 =
  let t2= btree_of_string int_of_string
            "1(2(4(8,9),5(10,11)),3(6(12,13),7(14,15)))"
  and lsty= {linewidth= 1.0;linecap=Buttcap;
             linejoin=Beveljoin;dashpattern=[]}
  and cl2= [1.0;0.5;0.5] in
  let tstyle2= {vdist=50.0; hdist=100.0; coef_list=cl2;
                tlsty=lsty; tcolor=black} in
  draw_btree tstyle2 (map_btree (draw_int_node 10.0) t2);;

(* +arbres_entiers_prep+ *)
(* +arbres_entiers+ *)

let pp1= translate_picture (100.0,800.0) p1 in
let pp2= translate_picture (100.0,800.0) p2 in
let blank= make_blank_picture (100.0,100.0) in
let p= align_horizontally Align_Center [pp1;blank;pp2] in
printexc__f (eps_file p) "../../PS/arbres_entiers";;

(* +arbres_entiers+ *)
(* +trees_prep+ *)

let p1 =
  let t1 = btree_of_string
             int_of_string
             "2(1,20(10(6(4(3,5),8(7,9)),15(12,17)),21))"
  in make_btree_picture (draw_int_node 10.0)
       (3.0,2.0) black t1;;

let p2 =
  let t2 = btree_of_string
             int_of_string
             ("15(6(4(2(1,3),5),10(8(7,9),12(11,14(13,()))))," ^
              "23(19(17(16,18),21(20,22)),25(24,26((),27))))")
  in make_btree_picture (draw_int_node 10.0)
       (3.0,2.0) black t2;;

(* +trees_prep+ *)
(* +trees+ *)

let b= make_blank_picture (100.0,100.0)
in let p=align_horizontally Align_Top [p1;b;p2]
   in printexc__f (eps_file p) "../../PS/trees";;

(* +trees+ *)
(* +strange_trees_prep+ *)

let t3 = btree_of_string int_of_string "1(2((),3((),5)),4(2(6,()),()))";;

let t4 = btree_of_string int_of_string "1(2(3(4,()),()),2(3(4,()),()))";;

(* +strange_trees_prep+ *)
(* +strange_trees+ *)

let p3 = make_btree_picture (draw_int_node 10.0)
          (3.0,2.0)
          black
          t3;;

let p4 = make_btree_picture (draw_int_node 10.0)
          (3.0,2.0)
          black
          t4;;

let b = make_blank_picture (100.0,100.0) in
let p=align_horizontally Align_Top [p3;b;p4]
in printexc__f (eps_file p) "../../PS/strange_trees";
exit 0;;

(* +strange_trees+ *)
