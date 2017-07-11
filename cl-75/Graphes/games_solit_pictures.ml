#directory "../MLGRAPH.DIR";;
#open "MLgraph";;
#directory "../Util";;
#directory "../Arbres";;
#open "prelude";;
#open "binary_trees";;
#open "sets";;
#open "games";;
#open "games_solit";;


let Pi= acos (-1.);;

(*  Dessin du Jeu    *)

let hole_dist = 40.0;;
let ext= 20.0;;
let hole_size = 8.0;;

let black_hole=
  make_fill_picture (Nzfill,black)
      (make_sketch [Arc(origin,hole_size,0.0,360.0)]);;


let white_hole=
  make_draw_picture ({linewidth=1.0;linejoin=Miterjoin;
                      linecap=Buttcap;dashpattern=[]},black)
      (make_sketch [Arc(origin,hole_size,0.0,360.0)]);;
      
let spec_hole=
  group_pictures
    [black_hole;
     center_picture (make_fill_picture (Nzfill,white)
                         (make_sketch [Arc(origin,hole_size/.3.0,0.0,360.0)]))
                    (picture_center black_hole)];;

let blank= make_blank_picture (0.0,0.0);;
let draw_block l =
 translate (50.0,50.0)
  (group_pictures
      (map (fun(pos,b)-> 
              translate ((float_of_int(pos mod 10-1))*. hole_dist,
                         (7.0-.float_of_int(pos / 10))*. hole_dist)
                        (if b=1 then  black_hole else 
                        if b=0 then white_hole else 
                        if b=3 then spec_hole else blank))
      (select (fun(pos,b) -> b=0 or b=1 or b=3)  
        (combine([13;14;15;23;24;25;31;32;
                  33;34;35;36;37;41;42;43;
                  44;45;46;47;51;52;53;54;
                  55;56;57;63;64;65;73;74;75]
                 ,l)))));;

let block1_pict = 
 draw_block  [2;2;2;
              0;2;2;
          1;1;0;2;2;2;2;
          1;1;3;2;2;2;2;
          1;1;2;2;2;2;2;
              2;2;2;
              2;2;2];;

let block2_pict = 
 draw_block  [2;2;2;
              3;2;2;
          1;1;0;2;2;2;2;
          1;1;0;2;2;2;2;
          1;1;2;2;2;2;2;
              2;2;2;
              2;2;2];;

let block3_pict = 
 draw_block  [2;2;2;
              2;2;2;
          1;1;0;3;2;2;2;
          1;1;2;2;2;2;2;
          1;1;2;2;2;2;2;
              2;2;2;
              2;2;2];;

let block4_pict = 
 draw_block  [2;2;2;
              2;2;2;
          2;2;1;2;2;2;2;
          2;3;1;0;2;2;2;
          2;2;1;2;2;2;2;
              1;1;1;
              2;2;2];;

let block5_pict = 
 draw_block  [2;2;2;
              2;2;2;
          2;2;1;2;2;2;2;
          2;0;1;3;2;2;2;
          2;2;1;2;2;2;2;
              1;1;1;
              2;2;2];;

let back_pict = 
  let back_sk =
      make_sketch
       [Seg [{xc= -.ext;yc= -.ext};{xc=6.0*.hole_dist+.ext; yc= -.ext};
             {xc=6.0*.hole_dist+.ext; yc=6.0*.hole_dist+.ext};
             {xc= -.ext; yc=6.0*.hole_dist+.ext};{xc= -.ext;yc= -.ext}]]
  in group_pictures
       [make_fill_picture (Eofill,Gra 0.8) back_sk;
        make_draw_picture ({linewidth=3.0;linejoin=Miterjoin;
                            linecap=Buttcap;dashpattern=[]},black)
                           back_sk];;


let french_back_pict = 
  let back_sk =
    let d=(3.0*.hole_dist+.ext)*.tan (Pi/.8.0) in
      make_sketch
       [Seg [{xc= -.ext;yc=3.0*.hole_dist+.d};
             {xc=3.0*.hole_dist-.d; yc=6.0*.hole_dist+.ext};
             {xc=3.0*.hole_dist+.d; yc=6.0*.hole_dist+.ext};
             {xc=6.0*.hole_dist+.ext;yc=3.0*.hole_dist+.d};
             {xc=6.0*.hole_dist+.ext;yc=3.0*.hole_dist-.d};
             {xc=3.0*.hole_dist+.d; yc= -.ext};
             {xc=3.0*.hole_dist-.d; yc= -.ext};
             {xc= -.ext;yc=3.0*.hole_dist-.d};
             {xc= -.ext;yc=3.0*.hole_dist+.d}]]
  in group_pictures
       [make_fill_picture (Eofill,Gra 0.8) back_sk;
        make_draw_picture ({linewidth=3.0;linejoin=Miterjoin;
                            linecap=Buttcap;dashpattern=[]},black)
                           back_sk];;

let solitaire_anglais=
 translate (50.0,50.0)
  (group_pictures
      (back_pict :: map (fun(pos,b)-> 
                          translate ((float_of_int(pos mod 10-1))
                                      *. hole_dist,
                                     (7.0-.float_of_int(pos / 10))
                                      *. hole_dist)
                                      (if b=1 then  black_hole
                                              else  white_hole))
                          (combine([13;14;15;23;24;25;31;32;
                                    33;34;35;36;37;41;42;43;
                                    44;45;46;47;51;52;53;54;
                                    55;56;57;63;64;65;73;74;75]
                                  ,[0;0;0;0;0;0;0;0;
                                    0;0;0;0;0;0;0;0;
                                    0;0;0;0;0;0;0;0;
                                    0;0;0;0;0;0;0;0;0]))));;



let text size n =make_text_picture 
            (make_font Helvetica size) black (string_of_int n);;

let solit_num_pict=
 translate (50.0,50.0)
  (group_pictures
      (back_pict :: map (fun(pos,n)-> 
                          translate ((float_of_int(pos mod 10-1))
                                      *. hole_dist,
                                     (7.0-.float_of_int(pos / 10))
                                      *. hole_dist)
                                      (text 12.0 n))
                          (combine([13;14;15;23;24;25;31;32;
                                    33;34;35;36;37;41;42;43;
                                    44;45;46;47;51;52;53;54;
                                    55;56;57;63;64;65;73;74;75]
                                  ,[13;14;15;23;24;25;31;32;
                                    33;34;35;36;37;41;42;43;
                                    44;45;46;47;51;52;53;54;
                                    55;56;57;63;64;65;73;74;75]))));;

let solit_num_pict_french=
 translate (50.0,50.0)
  (group_pictures
      (back_pict :: map (fun(pos,n)-> 
                          translate ((float_of_int(pos mod 10-1))
                                      *. hole_dist,
                                     (7.0-.float_of_int(pos / 10))
                                      *. hole_dist)
                                      (text 12.0 n))
                          (combine([13;14;15;22;23;24;25;26;31;32;
                                    33;34;35;36;37;41;42;43;
                                    44;45;46;47;51;52;53;54;
                                    55;56;57;62;63;64;65;66;73;74;75]
                                  ,[13;14;15;22;23;24;25;26;31;32;
                                    33;34;35;36;37;41;42;43;
                                    44;45;46;47;51;52;53;54;
                                    55;56;57;62;63;64;65;66;73;74;75]))));;

let solit_partition =
 translate (50.0,50.0)
  (group_pictures
      (french_back_pict :: map (fun(pos,n)-> 
                          translate ((float_of_int(pos mod 10-1))
                                      *. hole_dist,
                                     (7.0-.float_of_int(pos / 10))
                                      *. hole_dist)
                                      (text 12.0 n))
                          (combine([13;14;15;22;23;24;25;26;31;32;
                                    33;34;35;36;37;41;42;43;
                                    44;45;46;47;51;52;53;54;
                                    55;56;57;62;63;64;65;66;73;74;75]
                                  ,[ 1; 2; 3; 1; 2; 3; 1; 2; 1; 2;
                                     3; 1; 2; 3; 1; 2; 3; 1;
                                     2; 3; 1; 2; 3; 1; 2; 3;
                                     1; 2; 3; 2; 3; 1; 2; 3; 1; 2; 3]))));;

let conv n=
if n<= 12 then failwith "conv: wrong arg" else
if n <= 15 then n-13 else
 if n<=25 then n-20 else
  if n<= 37 then n-25 else
   if n<=47 then n-28 else
    if n<=57 then n-31 else
     if n<= 65 then n-36 else
      if n<= 75 then n-43 else failwith "conv: wrong arg";;

let solit_intern_num_pict=
 translate (50.0,50.0)
  (group_pictures
      (back_pict :: map (fun(pos,n)-> 
                          translate ((float_of_int(pos mod 10-1))
                                      *. hole_dist,
                                     (7.0-.float_of_int(pos / 10))
                                      *. hole_dist)
                                      (text 12.0 n))
                          (combine([13;14;15;23;24;25;31;32;
                                    33;34;35;36;37;41;42;43;
                                    44;45;46;47;51;52;53;54;
                                    55;56;57;63;64;65;73;74;75]
                                  ,map conv [13;14;15;23;24;25;31;32;
                                    33;34;35;36;37;41;42;43;
                                    44;45;46;47;51;52;53;54;
                                    55;56;57;63;64;65;73;74;75]))));;




let draw_config c =
  let l= combine([13;14;15;23;24;25;31;32;33;34;35;36;37;41;42;43;44;
                  45;46;47;51;52;53;54;55;56;57;63;64;65;73;74;75]
                 ,uncompact c)
                 
  in group_pictures
      (back_pict :: map (fun(pos,b)-> 
                          translate ((float_of_int(pos mod 10-1))
                                      *. hole_dist,
                                     (7.0-.float_of_int(pos / 10))
                                      *. hole_dist)
                                      (if b=1 then  black_hole
                                              else  white_hole))
                          l);;

let solit_start_pict = translate (50.0,50.0)
                          (draw_config start);; 

let solit_end_pict = translate (50.0,50.0)
                          (draw_config b4);; 

let solit_double_pict = 
   translate (50.0,50.0)
      (align_horizontally Align_Center
         [extend_picture_frame Right_ext 0.2 (draw_config start);
          draw_config b4]);; 
          
let solit_sext1_pict = 
   translate (50.0,50.0)
      (align_horizontally Align_Center
         [extend_picture_frame Right_ext 0.2 (draw_config b1);
          draw_config b2]);; 

let draw_config_french l =
translate (50.0,50.0)
  (group_pictures
      (french_back_pict :: map (fun(pos,b)-> 
                          translate ((float_of_int(pos mod 10-1))
                                      *. hole_dist,
                                     (7.0-.float_of_int(pos / 10))
                                      *. hole_dist)
                                      (if b=1 then  black_hole
                                              else  white_hole))
                               l));;

let solitaire_francais=
  draw_config_french      (combine([13;14;15;22;23;24;25;26;31;32;
                                    33;34;35;36;37;41;42;43;
                                    44;45;46;47;51;52;53;54;
                                    55;56;57;62;63;64;65;66;73;74;75]
                                  ,[0;0;0;0;0;0;0;0;0;0;
                                    0;0;0;0;0;0;0;0;
                                    0;0;0;0;0;0;0;0;
                                    0;0;0;0;0;0;0;0;0;0;0]));;

let solit_french_start_pict=
  draw_config_french      (combine([13;14;15;22;23;24;25;26;31;32;
                                    33;34;35;36;37;41;42;43;
                                    44;45;46;47;51;52;53;54;
                                    55;56;57;62;63;64;65;66;73;74;75]
                                  ,[1;1;1;1;1;1;1;1;1;1;
                                    1;1;1;1;1;1;1;1;
                                    0;1;1;1;1;1;1;1;
                                    1;1;1;1;1;1;1;1;1;1;1]));;

let solit_french_end_pict=
  draw_config_french      (combine([13;14;15;22;23;24;25;26;31;32;
                                    33;34;35;36;37;41;42;43;
                                    44;45;46;47;51;52;53;54;
                                    55;56;57;62;63;64;65;66;73;74;75]
                                  ,[0;0;0;0;0;0;0;0;0;0;
                                    0;0;0;0;0;0;0;0;
                                    1;0;0;0;0;0;0;0;
                                    0;0;0;0;0;0;0;0;0;0;0]));;

          
let solit_double_pict_french = 
   translate (50.0,50.0)
      (align_horizontally Align_Center
         [extend_picture_frame Right_ext 0.2 solit_french_start_pict;
          solit_french_end_pict]);; 
          

         
eps_file solitaire_anglais "../../PS/solitaire_anglais";;
eps_file solitaire_francais "../../PS/solitaire_francais";;

eps_file solit_start_pict "../../PS/solit_start_pict";;
eps_file solit_end_pict "../../PS/solit_end_pict";;
eps_file solit_double_pict "../../PS/solit_double_pict";;
eps_file solit_double_pict_french "../../PS/solit_double_pict_french";;
eps_file solit_partition "../../PS/solit_partition";;
eps_file solit_sext1_pict "../../PS/solit_sext1_pict";;

eps_file solit_intern_num_pict "../../PS/solit_intern_num_pict";;
eps_file solit_num_pict "../../PS/solit_num_pict";;
eps_file solit_num_pict_french "../../PS/solit_num_pict_french";;

eps_file block1_pict "../../PS/solit_block1_pict";;
eps_file block2_pict "../../PS/solit_block2_pict";;
eps_file block3_pict "../../PS/solit_block3_pict";;
eps_file block4_pict "../../PS/solit_block4_pict";;
eps_file block5_pict "../../PS/solit_block5_pict";;

eps_file (align_horizontally Align_Center 
          [extend_picture_frame Right_ext 0.5 block2_pict;
          block3_pict])
         "../../PS/solit_block23_pict";;
eps_file (align_horizontally Align_Center 
          [extend_picture_frame Right_ext 0.5 block4_pict;
          block5_pict])
         "../../PS/solit_block45_pict";;

