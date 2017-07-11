
(*  Dessin du Jeu    *)
#directory "../MLGRAPH.DIR";;
#open "MLgraph";;
#directory "../Util";;
#directory "../Arbres";;
#open "option";;
#open "graph";;
#open "prelude";;
#open "binary_trees";;
#open "sets";;
#open "games";;
#open "games_ane_rouge";;
#infix "o";;


let size = 40.0;;
let interspace = 2.5;;
let ext= 5.0;;
let dot_size = 5.0;;
let back_color= Gra 0.1;;
let side_color= Gra 0.4;;
let piece_color= Gra 0.8;;
let dot_color= Gra 0.1;;


let dot=
  make_fill_picture (Nzfill,dot_color)
      (make_sketch [Arc(origin,dot_size,0.0,360.0)]);;


let square_sk = make_sketch
  [Seg [origin;{xc=size-.interspace; yc=0.0};
       {xc=size-.interspace; yc=size-.interspace};
       {xc=0.0; yc=size-.interspace};origin]];;

let square_pict=
 let p= make_fill_picture (Nzfill,piece_color) square_sk
 in group_pictures
  [p;center_picture dot (picture_center p)];;
  
let slot_pict=
 make_fill_picture (Nzfill,piece_color) square_sk;;


let def_sk = make_sketch
  [Seg [origin;{xc=size-.interspace; yc=0.0};
       {xc=size-.interspace; yc=2.0*.size-.interspace};
       {xc=0.0; yc=2.0*.size-.interspace};origin]];;

let vertic_pict=
 let p= make_fill_picture (Nzfill,piece_color) def_sk
 in group_pictures
  [p;center_picture dot (picture_center p)];;
  
let horiz_pict=
 let p= rotate_picture 90.0
    (make_fill_picture (Nzfill,piece_color) def_sk)
 in group_pictures
  [p;center_picture dot (picture_center p)];;

  
let donkey_sk = make_sketch
  [Seg [origin;{xc=2.0*.size-.interspace; yc=0.0};
       {xc=2.0*.size-.interspace; yc=2.0*.size-.interspace};
       {xc=0.0; yc=2.0*.size-.interspace};origin]];;

let donkey_pict=
 let p= make_fill_picture (Nzfill,piece_color) donkey_sk
 in group_pictures
  [p;center_picture dot (picture_center p)];;

let thai_elements =
  let fnt= make_font Helvetica 10.0
  in let [txt1;txt2;txt3;txt4]
       = map (fun s -> extend_picture_frame Top_ext 2.0
                         (make_text_picture fnt black s))
             ["Donkey";"Horiz";"Vertic";"Square"]
     in align_horizontally Align_Bottom
          [extend_picture_frame Right_ext 0.5 
            (align_vertically Align_Center [donkey_pict; txt1]); 
           extend_picture_frame Right_ext 0.5 
            (align_vertically Align_Center [horiz_pict; txt2]);
           extend_picture_frame Right_ext 1.0 
            (align_vertically Align_Center [vertic_pict; txt3]); 
           extend_picture_frame Right_ext 1.0 
            (align_vertically Align_Center [square_pict; txt4])];;

eps_file thai_elements "../../PS/thai_elements";;
         
 
let board_sk = make_sketch
  [Seg [origin;{xc=4.0*.size; yc=0.0};
       {xc=4.0*.size; yc=5.0*.size};
       {xc=0.0; yc=5.0*.size};origin]];;

let board_ext_sk =make_sketch
  [Seg [origin;{xc=4.0*.size+.2.0*.ext; yc=0.0};
       {xc=4.0*.size+.2.0*.ext; yc=5.0*.size+.2.0*.ext};
       {xc=0.0; yc=5.0*.size+.2.0*.ext};origin]];;


let board_pict =
  group_pictures 
        [center_picture
           (make_fill_picture (Nzfill,side_color) board_ext_sk)
           {xc=2.0*.size; yc=2.5*.size};
         center_picture
           (make_fill_picture (Nzfill,back_color) board_sk)
           {xc=2.0*.size; yc=2.5*.size}];;
           
let grid_pict=
  group_pictures 
   (map (make_draw_picture 
           ({linewidth=2.0;linecap=Squarecap;
             linejoin=Beveljoin;dashpattern=[]},
            black))
     ( map make_sketch 
         [[Seg [{xc=size;yc=0.0}; {xc=size;yc=5.0*.size}]];
          [Seg [{xc=2.0*.size;yc=0.0}; {xc=2.0*.size;yc=5.0*.size}]];
          [Seg [{xc=3.0*.size;yc=0.0}; {xc=3.0*.size;yc=5.0*.size}]];
          [Seg [{xc=0.0;yc=size}; {xc=4.0*.size;yc=size}]];
          [Seg [{xc=0.0;yc=2.0*.size}; {xc=4.0*.size;yc=2.0*.size}]];
          [Seg [{xc=0.0;yc=3.0*.size}; {xc=4.0*.size;yc=3.0*.size}]];
          [Seg [{xc=0.0;yc=4.0*.size}; {xc=4.0*.size;yc=4.0*.size}]]]));;

let board_grid_pict =
 let center = {xc=2.0*.size; yc=2.5*.size}
 in group_pictures 
        [center_picture
           (make_fill_picture (Nzfill,side_color) board_ext_sk)
           center;
         center_picture
           (make_fill_picture (Nzfill,Gra 0.9) board_sk)
           center;
         center_picture grid_pict center];;

eps_file board_grid_pict "../../PS/board_grid_pict";;

let compute_center (p,n) =
 let (n1,n2)=(n / 10, n mod 10)
 in match p
    with Donkey
   -> {xc=size*.float_of_int(n2); yc=size*.float_of_int(5-n1)}
     |  Square
   -> {xc=size*.(float_of_int(n2)-.0.5); 
       yc=size*.(float_of_int(5-n1)+.0.5)}
     |  Horiz
   -> {xc=size*.(float_of_int(n2)); 
       yc=size*.(float_of_int(5-n1)+.0.5)}
     |  Vertic
   -> {xc=size*.(float_of_int(n2)-.0.5); 
       yc=size*.(float_of_int(5-n1))}
     |  None -> origin;;
 
 

let thai_board_num =
 let fnt = make_font Helvetica 10.0
 in 
 translate_picture  (100.0, 300.0)
  (group_pictures
   (board_grid_pict
     ::map (fun n
            -> center_picture
                 (make_text_picture fnt black 
                        (string_of_int n))
                 (compute_center (Square,n)))
            
[11;12;13;14;21;22;23;24;31;32;33;34;41;42;43;44;51;52;53;54]));;

eps_file thai_board_num "../../PS/thai_board_num_pict";;





let draw_config c=
 translate_picture  (100.0, 300.0)
  (group_pictures
   ( (board_pict
     ::center_picture donkey_pict 
           (compute_center (Donkey,c.donkey))
     ::center_picture horiz_pict 
           (compute_center (Horiz,c.horiz))
   ::map (fun p -> center_picture vertic_pict 
           (compute_center (Vertic,p))) c.vertics)
   @  map (fun p -> center_picture square_pict 
           (compute_center (Square,p))) c.squares));;

  

(*
         
let app_move (b'',d) ((b,b'),board) =
 if b''=b then
   snd (find (fun (ml,((b,b'),board))-> snd (hd ml)=d)
        (moves1 ([],((b,b'),board)) append moves2 ([],((b,b'),board))))
    else if b''=b' then
   snd (find (fun (ml,((b,b'),board))-> snd (hd ml)=d)
        (moves1 ([],((b',b),board))))
        else failwith "wrong move";;



           
let app_moves ml = list_it app_move (rev ml);;
*)


let rec make_conf_seq ml cl=
 match (ml,cl)
 with  ([],cl)  ->    rev cl
 |  (m::ml',c::cl')  
     ->  make_conf_seq ml' (app_move c m ::cl);;

let mlex= [31,Left; 34,Down];;
let [c1;c2;c3] = make_conf_seq mlex [start];;

let arrow_pict color n =
  scale_picture (n/.13.0,n/.13.0)
    (make_fill_picture (Eofill,color)
       (make_sketch [Seg[origin;{xc=10.0; yc=0.0};
                         {xc=10.0; yc= -0.5};{xc=13.0; yc=0.5};
                         {xc=10.0; yc=1.5};{xc=10.0; yc=1.0};
                         {xc=0.0; yc=1.0};origin]]));;

let text_on_arrow fnt color s=
  let t= make_text_picture fnt color s
  and w= text_width fnt s
  in
 align_vertically Align_Left
   [t; 
    extend_picture_frame Top_ext 0.5
      (arrow_pict color (w*.1.5))];;

let texts_on_arrows fnt color sl =
  let w = it_list (fun x s -> let  w= text_width fnt s
                              in if w>.x then w else x)
                  0.0 sl
  in let arrow = extend_picture_frame Top_ext 0.5
                       (arrow_pict color (w*.1.5))
     in map (fun s ->
                align_vertically Align_Left 
                  [make_text_picture fnt color s; arrow])
            sl;;


let thai_moves =
 let fnt=make_font Helvetica 12.0
 and blank = make_blank_picture (10.0,10.0)
 in
 let [arrow1;arrow2] = texts_on_arrows fnt black ["(31,Left)";"(34,Down)"]
 in
  align_horizontally Align_Center
    [draw_config (snd c1); blank;arrow1; blank;
     draw_config (snd c2); blank;arrow2; blank;
     draw_config (snd c3)]

;;
eps_file thai_moves "../../PS/thai_moves_pict";;


let ml=
[31,Left ; 34,Down ; 33,Left ; 24,Down ; 34,Down ; 14,Right ; 12,Right ; 
11,Up ; 21,Up ; 32,Left ; 31,Left ; 33,Left ; 32,Left ; 33,Down ; 13,Right 
; 12,Right ; 14,Right ; 13,Right ; 11,Up ; 21,Up ; 22,Right ; 31,Up ; 
21,Up ; 41,Left ; 42,Down ; 32,Down ;
52,Down ; 42,Down ; 22,Left ; 34,Up ; 24,Up ; 44,Right ; 43,Right ; 
53,Down ; 42,Down ; 23,Down ; 13,Left ; 22,Left ; 23,Down ; 13,Right ; 
14,Up ; 11,Up ; 
34,Up ; 31,Up ; 54,Right ; 51,Left ; 52,Down ] ;;

let thai_end =app_moves start ml;;

let thai_start_pict= draw_config (snd start);;
let thai_end_pict= draw_config (snd thai_end);;
let thai_start_pict2= draw_config (snd start2);;
let thai_small_cc_pict= draw_config (snd small_cc);;

let thai_picts =
    map (fun c -> draw_config (snd c)) (make_conf_seq ml [start]);;
    
let rec nfirst l n=
  if n=0 then [] else (hd l)::nfirst (tl l) (n-1);;
let rec sub_list l m n=
  if m=0 then nfirst l (n+1)
         else sub_list (tl l) (m-1)( n-1);;
         
         
let make_row_picture =
fun   [] ->  make_blank_picture (0.0,0.0)
|  (p::pl) -> align_horizontally Align_Center
                  (p::map (extend_picture_frame Left_ext  0.2) pl)
;;


let make_column_picture =
fun   [] ->  make_blank_picture (0.0,0.0)
|  (p::pl) -> align_vertically Align_Left
                  (p::map (extend_picture_frame Top_ext 0.2) pl)
;;

let thai_pict1=
  translate_picture  (-.90.0,800.0)
  (scale_picture (0.5,0.5)
   (make_column_picture
     [make_row_picture (sub_list thai_picts 0 4);
      make_row_picture (sub_list thai_picts 5 9);
      make_row_picture (sub_list thai_picts 10 14);
      make_row_picture (sub_list thai_picts 15 19);
      make_row_picture (sub_list thai_picts 20 24)]))
;;

let thai_pict2=
  translate_picture  (-.90.0,800.0)
  (scale_picture (0.5,0.5)
   (make_column_picture
     [make_row_picture (sub_list thai_picts 25 29);
      make_row_picture (sub_list thai_picts 30 34);
      make_row_picture (sub_list thai_picts 35 39);
      make_row_picture (sub_list thai_picts 40 44);
      make_row_picture (sub_list thai_picts 45 47)]))
;;

 eps_file thai_pict1 "../../PS/thai_pict1";;  
 eps_file thai_pict2 "../../PS/thai_pict2";;  
 eps_file thai_start_pict "../../PS/thai_start_pict";;  
 eps_file thai_end_pict "../../PS/thai_end_pict";;  
 eps_file thai_start_pict2 "../../PS/thai_start_pict2";;  
 eps_file thai_small_cc_pict "../../PS/thai_small_cc_pict";;  



let pos_moves ((b1,b2),c) =
  moves1 b1 c @ moves1 b2 c @ moves2 (b1,b2) c;;

let [m1;m2;m3;m4;m5;m6] = pos_moves start;;

let [c1;c2;c3;c4;c5;c6] = map (fun m -> app_move start m)
                              [m1;m2;m3;m4;m5;m6];;

let c1=start;;
let c2= app_move c1 (31,Left);;
let c3= app_move c1 (34,Down);;
let c4= app_move c3 (31,Left);;
let c4' = app_move c2 (34,Down);;

let [p1;p2;p3;p4] = map (draw_config o snd) [c1;c2;c3;c4];;


let debut_graphe= 
assembleGraphs [] ["c1";"c2";"c3";"c4"]
  [ [string "arrowDir" "F"], "c1" , Dsw, "c2";
    [string "arrowDir" "F"], "c1" , Dse ,"c3";
    [string "arrowDir" "F"], "c2" , Dse ,"c4";
    [string "arrowDir" "F"], "c2" , Dse ,"c4";
    [string "arrowDir" "F"], "c3" , Dsw ,"c4";
    [string "arrowDir" "F"], "c2" , Dn ,"c1";
    [string "arrowDir" "F"], "c3" , Dn ,"c1";
    [string "arrowDir" "F"], "c4" , Dw ,"c2";
    [string "arrowDir" "F"], "c4" , De ,"c3"
  ];;

let debut_graphe_pict= 
  scale_picture (0.1,0.1) 
       (graphGen [float "lineWidthCoef" 1.0] 
                 debut_graphe
                 (combine (["c1";"c2";"c3";"c4"], [p1;p2;p3;p4])));;

eps_file debut_graphe_pict "../../PS/debut_thai_graphe";;



