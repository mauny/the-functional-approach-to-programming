#open "../Util/prelude";;
#directory "../MLGRAPH.DIR";;
#open "MLgraph";;
#open "pavages1";;
#open "permutations";;


(* +motif1+ *)
let ptA1 = {xc=300.0;yc=300.0} and ptB1={xc=320.0;yc=280.0}
and ptC1 = {xc=360.0;yc=300.0} and ptD1={xc=340.0;yc=350.0};;
let trans1 = point_translation ptC1 ptA1;;
let trans2 = point_translation ptB1 ptD1;;
let ptE1 = transform_point trans2 ptA1;;
let ptF1 = transform_point trans1 ptD1;;
let line = make_sketch [Arc (origin,1.0,0.0,40.0)];;
let trans3 = point_translation ptB1 ptF1;;

let lineAB= transform_sketch
             (handle_transform (ptB1,ptA1) 
                               ({xc=1.0;yc=0.0},
                                {xc=cosinus 40.0;yc=sinus 40.0}))
              line;;
let lineBC= transform_sketch
             (handle_transform (ptC1,ptB1) 
                               ({xc=1.0;yc=0.0},
                                {xc=cosinus 40.0;yc=sinus 40.0}))
              line;;
let lineCD= transform_sketch
             (handle_transform (ptD1,ptC1) 
                               ({xc=1.0;yc=0.0},
                                {xc=cosinus 40.0;yc=sinus 40.0}))
              line;;
let lineAF= transform_sketch trans1 lineCD;;
let lineFE= transform_sketch trans3 lineBC;;
let lineED= transform_sketch trans2 lineAB;;

let lsty = {linewidth= 1.0;linecap=Buttcap;
            linejoin=Beveljoin;dashpattern=[]};;

let tile1 =
 make_draw_picture (lsty,black)
  (group_sketches [lineAB;lineBC;lineCD;
                   lineAF;lineFE;lineED]);;
eps_file tile1 "../../PS/motif1";;

(* +motif1+ *)

(* +motif1_decor+ *)
let bullet= make_fill_picture (Nzfill,black)
             (make_sketch [Arc(origin,2.0,0.0,360.0) ]);;

let north pt dist pict=
   translate_picture (0.0,dist)
        (center_picture pict pt);;
let south pt dist pict=
   translate_picture (0.0,-.dist)
        (center_picture pict pt);;
let east pt dist pict=
   translate_picture (dist,0.0)
        (center_picture pict pt);;
let west pt dist pict=
   translate_picture (-.dist,0.0)
        (center_picture pict pt);;

let text s=make_text_picture 
            (make_font Helvetica 12.0) black s;;

let tile1_decor=
  group_pictures 
   ([tile1; south ptA1 10.0 (text "A"); south ptB1 10.0 (text "B");
    south ptC1 10.0 (text "C"); east ptD1 10.0 (text "D");
    north ptE1 10.0 (text "E"); west ptF1 10.0 (text "F")]
   @ (map  (center_picture bullet) [ptA1;ptB1;ptC1;ptD1;ptE1;ptF1]));;

eps_file tile1_decor "../../PS/motif1_decor";;

(* +motif1_decor+ *)




(* +pavage1_prep+ *)
let tiling1=
    make_tiling tile1 
       [id_trans; trans1; trans2; comp_trans trans1 trans2];;

(* +pavage1_prep+ *)

(* +pavage1+ *)
eps_file tiling1 "../../PS/pavage1";;

(* +pavage1+ *)


(* +motif2+ *)
let xunit={xc=1.0;yc=0.0};;
let ptA2={xc=300.0;yc=300.0} and ptB2={xc=320.0;yc=280.0}
and ptC2={xc=360.0;yc=300.0} and ptD2={xc=340.0;yc=350.0}
and ptE2={xc=320.0;yc=380.0};;
let ptF2= transform_point (point_translation ptC2 ptB2) ptE2;;

let alpha=60.0;;
let line1= 
     make_sketch [Arc (origin,1.0,0.0,alpha)];;
let line2= 
   group_sketches
     [line1; transform_sketch
                (rotation xunit 180.0) line1];;
let extrem1= {xc=cosinus alpha;yc=sinus alpha};;
let extrem2= transform_point (rotation xunit 180.0)
                 extrem1;;


let lineAB= transform_sketch
             (handle_transform (ptA2,ptB2) 
                               (extrem1,extrem2))
              line2;;
let lineBC= transform_sketch
             (handle_transform (ptB2,ptC2) 
                               (extrem1,xunit))
              line1;;
let lineCD= transform_sketch
             (handle_transform (ptC2,ptD2) 
                               (extrem1,extrem2))
              line2;;
let lineDE= transform_sketch
             (handle_transform (ptD2,ptE2) 
                               (extrem1,extrem2))
              line2;;
let lineEF= transform_sketch
             (point_translation ptC2 ptE2)
              lineBC;;
let lineFA= transform_sketch
             (handle_transform (ptF2,ptA2) 
                               (extrem1,extrem2))
              line2;;

let lsty = {linewidth= 1.0;linecap=Buttcap;
            linejoin=Beveljoin;dashpattern=[]};;

let tile2 =
 make_draw_picture (lsty,black)
  (group_sketches [lineAB;lineBC;lineCD;
                   lineDE;lineEF;lineFA]);;
eps_file tile2 "../../PS/motif2";;

(* +motif2+ *)



(* +motif2_decor+ *)
let tile2_decor=
  group_pictures 
   ([tile2; south ptA2 10.0 (text "A"); south ptB2 10.0 (text "B");
    south ptC2 10.0 (text "C"); east ptD2 10.0 (text "D");
    north ptE2 10.0 (text "E"); west ptF2 10.0 (text "F")]
   @ (map  (center_picture bullet) [ptA2;ptB2;ptC2;ptD2;ptE2;ptF2]));;

eps_file tile2_decor "../../PS/motif2_decor";;

(* +motif2_decor+ *)

(* +rot1_rot2_rot3+ *)
let middle pt1 pt2 =
  {xc=(pt1.xc +. pt2.xc) /. 2.0;
   yc=(pt1.yc +. pt2.yc) /. 2.0};;

let rot1 = rotation (middle ptD2 ptE2) 180.0
and rot2 = rotation (middle ptA2 ptF2) 180.0
and rot3 = rotation (middle ptC2 ptD2) 180.0;;

(* +rot1_rot2_rot3+ *)


(* +pavage2_prep+ *)
let tiling2 =
    make_tiling tile2 [id_trans;rot1;rot2;rot3];;

(* +pavage2_prep+ *)

(* +pavage2+ *)
eps_file (group_pictures [tiling2;
                          center_picture bullet (middle ptD2 ptE2);
                          center_picture bullet (middle ptA2 ptF2);
                          center_picture bullet (middle ptC2 ptD2);
                          east (middle ptD2 ptE2) 10.0 (text "1");
                          west (middle ptA2 ptF2) 10.0 (text "2");
                          east (middle ptC2 ptD2) 10.0 (text "3")])
          "../../PS/pavage2";;

(* +pavage2+ *)

(* +pavage3_prep+ *)
let tiling3 =
  make_tiling tile2 (generate_transformations_group
                        [rot1;rot2;rot3] 3);;

(* +pavage3_prep+ *)

(* +pavage3+ *)
eps_file tiling3 "../../PS/pavage3";;

(* +pavage3+ *)




(* +pavage4_prep+ *)
let tiling4 =
   let tile = group_pictures 
               [tile2; transform_picture rot3 tile2]
   and tr1 = comp_trans rot3 rot2
   and tr2 = comp_trans rot1 rot3 in
   let tile' = make_tiling tile (generate_transformations_group [tr1] 3)
   in make_tiling
        tile' (generate_transformations_group [tr2] 3);;

(* +pavage4_prep+ *)

(* +pavage4+ *)
eps_file tiling4 "../../PS/pavage4";;

(* +pavage4+ *)

(* +new_motif2+ *)

let alpha = 60.0;;
let line1 = 
     points_of_arc (Arc (origin,1.0,0.0,alpha)) 4;;

let line2 = rev line1 @ (transform_points (rotation xunit 180.0) line1);;

let extrem1 = {xc= cosinus alpha; yc= sinus alpha};;
let extrem2 = transform_point (rotation xunit 180.0)
                 extrem1;;


let lineAB = transform_points
             (handle_transform (ptA2, ptB2) 
                               (extrem1,extrem2))
              line2;;
let lineBC = transform_points
             (handle_transform (ptB2, ptC2) 
                               (extrem1,xunit))
              (rev(line1));;
let lineCD = transform_points
             (handle_transform (ptC2, ptD2) 
                               (extrem1,extrem2))
              line2;;
let lineDE = transform_points
             (handle_transform (ptD2, ptE2) 
                               (extrem1,extrem2))
              line2;;
let lineEF =  transform_points
                  (handle_transform (ptF2, ptE2)
                               (extrem1,xunit))
                 line1;;
let lineFA = transform_points
             (handle_transform (ptF2, ptA2) 
                               (extrem1,extrem2))
              line2;;



let tile2sk =
 make_sketch [Seg (lineAB@lineBC@lineCD@lineDE@lineEF@lineFA)];;

(* +new_motif2+ *)
(* +color_for_pavage5+ *)

let color_of_int =fun
  0 -> Gra 0.2
| 1 -> Gra 0.4
| 2 -> Gra 0.7
| _ -> failwith "color_of_int: wrong int";;
 
let ct1 = [|1; 0; 2|] and ct2 = [|2; 1; 0|];;

(* +color_for_pavage5+ *)



(* +pavage5_prep+ *)
let tiling5 =
  let lsty = {linewidth= 2.0; linecap=Buttcap;
              linejoin=Beveljoin; dashpattern=[]}
  in make_colored_tiling (0, lsty, tile2sk) color_of_int
        (generate_coltrans_group  
               [(ct1,rot1);(ct1,rot2);(ct2,rot3)] 4);;

(* +pavage5_prep+ *)

(* +pavage5+ *)
eps_file tiling5 "../../PS/pavage5";;

(* +pavage5+ *)


(* +pavage_cp4+ *)

let ptA4 = {xc=300.0; yc=300.0} and ptB4={xc=325.0; yc=325.0};;
let ptC4 = {xc=300.0; yc=350.0} and ptD4={xc=350.0; yc=300.0};;
let rot1 = rotation ptA4 90.0;;
let rot2 = rotation ptB4 180.0;;


let alpha = 40.0;;

let line1 = 
     points_of_arc (Arc (origin, 1.0, 0.0, alpha)) 4;;

let line2 = rev line1 @ (transform_points (rotation xunit 180.0) line1);;

let extrem1 = {xc=cosinus alpha; yc=sinus alpha};;
let extrem2 = transform_point (rotation xunit 180.0)
                 extrem1;;


let lineAC = transform_points
             (handle_transform (ptA4, ptC4) 
                               (xunit, extrem1))
              line1;;
let lineAD = transform_points
             (handle_transform (ptA4, ptD4) 
                               (xunit, extrem1))
              line1;;
let lineCD = transform_points
             (handle_transform (ptC4, ptD4) 
                               (extrem1, extrem2))
              line2;;

let tile4sk =
 make_sketch [Seg (lineAC@lineCD@(rev lineAD))];;

let c2 = Gra 0.5;;

let tile4 =
 make_fill_picture (Nzfill, c2)  tile4sk ;;
eps_file tile4 "../../PS/motif4";;

let color_of_int =
fun  0 ->  Gra 0.2 
 |   1 -> Gra 0.4 
 |   2 -> Gra 0.6
 |   3 -> Gra 0.8
 |   _ -> failwith "color_of_int: wrong int";;
 
let ct1= [|1;2;3;0|] and ct2= [|1;0;3;2|];;

let close_enough d 
  {m11=m11;m12=m12;m13=m13;m21=m21;m22=m22;m23=m23}
  {m11=m11';m12=m12';m13=m13';m21=m21';m22=m22';m23=m23'}
  =
  abs_float (m11-. m11') <. d &
  abs_float (m12-. m12') <. d &
  abs_float (m13-. m13') <. d &
  abs_float (m21-. m21') <. d &
  abs_float (m22-. m22') <. d &
  abs_float (m23-. m23') <. d ;;


let rec sol =
  fun  []  ->  []
    |    (((ct,t) as a)::l) -> if exists (fun x -> close_enough 0.1 t (snd x)) l
                      then sol l
                      else a::sol l;;


let power' f x l n =  sol (power f x l n);;

let generate_coltrans_group' trl n=
  if trl=[] then []
    else let k= vect_length (fst(hd trl))
    in set_of_list
           (power' (pairing cpermut comp_trans)
           (id_permut k,id_trans)  trl n);;


let tiling_cp4=
  let lsty = {linewidth= 2.0;linecap=Buttcap;
              linejoin=Beveljoin;dashpattern=[]}
  in make_colored_tiling (0,lsty,tile4sk) color_of_int
        (generate_coltrans_group'
               [ct1,rot1;ct2,rot2] 11);;
let sk= frame_sketch {xmin=150.;xmax=500.;ymin=150.;ymax=500.};;

eps_file (clip_picture Nzclip sk tiling_cp4) "../../PS/pavage_cp4";;

(* +pavage_cp4+ *)
