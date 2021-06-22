(* #use "load.ml" ;; *)

open Mlgraph
open Pavages2


(** +p1_gen+ *)
type p1_gen = TA | TB | Ta | Tb ;;

(** +p1_group+ *)
let p1_op = function
  | TA -> (function
      | [] -> [TA]
      | (((TA | TB | Tb) :: _tl) as tl') -> TA :: tl'
      | (Ta :: _) -> raise Group_gen_exc)
  | TB -> (function
      | [] -> [TB]
      | ((TB :: _tl) as tl') -> TB :: tl'
      | ((TA | Ta | Tb ) :: _) -> raise Group_gen_exc)
  | Ta -> (function
      | [] -> [Ta]
      | (((Ta | TB | Tb) :: _tl) as tl') -> Ta :: tl'
      | (TA :: _) -> raise Group_gen_exc)
  | Tb -> (function
      | [] -> [Tb]
      | ((Tb :: _tl) as tl') -> Tb :: tl'
      | ((TA | Ta | TB) :: _) -> raise Group_gen_exc) ;;

let p1_group =
  { tgroup_gens = [TA; TB; Ta; Tb] ;
    tgroup_op = p1_op } ;;

(** +motif1+ *)
let ptA1 = { xc = 300.0; yc = 300.0 } and ptB1 = { xc = 320.0; yc = 280.0 }
and ptC1 = { xc = 360.0; yc = 300.0 } and ptD1 = { xc = 340.0; yc = 350.0 } ;;
let trans1 = point_translation ptC1 ptA1 ;;
let trans2 = point_translation ptB1 ptD1 ;;
let ptE1 = transform_point trans2 ptA1 ;;
let ptF1 = transform_point trans1 ptD1 ;;
let line = make_sketch [Arc (origin, 1.0, 0.0, 40.0)] ;;
let trans3 = point_translation ptB1 ptF1 ;;

let lineAB = transform_sketch
    (handle_transform (ptB1, ptA1)
       ({ xc = 1.0; yc = 0.0 },
        { xc = cosinus 40.0; yc = sinus 40.0 }))
    line ;;
let lineBC = transform_sketch
    (handle_transform (ptC1, ptB1)
       ({ xc = 1.0; yc = 0.0 },
        { xc = cosinus 40.0; yc = sinus 40.0 }))
    line ;;
let lineCD = transform_sketch
    (handle_transform (ptD1, ptC1)
       ({ xc = 1.0; yc = 0.0 },
        { xc = cosinus 40.0; yc = sinus 40.0 }))
    line ;;
let lineAF = transform_sketch trans1 lineCD ;;
let lineFE = transform_sketch trans3 lineBC ;;
let lineED = transform_sketch trans2 lineAB ;;

let lsty = { linewidth = 1.0; linecap = Buttcap;
             linejoin = Beveljoin; dashpattern = [] } ;;

let tile1 =
  make_draw_picture (lsty, black)
    (group_sketches [ lineAB; lineBC; lineCD;
                      lineAF; lineFE; lineED ]) ;;
eps_file tile1 "../PS/motif1" ;;

(** +p1_transfo+ *)
let p1_transfo =
  let transfo =
    let trans1 = point_translation ptA1 ptC1
    and trans2 = point_translation ptA1 ptE1
    in function
      | TA -> trans1
      | TB -> trans2
      | Ta -> inverse_transformation trans1
      | Tb -> inverse_transformation trans2
  in List.fold_left (fun t t' -> comp_trans t (transfo t')) id_trans ;;

(** +pavage6_prep+ *)
let tiling6 =
  make_tiling tile1
    (List.map p1_transfo
       (generate_transformations_group p1_group 3)) ;;

(** +pavage6_prep2+ *)
let p1_transfo =
  let transfo =
    let trans1 = point_translation ptA1 ptC1
    and trans2 = point_translation ptA1 ptE1 in
    function
    | TA -> ("A", trans1)
    | TB -> ("B", trans2)
    | Ta -> ("a", inverse_transformation trans1)
    | Tb -> ("b", inverse_transformation trans2)
  in List.fold_left (fun (s, t) c
                      -> let (s', t') = transfo c
                        in (s ^ s', comp_trans t t'))
    ("", id_trans) ;;

let text s = make_text_picture
    (make_font Helvetica 12.0) black s ;;

let make_tag_tiling p trans =
  group_pictures
    (List.map (fun (s, t) ->
         let p' = transform_picture t p
         in group_pictures
           [ p'; center_picture (text s) (picture_center p') ])
        trans) ;;

let tiling6 =
  make_tag_tiling tile1
    (List.map p1_transfo
       (generate_transformations_group p1_group 3)) ;;

(** +pavage6+ *)
eps_file tiling6 "../PS/pavage6" ;;

(** +p2_gen+ *)
type p2_gen = TA | TB | TC | Ta | Tb | Tc ;;

(** +p2_group+ *)
let p2_op = function
  | TA -> (function
      | [] -> [TA]
      | (( (TA | TB | TC | Tb | Tc) :: _tl) as tl') -> TA :: tl'
      | (Ta :: _) -> raise Group_gen_exc)
  | TB -> (function
      | [] -> [TB]
      | (((TB | TC | Tc ) :: _tl) as tl') -> TB :: tl'
      | ((TA | Ta | Tb) :: _) -> raise Group_gen_exc)
  | TC -> (function
      | [] -> [TC]
      | ((TA | TB | TC | Ta | Tb | Tc) :: _) -> raise Group_gen_exc)
  | Ta -> (function
      | [] -> [Ta]
      | (((Ta | TB | TC | Tb | Tc) :: _tl) as tl') -> Ta :: tl'
      | (TA :: _) -> raise Group_gen_exc)
  | Tb -> (function
      | [] -> [Tb]
      | (((Tb | TC | Tc) :: _tl) as tl') -> Tb :: tl'
      | ((TA | Ta | TB) :: _) -> raise Group_gen_exc)
  | Tc -> (function
      | [] -> [TC]
      | ((TA | TB | TC | Ta | Tb | Tc) :: _) -> raise Group_gen_exc) ;;

let p2_group =
  { tgroup_gens = [TA; TB; TC; Ta; Tb; Tc] ;
    tgroup_op = p2_op } ;;


let middle pt1 pt2 =
  { xc = (pt1.xc +. pt2.xc) /. 2.0 ;
    yc = (pt1.yc +. pt2.yc) /. 2.0 } ;;

(** +motif2+ *)
let xunit = { xc = 1.0; yc = 0.0 } ;;
let ptA2 = { xc = 300.0; yc = 300.0 } and ptB2 = { xc = 320.0; yc = 280.0 }
and ptC2 = { xc = 360.0; yc = 300.0 } and ptD2 = { xc = 340.0; yc = 350.0 }
and ptE2 = { xc = 320.0; yc = 380.0 } ;;
let ptF2 = transform_point (point_translation ptC2 ptB2) ptE2 ;;

let alpha = 60.0 ;;
let line1 =
  make_sketch [ Arc (origin, 1.0, 0.0, alpha) ] ;;
let line2 =
  group_sketches
    [ line1; transform_sketch
        (rotation xunit 180.0) line1 ] ;;
let extrem1 = { xc = cosinus alpha; yc = sinus alpha } ;;
let extrem2 = transform_point (rotation xunit 180.0)
    extrem1 ;;

let lineAB = transform_sketch
    (handle_transform (ptA2, ptB2)
       (extrem1, extrem2))
    line2 ;;
let lineBC = transform_sketch
    (handle_transform (ptB2, ptC2)
       (extrem1, xunit))
    line1 ;;
let lineCD = transform_sketch
    (handle_transform (ptC2, ptD2)
       (extrem1, extrem2))
    line2 ;;
let lineDE = transform_sketch
    (handle_transform (ptD2, ptE2)
       (extrem1, extrem2))
    line2 ;;
let lineEF = transform_sketch
    (point_translation ptC2 ptE2)
    lineBC ;;
let lineFA = transform_sketch
    (handle_transform (ptF2, ptA2)
       (extrem1, extrem2))
    line2 ;;

let lsty = { linewidth = 1.0; linecap = Buttcap;
             linejoin = Beveljoin; dashpattern = [] } ;;

let tile2 =
  make_draw_picture (lsty, black)
    (group_sketches [ lineAB; lineBC; lineCD;
                      lineDE; lineEF; lineFA ]) ;;
eps_file tile2 "../PS/motif2" ;;


(** +p2_transfo+ *)
let p2_transfo =
  let transfo =
    let rot1 = rotation (middle ptD2 ptE2) 180.0
    and rot2 = rotation (middle ptA2 ptF2) 180.0
    and rot3 = rotation (middle ptC2 ptD2) 180.0
    in  function
      | TA  ->  comp_trans rot2 rot1
      | TB  ->  comp_trans rot2 rot3
      | TC  ->  rot2
      | Ta  ->  inverse_transformation (comp_trans rot2 rot1)
      | Tb  ->  inverse_transformation (comp_trans rot2 rot3)
      | Tc  ->  rot3

  in List.fold_left (fun t t' -> comp_trans t (transfo t')) id_trans ;;

(** +pavage7_prep+ *)
let tiling7=
  make_tiling tile2
    (List.map p2_transfo
       (generate_transformations_group p2_group 2)) ;;

(** +pavage7+ *)
let p2_transfo=
  let transfo =
    let rot1= rotation (middle ptD2 ptE2) 180.0
    and rot2= rotation (middle ptA2 ptF2) 180.0
    and rot3= rotation (middle ptC2 ptD2) 180.0
    in  function
      | TA  ->  "A", comp_trans rot2 rot1
      | TB  ->  "B", comp_trans rot2 rot3
      | TC  ->  "C", rot2
      | Ta  ->  "a", inverse_transformation (comp_trans rot2 rot1)
      | Tb  ->  "b", inverse_transformation (comp_trans rot2 rot3)
      | Tc  ->  "c", rot3

  in List.fold_left (fun (s, t) x
                      -> let s', t' = transfo x
                        in s ^ s', comp_trans t t')
    ("", id_trans) ;;

let tiling7 =
  make_tag_tiling tile2
    (List.map p2_transfo
       (generate_transformations_group p2_group 2)) ;;

eps_file tiling7 "../PS/pavage7" ;;


(** +pavage8_prep+ *)
type p2_gen' = R1 | R2 | R3 ;;

let p2_op =
  function
  | R1  -> (function
      | [] -> [R1]
      | (((R2 | R3) :: _tl) as tl') -> R1 :: tl'
      | (R1 :: _) -> raise Group_gen_exc)
  | R2  -> (function
      | [] -> [R2]
      | (((R1 | R3) :: _tl) as tl') -> R2 :: tl'
      | (R2 :: _) -> raise Group_gen_exc)
  | R3  -> (function
      | [] -> [R3]
      | (((R1 | R2) :: _tl) as tl') -> R3 :: tl'
      | (R3 :: _) -> raise Group_gen_exc) ;;
let p2_group =
  {tgroup_gens = [R1; R2; R3];
   tgroup_op = p2_op } ;;

let rec trim_list p =
  function
  | []        -> []
  | (x :: l)  -> if List.exists (fun y -> p x y) l
    then trim_list p l
    else x :: trim_list p l ;;

let simplify_p2 x = trim_list (fun x y -> List.length x >= 3 && x = List.rev y) x ;;

let p2_transfo =
  let transfo =
    function
    | R1  -> "1",rotation (middle ptD2 ptE2) 180.0
    | R2  -> "2", rotation (middle ptA2 ptF2) 180.0
    | R3  -> "3", rotation (middle ptC2 ptD2) 180.0
  in List.fold_left (fun (s, t) x
                      -> let s', t' = transfo x
                        in s ^ s', comp_trans t t')
    ("", id_trans) ;;

let tiling8=
  make_tag_tiling tile2
    (map   p2_transfo
       (simplify_p2 (generate_transformations_group p2_group 3))) ;;

(** +pavage8+ *)
eps_file tiling8 "../PS/pavage8" ;;

(** +color_for_p2_ctgroup+ *)
let ct1= [|1;0;2|] and ct2= [|2;1;0|] ;;

(** +p2_ctgroup+ *)
let p2_ctgroup =
  make_ctgroup [R1;R2;R3] [ct1;ct1;ct2] p2_op ;;

(** +p2c_transfo+ *)
let p2c_transfo=
  let transfo =
    function
    | R1  -> rotation (middle ptD2 ptE2) 180.0
    | R2  -> rotation (middle ptA2 ptF2) 180.0
    | R3  -> rotation (middle ptC2 ptD2) 180.0
  in fun (ct, tl) ->
    ct, List.fold_left (fun t t' -> comp_trans t (transfo t'))
      id_trans tl ;;

(** +simplify_p2c+ *)
let simplify_p2c x =
  trim_list (fun (_, x) (_, y) -> List.length x >= 3 && x = List.rev y) x ;;

(** +make_colored_tiling+ *)
let make_colored_tiling (c, lsty, sk) col trans =
  group_pictures
    (List.map (fun (v, t) ->
         (transform_picture
            t (group_pictures
                 [make_fill_picture (Nzfill, col v.(c)) sk ;
                  make_closed_draw_picture (lsty, black) sk ])))
        trans) ;;

(** +new_motif2+ *)
let alpha = 60.0 ;;
let line1 =
  points_of_arc (Arc (origin, 1.0, 0.0, alpha)) 4 ;;

let line2 = List.rev line1 @ (transform_points (rotation xunit 180.0) line1) ;;

let extrem1 = { xc = cosinus alpha; yc = sinus alpha } ;;
let extrem2 = transform_point (rotation xunit 180.0)
    extrem1 ;;

let lineAB = transform_points
    (handle_transform (ptA2, ptB2)
       (extrem1,extrem2))
    line2 ;;
let lineBC = transform_points
    (handle_transform (ptB2, ptC2)
       (extrem1, xunit))
    (List.rev (line1)) ;;
let lineCD = transform_points
    (handle_transform (ptC2, ptD2)
       (extrem1, extrem2))
    line2 ;;
let lineDE = transform_points
    (handle_transform (ptD2, ptE2)
       (extrem1, extrem2))
    line2 ;;
let lineEF = transform_points
    (handle_transform (ptF2, ptE2)
       (extrem1, xunit))
    line1 ;;
let lineFA = transform_points
    (handle_transform (ptF2, ptA2)
       (extrem1, extrem2))
    line2 ;;

let tile2sk =
  make_sketch [ Seg (lineAB @ lineBC @ lineCD @ lineDE @ lineEF @ lineFA) ] ;;


let color_of_int = function
  | 0 -> Gra 0.2
  | 1 -> Gra 0.4
  | 2 -> Gra 0.7
  | _ -> failwith "color_of_int: wrong int" ;;

(** +pavage9_prep+ *)
let tiling9=
  let lsty = {linewidth= 2.0;linecap=Buttcap;
              linejoin=Beveljoin;dashpattern=[]}
  in make_colored_tiling (0,lsty,tile2sk) color_of_int
    (map   p2c_transfo
       (simplify_p2c (generate_coltrans_group p2_ctgroup  4))) ;;

(** +pavage9+ *)
eps_file tiling9 "../PS/pavage9" ;;
