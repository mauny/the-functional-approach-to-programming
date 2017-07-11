#directory "../Util";;
#open "prelude";;
#directory "../MLGRAPH.DIR";;
#open "MLgraph";;
#open "complex";;
#open "pavages2";;
#infix "ctrans";;


let bullet = make_fill_picture (Nzfill,black)
             (make_sketch [Arc(origin,2.0,0.0,360.0) ]);;

let north pt dist pict =
   translate_picture (0.0,dist)
        (center_picture pict pt);;
let south pt dist pict =
   translate_picture (0.0,-.dist)
        (center_picture pict pt);;
let east pt dist pict =
   translate_picture (dist,0.0)
        (center_picture pict pt);;
let west pt dist pict =
   translate_picture (-.dist,0.0)
        (center_picture pict pt);;

let text s = make_text_picture 
            (make_font Helvetica 12.0) black s;;

(* +point_of_cx+ *)
let point_of_cx {re_part=r; im_part=i} = {xc=r; yc=i};;
let cx_of_point {xc=r; yc=i} = {re_part=r; im_part=i};;

(* +point_of_cx+ *)

(* +pointP+ *)
let ptP = cx_of_pol (1.0/.sqrt(1.0+.sqrt 2.0)) 22.5;;

(* +pointP+ *)


(* +ABCD+ *)
let ptA = cx_of_pol (sqrt(sinus 7.5 /. sinus 52.5)) 0.0;;
let ptB = cx_of_pol (sqrt(sinus 7.5 /. sinus 52.5)) 45.0;;
let ptC = cx_0;;

let rotA = hyp_rotation ptA 120.0;;
let rotB = hyp_rotation ptB 120.0;;
let rotC =  hyp_rotation ptC 90.0;;
let ptD = apply_hyp_iso rotC ptA;;

(* +ABCD+ *)

(* +points_du_poisson+ *)
let p0	= ptA
and p1	= mk_cx	(0.32)	(-0.018)
and p2	= mk_cx	(0.24)	(0.0)
and p3	= mk_cx	(0.2)	(-0.1)
and p4	= mk_cx	(0.09)	(-0.14);;
let p5	= ptC
and p6	= apply_hyp_iso rotC p4
and p7	= apply_hyp_iso rotC p3
and p8	= apply_hyp_iso rotC p2
and p9	= apply_hyp_iso rotC p1
and p10	= apply_hyp_iso rotC p0;;
let p11	= mk_cx	(0.08)	(0.355)
and p12	= mk_cx	(0.155)	(0.338)
and p13	= mk_cx	(0.18)	(0.25)
and p14	= mk_cx	(0.25)	(0.2);;
let p15	= ptB
and p16	= apply_hyp_iso rotB p14
and p17	= apply_hyp_iso rotB p13
and p18	= apply_hyp_iso rotB p12
and p19	= apply_hyp_iso rotB p11;;

(* +points_du_poisson+ *)


(* +interieur_du_poisson+ *)
let q1	= mk_cx	(0.26)	(0.09);;
let q2	= mk_cx	(0.16)	(0.19);;
let q3	= mk_cx	(0.08)	(0.28);;
let q4	= mk_cx	(0.22)	(0.02);;
let q5	= mk_cx	(0.16)	(0.07);;
let q6	= mk_cx	(0.305)	(0.17);;
let q7	= mk_cx	(0.27)	(0.19);;
let q8	= mk_cx	(0.09)	(0.22);;
let q9	= mk_cx	(0.08)	(0.25);;
let q10	= mk_cx	(0.11)	(0.285);;
let q11	= mk_cx	(0.16)	(0.26);;
let q12	= mk_cx	(0.34)	(0.01);;
let q13	= mk_cx	(0.31)	(0.01);;
let q14	= mk_cx	(0.32)	(0.03);;
let q15	= mk_cx	(0.365)	(0.05);;
let q16	= mk_cx	(0.35)	(0.08);;
let q17	= mk_cx	(0.338)	(0.068);;

let r1	= mk_cx	(0.15)	(0.065);;
let r2	= mk_cx	(0.09)	(0.005);;
let r3	= mk_cx	(0.055)	(-0.055);;
let r4	= mk_cx	(0.18)	(0.04);;
let r5	= mk_cx	(0.12)	(-0.025);;
let r6	= mk_cx	(0.1)	(-0.105);;
let r7	= mk_cx	(0.215)	(0.015);;
let r8	= mk_cx	(0.165)	(-0.05);;
let r9	= mk_cx	(0.16)	(-0.095);;
let r10	= mk_cx (0.3)	(0.175);;
let r11	= mk_cx	(0.35)	(0.25);;
let r12	= mk_cx	(0.275)	(0.195);;
let r13	= mk_cx	(0.315)	(0.275);;
let r14	= mk_cx	(0.035)	(0.29);;
let r15	= mk_cx	(0.09)	(0.34);;

(* +interieur_du_poisson+ *)


(* +corps+ *)
let body t = let tt z = point_of_cx (apply_hyp_iso t z)
in make_sketch [Seg (map tt [p0;p1;p2;p3;p4;p5;p6;p7;p8;p9;
                             p10;p11;p12;p13;p14;p15;p16;p17;
                             p18;p19;p0])];;

(* +corps+ *)


(* +quad+ *)
let quad t = let tt z = point_of_cx (apply_hyp_iso t z) in
make_sketch [Seg (map tt [ptA;ptB;ptD;ptC;ptA])];;

(* +quad+ *)


(* +dessin+ *)
let drawing t = let tt z = point_of_cx (apply_hyp_iso t z)
in
group_sketches [
make_sketch [Seg (map tt [p0;q1;q2;q3;p10])];
make_sketch [Seg (map tt [q4;q5])];
make_sketch [Seg (map tt [q6;q7])];
make_sketch [Seg (map tt [q8;q9;q10;q11])];
make_sketch [Seg (map tt [q12;q13;q14;q12])];
make_sketch [Seg (map tt [q15;q16;q17;q15])];
make_sketch [Seg (map tt [r1;r2;r3])];
make_sketch [Seg (map tt [r4;r5;r6])];
make_sketch [Seg (map tt [r7;r8;r9])];
make_sketch [Seg (map tt [r10;r11])];
make_sketch [Seg (map tt [r12;r13])];
make_sketch [Seg (map tt [q9;r14])];
make_sketch [Seg (map tt [q10;r15] )]
];;

(* +dessin+ *)
(* +poisson_modele+ *)

let model_fish =
  let t= comp_trans (scaling (200.0,200.0)) (translation (100.0,100.0)) in
  let A = transform_point t (point_of_cx ptA)
  and B = transform_point t (point_of_cx ptB)
  and C = transform_point t (point_of_cx ptC)
  and D = transform_point t (point_of_cx ptD) in
  group_pictures
    ([transform_picture t
        (make_default_draw_picture
           (group_sketches [body hyp_identity;
                            drawing hyp_identity]));
      east A 10.0 (text "A");
      north B 10.0 (text "B");
      west C 10.0 (text "C");
      north D 10.0 (text "D")]
     @ (map (center_picture bullet) [A;B;C]));;

eps_file model_fish "../../PS/poisson";;

(* +poisson_modele+ *)


(* +escher_gen+ *)
type escher_gen = TA | TB | TC | Ta | Tb | Tc ;;

(* +escher_gen+ *)
(* +escher_op+ *)

let escher_op = fun
  TA -> (fun [] -> [TA]
           | ((TA::_)|(TC::_)|(Ta::_)) -> raise Group_gen_exc
           | tl -> TA::tl)
| TB -> (fun [] -> [TB]
           | ((TB::_)|(TA::_)|(Tb::_)) -> raise Group_gen_exc
           | tl -> TB::tl)
| TC -> (fun [] -> [TC]
           | ((TB::_)|(Tc::_)|(TC::TC::_)) -> raise Group_gen_exc
           | tl -> TC::tl)
| Ta -> (fun [] -> [Ta]
           | ((Ta::_)|(Tb::_)|(TC::_)|(TB::_)|(TA::_))
             -> raise Group_gen_exc
           | tl -> Ta::tl)
| Tb -> (fun [] -> [Tb]
           | ((TA::_)|(Tc::_)|(Tb::_)|(TC::TC::_)|(TB::_))
             -> raise Group_gen_exc
           | tl -> Tb::tl)
| Tc -> (fun [] -> [Tc]
           | ((TA::_)|(Tc::_)|(Ta::_)|(TB::_)|(TC::_))
             -> raise Group_gen_exc
           | tl -> Tc::tl);;

(* +escher_op+ *)

(*
let escher_op =
  fun  TA  -> (fun [] -> [TA]
                        | ((Ta::_)|(Tc::_)|(Tb::_)
                           |(TA::_)|(TB::_))
                                  -> raise Group_gen_exc
                        |   tl -> TA::tl)
     |      TB  -> (fun [] -> [TB]
                        | ((Tb::_)|(TC::_)|(TB::_)
                           |(Ta::_)|(Tc::_))
                                   -> raise Group_gen_exc
                        |   tl -> TB::tl)
     |      TC  -> (fun [] -> [TC]
                        | ((Tc::_)|(TA::_)|(Ta::_)
                           |(TC::Tb::_)|(TC::TC::_))
                                        -> raise Group_gen_exc
                        |   tl -> TC::tl)
     |      Ta  -> (fun [] -> [Ta]
                        | ((TA::_)|(Tc::_)|(Ta::_))
                                        -> raise Group_gen_exc
                        |   tl -> Ta::tl)
     |      Tb  -> (fun [] -> [Tb]
                        | ((TB::_)|(Tb::_)|(Ta::_))
                                        -> raise Group_gen_exc
                        |   tl -> Tb::tl)
     |      Tc  -> (fun [] -> [Tc]
                        | ((TC::_)|(Tc::_)|(Tb::_))
                                        -> raise Group_gen_exc
                        |   tl -> Tc::tl);;
*)

(* +escher_group+ *)
let escher_group =
  make_tgroup [TA;TB;TC;Ta;Tb;Tc] escher_op;;

(* +escher_group+ *)
(* +escher_transfo+ *)

let escher_transfo =
  let transfo = fun
    TA -> hyp_rotation ptA 120.0
  | TB -> hyp_rotation ptB 120.0
  | TC -> hyp_rotation ptC 90.0
  | Ta -> hyp_rotation ptA (-.120.0)
  | Tb -> hyp_rotation ptB (-.120.0)
  | Tc -> hyp_rotation ptC (-.90.0)
  in (fun (ct,tl) ->
            (ct, it_list (fun t t' -> compose_hyp_iso t (transfo t'))
                   hyp_identity tl));;

(* +escher_transfo+ *)
(* +escher_tag_transfo+ *)

let escher_tag_transfo =
  let transfo = fun
    TA -> "A",hyp_rotation ptA 120.0
  | TB -> "B",hyp_rotation ptB 120.0
  | TC -> "C",hyp_rotation ptC 90.0
  | Ta -> "a",hyp_rotation ptA (-.120.0)
  | Tb -> "b",hyp_rotation ptB (-.120.0)
  | Tc -> "c",hyp_rotation ptC (-.90.0)
  in it_list (fun (s,t) c
                  -> let (s',t') = transfo c
                     in (s^s', compose_hyp_iso t t'))
       ("", hyp_identity);;

(* +escher_tag_transfo+ *)
(* +pseudo_escher_tiling+ *)

let text size s =
  make_text_picture
    (make_font Helvetica size) black s;;

let make_pseudo_escher_tiling polygon n =
  let center = {re_part=0.2; im_part=0.15} in
  group_pictures
    (map (fun (tag, T) ->
                let p = make_default_draw_picture (polygon T) in
                group_pictures
                  [p; center_picture (text 0.05 tag)
                        (point_of_cx (apply_hyp_iso T center)) ])
       (map escher_tag_transfo
          (generate_transformations_group escher_group n)));;

(* +pseudo_escher_tiling+ *)



(* +pseudo_escher_prep+ *)
let pseudo_escher1 = 
  make_pseudo_escher_tiling quad 2;;
let pseudo_escher2 = 
  make_pseudo_escher_tiling body 2;;

(* +pseudo_escher_prep+ *)


(* +pseudo_escher+ *)
eps_file (scale_picture (200.0,200.0) pseudo_escher1) "../../PS/pseudo_escher1";;
eps_file (scale_picture (200.0,200.0) pseudo_escher2) "../../PS/pseudo_escher2";;

(* +pseudo_escher+ *)

(* +color_permut_for_escher+ *)
let CTA = [|2; 0; 1; 3|];;
let CTB = [|2; 1; 3; 0|];;
let CTC = [|1; 0; 3; 2|];;
let CTa = [|1; 2; 0; 3|];;
let CTb = [|3; 1; 0; 2|];;

(* +color_permut_for_escher+ *)
(* +escher_group2+ *)

let escher_group =
  make_ctgroup [TA; TB; TC; Ta; Tb; Tc]
    [CTA; CTB; CTC; CTa; CTb; CTC]
    escher_op;;

(* +escher_group2+ *)
(* +les_couleurs_pour_escher+ *)

let color_of_int = fun
  0 -> Gra 0.3 | 1 -> Gra 0.5
| 2 -> Gra 0.7 | 3 -> Gra 0.8
| _ -> failwith "wrong color";;

(* +les_couleurs_pour_escher+ *)
(* +le_poisson_pour_escher+ *)

let fish t col =
  let outline_sk = body t
  and decoration_sk = drawing t
  in group_pictures
       [make_fill_picture
          (Nzfill, color_of_int col) outline_sk;
        make_default_draw_picture
          (group_sketches [outline_sk; decoration_sk])];;

(* +le_poisson_pour_escher+ *)



(* +simplify_escher+ *)
let center= cx_of_point (sketch_center  (body hyp_identity));;
let simplify_escher d= 
select (fun (_,x)-> module (apply_hyp_iso x center) <. d);;

(* +simplify_escher+ *)
(* +make_escher_tiling+ *)

let make_escher_tiling n d =
  group_pictures
    (map (fun (t1,t2) -> (fish t2 (t1.(0))))
       (simplify_escher d
          (map escher_transfo
             (generate_coltrans_group escher_group n))));;

(* +make_escher_tiling+ *)


(* +circle_limit+ *)
let escher1 = scale_picture (200.0,200.0) 
          (make_escher_tiling 1 1.0);;
eps_file escher1 "../../PS/escher1";;

let escher2 = scale_picture (200.0,200.0) 
          (make_escher_tiling 2 1.0);;
eps_file escher2 "../../PS/escher2";;
(*
let escher5 = scale_picture (200.0,200.0) 
          (make_escher_tiling 5 0.95);;
eps_file escher5 "../../PS/escher5";;

let escher6 = scale_picture (200.0,200.0) 
          (make_escher_tiling 6 0.95);;
eps_file escher6 "../../PS/escher6";;

let escher8 = scale_picture (200.0,200.0) 
          (make_escher_tiling 8 0.95);;
eps_file escher8 "../../PS/escher8";;
*)

let escher9 = scale_picture (200.0,200.0) 
          (make_escher_tiling 9 0.95);;
eps_file escher9 "../../PS/escher9";;

(* +circle_limit+ *)
(* +circle_limit_couleur+ *)

let color_of_in t=
  let c = 255.0 in
  fun 0 -> Rgb (255.0 /. c, 182.0 /. c, 48.0 /. c)
    | 1 -> Rgb (110.0 /. c, 156.0 /. c, 84.0 /. c)
    | 2 -> Rgb (182.0 /. c, 65.0 /. c, 33.0 /. c)
    | 3 -> Rgb (51.0 /. c, 102.0 /. c, 90.0 /. c)
    | _ -> failwith "wrong color";;

let fish t col =
  let  outline_sk=  body t
  and   decoration_sk = drawing t
  in group_pictures
       [make_fill_picture 
          (Nzfill, color_of_int col) outline_sk;
        make_default_draw_picture 
          (group_sketches [outline_sk; decoration_sk])];;

let make_escher_tiling n d =
  group_pictures
    (map (fun (t1,t2) ->
                (fish t2 (t1.(0))))
       (simplify_escher d
          (map escher_transfo
             (generate_coltrans_group escher_group n))));;


let escher9c =
  center_picture
    (scale_picture (250.0, 250.0)
       (group_pictures
          [make_fill_picture
             (Nzfill, let c=255.0 in Rgb (182.0 /. c, 65.0 /. c, 33.0 /. c))
             (make_sketch[Arc(origin, 0.99, 0.0, 360.0)]);
           make_escher_tiling 9 0.95]))
    {xc=150.0; yc=300.0};;

eps_file escher9c "../../PS/escher9c";;


(* +circle_limit_couleur+ *)



(* ++ *)
(* ++ *)
(* ++ *)
(* ++ *)
