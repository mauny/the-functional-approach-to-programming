(*                                                                       *)
(*                    Projet      Formel                                *)
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

(* $Id: frames.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* frames.ml         basic geometric objects frames                      *)
(*                   Emmanuel Chailloux & Guy Cousineau                  *)
(*                   Mon Jan 20  1992                                    *)





#open "MLgraph";;

#open  "compatibility";;
#open  "prelude";;
#open "geometry";;




let frame_center f = {xc=(f.xmin+.f.xmax)/.2.0 ; yc=(f.ymin+.f.ymax)/.2.0};;

let extend_frame ext k {xmin=a;xmax=b;ymin=c;ymax=d} =
  let hmargin = k*.(b-.a)
  and vmargin = k*.(d-.c)
  in 
    match ext 
    with  All_ext   ->   {xmin=a-.hmargin;xmax=b+.hmargin;
                        ymin=c-.vmargin;ymax=d+.vmargin} 
     |  Horiz_ext ->   {xmin=a-.hmargin;xmax=b+.hmargin; ymin=c;ymax=d} 
     |  Vertic_ext->   {xmin=a;xmax=b; ymin=c-.vmargin;ymax=d+.vmargin} 
     |  Left_ext  ->   {xmin=a-.hmargin;xmax=b; ymin=c; ymax=d} 
     |  Right_ext ->   {xmin=a; xmax=b+.hmargin; ymin=c;ymax=d} 
     |  Top_ext   ->   {xmin=a;xmax=b; ymin=c;ymax=d+.vmargin} 
     |  Bottom_ext->   {xmin=a;xmax=b; ymin=c-.vmargin;ymax=d} 
;;
  

(* Computations of Convex Hulls *)

(* Exact frames mode ? *)
let exact_frame_mode = ref false;;
let set_exact_frame_mode b = (exact_frame_mode:= b);;
let get_exact_frame_mode () = (!exact_frame_mode);;


(* number of points used for a full circle  *)
(* a linear proportion will be used for circle arcs *)

let discrete_circle_number = ref 30;;
let set_discrete_circle_number n = (discrete_circle_number:= n);;
let get_discrete_circle_number () = (!discrete_circle_number);;


let discrete_curve_number = ref 20;;
let set_discrete_curve_number n = (discrete_curve_number:= n);;
let get_discrete_curve_number () = (!discrete_curve_number);;


let point_convex_hull ({xc=x;yc=y} as pt) = [pt];;

let rec interval_dec m n =
  if m < n then [] else m::interval_dec (m-1) n;;

let angle_intervals a1 a2 =
  let n = abs (int_of_float (float_of_int !discrete_circle_number *. ((a2-.a1)/.360.))) in
  let dx = (a2-.a1) /. (float_of_int n) in
  let rec interv_rec =
         function  ([],r)  -> r
              |  (a::l,r) -> interv_rec (l, a1+.(float_of_int a)*.dx::r) in
  let nl = interval_dec (n-1) 1 in
    if lt_float a1 a2 then a1::interv_rec (nl,[a2])
               else a2::interv_rec (nl,[a1]);;


let arc_convex_hull (c,r,a1,a2) =
  let point_at_angle a = {xc=c.xc+.r*.cosinus a; yc=c.yc+.r*.sinus a} in
  let angle_list = angle_intervals a1 a2 in
    map point_at_angle angle_list;;


let rec generate f x n =
if n<0 then failwith "generate: negative argument" else
match n with
   0   ->   [x]
|  n   ->   x :: generate f (f x) (n-1);;

let curve_convex_hull (p0,p1,p2,p3) =
  let a0 = p0.xc and b0 = p0.yc
  and a1= 3. *. (p1.xc-.p0.xc) and b1 = 3. *. (p1.yc-.p0.yc)
  and a2 = 3. *. (p0.xc -. 2. *. p1.xc +. p2.xc)
  and b2 = 3. *. (p0.yc -. 2. *. p1.yc +. p2.yc)
  and a3 = p3.xc -. 3. *. p2.xc +. 3. *. p1.xc -. p0.xc
  and b3 = p3.yc -. 3. *. p2.yc +. 3. *. p1.yc -. p0.yc  in
  let compute_x t = ((a3*.t+.a2)*.t+.a1)*.t+.a0
  and compute_y t = ((b3*.t+.b2)*.t+.b1)*.t+.b0 in
  let args = generate (fun x -> x+.(1. /. (float_of_int !discrete_curve_number)))
             0. !discrete_curve_number in
   map  (fun t -> {xc=compute_x t; yc= compute_y t}) args;;



let coord_order {xc=x1; yc=y1} {xc=x2; yc=y2} =
  lt_float y1 y2 or (y1=y2 & lt_float x1 x2);;

let polar_order {xc=x0; yc=y0} {xc=x1; yc=y1} {xc=x2; yc=y2} =
 let x1' = x1-.x0
 and y1' = y1-.y0
 and x2' = x2-.x0
 and y2' = y2-.y0 in
 let det = x1'*.y2'-.x2'*.y1' in
   gt_float det 0.0 or (det=0.0 & 
                        gt_float (x1'*.x1'+.y1'*.y1')
                                 (x2'*.x2'+.y2'*.y2'));;

let min_list order = function
  []  ->  failwith "min_list: empty list"
| (a::l) -> 
     let rec min_rec a = function
       []    -> a
     | (b::l) -> if order a b then min_rec a l
                    else min_rec b l
     in min_rec a l;;


let convex_hull ptl =
  let pt1 = min_list coord_order ptl in  
   let rec convex_rec pt0 cvh pts =
     let pt = min_list (polar_order (hd cvh)) pts in
     if pt=pt0 then rev cvh else convex_rec pt0 (pt::cvh) pts
  in convex_rec pt1 [pt1] ptl ;;

let merge_convex_hulls cvhl =
  convex_hull (list_it append cvhl []);;


let compute_geom_elem_convex_hull =
 function  (Seg pl) -> convex_hull pl
        |  (Arc (a,b,c,d)) ->  arc_convex_hull (a,b,c,d)
        |  (Curve (a,b,c,d)) -> curve_convex_hull (a,b,c,d);;


let compute_geom_elem_list_convex_hull = function
  [] -> failwith "compute_convex_hull"
| gel ->  merge_convex_hulls (map compute_geom_elem_convex_hull gel);;

let convex_hull_of_frame {xmin=xmin ; xmax =xmax ; ymin =ymin; ymax =ymax} =
  let ptA={xc=xmin;yc=ymin} and ptB={xc=xmax;yc=ymin}
  and ptC={xc=xmax;yc=ymax} and ptD= {xc=xmin;yc=ymax} in
    [ptA;ptB;ptC;ptD];;


(* Computation of frames *)


let point_frame {xc=x;yc=y} = {xmin=x ; xmax =x ; ymin =y; ymax =y};;

let seg_frame = function
   [] -> failwith"seg_frame"
 | (pt::ptl) ->
  let frame = point_frame pt in
  do_list   (fun {xc=x;yc=y} -> frame.xmin<- min_float (frame.xmin) x;
                                frame.xmax<- max_float (frame.xmax) x;
                                frame.ymin<- min_float (frame.ymin) y;
                                frame.ymax<- max_float (frame.ymax) y)
            ptl;
  frame;;

let frame_of_convex_hull = seg_frame;;

let ordered_angles(a1,x,a2) =
         le_float a1 x & le_float x a2
     or  le_float a1 (x +. 360.0) & le_float (x +. 360.0) a2;;

let arc_frame (c,r,a1,a2) =  
  {xmin = (if ordered_angles(a1,180.0,a2) 
             then c.xc -. r
             else  let m = min_float (cosinus a1) (cosinus a2)
                   in c.xc +. m*.r);
   xmax = (if ordered_angles(a1,0.0,a2) 
             then c.xc +. r
             else  let m = max_float (cosinus a1) (cosinus a2)
                   in c.xc +. m*.r);
   ymin = (if ordered_angles(a1,270.0,a2) 
             then c.yc -. r
             else  let m = min_float (sinus a1) (sinus a2)
                   in c.yc +. m*.r);
   ymax = (if ordered_angles(a1,90.0,a2) 
             then c.yc +. r
             else  let m = max_float (sinus a1) (sinus a2)
                   in c.yc +. m*.r)};;

let curve_frame (pt1,pt2,pt3,pt4) =
  let x = pt1.xc
  and xl = [pt2.xc;pt3.xc;pt4.xc]
  and y = pt1.yc
  and yl = [pt2.yc;pt3.yc;pt4.yc]
  in
    {xmin = it_list min_float x xl;
     xmax = it_list max_float x xl;
     ymin = it_list min_float y yl;
     ymax = it_list max_float y yl};;


let merge_frames  = function
 []    ->  failwith "merge_frames: emty list of frames"
| (f::fl) ->
    it_list
      (fun {xmin=xmin1;xmax=xmax1;ymin=ymin1;ymax=ymax1}
           {xmin=xmin2;xmax=xmax2;ymin=ymin2;ymax=ymax2}
       ->
           {xmin= min_float xmin1 xmin2;
            xmax= max_float xmax1 xmax2;
            ymin= min_float ymin1 ymin2;
            ymax= max_float ymax1 ymax2})
      f fl;;


let compose_frames = merge_frames;;

let compute_geom_elem_frame =
 function  (Seg pl) -> seg_frame pl
        |  (Arc (a,b,c,d)) ->  arc_frame (a,b,c,d)
        |  (Curve (a,b,c,d)) -> curve_frame (a,b,c,d);;

let compute_frame = function
  [] -> failwith "compute_frame"
| gel -> merge_frames (map compute_geom_elem_frame gel);;


let transform_frame t  {xmin=xmin;xmax=xmax;ymin=ymin;ymax=ymax} =
let ptl = map (transform_point t) 
               [make_point(xmin,ymin);make_point(xmin,ymax);
                make_point(xmax,ymin);make_point(xmax,ymax)]
 in seg_frame ptl;;

let frame_to_frame_transform {xmin=a; xmax=b; ymin=c; ymax=d}
                             {xmin=a'; xmax=b'; ymin=c'; ymax=d'} =

  let hscale = (b'-.a')/.(b-.a)
  and vscale = (d'-.c')/.(d-.c)
  in
    let t1 = translation(-.a,-.c)
    and  s = scaling (hscale,vscale)
    and	t2 = translation (a',c')

    in  compose_transformations[t2; s; t1];;

