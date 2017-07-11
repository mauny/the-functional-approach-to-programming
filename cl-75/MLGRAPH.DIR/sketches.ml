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

(* $Id: sketches.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* sketches.ml                                                           *)
(*               Emmanuel Chailloux & Guy Cousineau                      *)
(*               Mon Jan 20  1992                                        *)




#open "MLgraph";;

#open "compatibility";;
#open "geometry";;
#open "frames";;




let sketch_frame sk = sk.frame;;

let has_exact_frame_sketch sk = (sk.exact_frame=true);;

let sketch_hull sk = 
   if has_exact_frame_sketch sk 
          then sk.hull
          else failwith "sketch hull not available"
;;

let sketch_center sk = frame_center (sketch_frame sk);;

let sketch_height p =
  let fr= sketch_frame p
  in fr.ymax -. fr.ymin;;
  
let sketch_width p =
  let fr= sketch_frame p
  in fr.xmax -. fr.xmin;;


let compute_size =
  it_list (fun n ge -> n + (match ge 
                            with (Seg ptl) -> list_length ptl
                            | (Arc (_,_,_,_))  ->  3
                            | (Curve (_,_,_,_)) -> 4))
          0;;

let rec compute_path_size = function
  Spath gel  -> compute_size gel
| Tpath (_,p) -> compute_path_size p
| Cpath pl  ->  it_list add_int 0 (map compute_path_size pl);;


let make_sketch gel =
 if gel=[] then failwith "make_sketch: cannot make sketch from empty list of geom elements"
 else
  let hull = if get_exact_frame_mode()
                then compute_geom_elem_list_convex_hull gel else []  in
  let frame =  if get_exact_frame_mode()
                then frame_of_convex_hull hull else compute_frame gel in
  {path= Spath gel; frame= frame; exact_frame=true; hull=hull; size=compute_size gel}
;;

let frame_sketch ({xmin=a;xmax=b;ymin=c;ymax=d} as f) =
 let ptA={xc=a;yc=c} and ptB={xc=a;yc=d}
 and ptC={xc=b;yc=d} and ptD={xc=b;yc=c} in
    {path= Spath [Seg [ptA;ptB;ptC;ptD;ptA]];
     frame=f;
     exact_frame=true;
     hull= [ptA;ptB;ptC;ptD];
     size = 5};;

let hull_sketch hull =
 if hull=[] then failwith "hull_sketch: cannot make sketch from empty hull"
  else
    {path= Spath [Seg hull];
     frame= frame_of_convex_hull hull;
     exact_frame=true;
     hull= hull;
     size = list_length hull};;


let sketch_center (sk:sketch) = frame_center (sk.frame);;  

let rec compute_path_hull path = 
match path with
  Spath gel -> compute_geom_elem_list_convex_hull gel
| Tpath (transfo,path') ->
      let hull' = compute_path_hull path'
      in map (transform_point transfo) hull'
| Cpath path_list ->
      merge_convex_hulls (map  compute_path_hull path_list);;

let recompute_sketch_hull ({path=path;frame=_;exact_frame=b;hull=_;size=sz} as sk) =
 if b then sk else
 let h= compute_path_hull path in
 let fr= frame_of_convex_hull h in
     begin  sk.frame<-fr; sk.exact_frame<-true; sk.hull<-h; sk end;;

let group_sketches skl = 
  if not (get_exact_frame_mode())
    then {path= Cpath (map (fun sk -> sk.path) skl); 
          frame=merge_frames (map (fun sk -> sk.frame) skl); 
          exact_frame=false;
          hull=[]; 
          size= it_list add_int 0 (map (fun sk -> sk.size) skl)}
    else let skl' = map  recompute_sketch_hull skl in
         let h = merge_convex_hulls (map (fun sk -> sk.hull) skl') in
         let fr = frame_of_convex_hull h in
         {path= Cpath (map (fun sk -> sk.path) skl') ; 
          frame=fr ; 
          exact_frame=true;
          hull=h; 
          size= it_list add_int 0 (map (fun sk -> sk.size) skl)};;


let ungroup_sketch sk =
match sk.path with
  Cpath pl -> map (fun p -> let h =  compute_path_hull p
                          and sz = compute_path_size p in
                          let fr = frame_of_convex_hull h in
                              {path=p;frame=fr;exact_frame=true;hull=h;size=sz})
                pl
| _  -> [sk];;




let transform_sketch t =
  if not (get_exact_frame_mode())
  then   let tf = transform_frame t  in
         function  {path=Tpath(t',p) ; frame =f; size=s}
              ->  {path=Tpath(ctrans t t',p) ; frame = tf f; exact_frame = false; hull=[]; size=s}
         | {path= p ; frame =f; size=s}
              -> {path = Tpath(t,p) ; frame = tf f; exact_frame = false; hull=[];size=s}
  else fun {path=p; frame=f; exact_frame=b; hull=h; size=s}
        -> let h = if b then map (transform_point t) h
                        else map (transform_point t) (compute_path_hull p) in
           let fr = frame_of_convex_hull h in
           match p with
            Tpath(t',p)
               -> {path=Tpath(ctrans t t',p) ; frame = fr; exact_frame = true; hull=h; size=s}
          | _ -> {path = Tpath(t,p) ; frame = fr; exact_frame = true; hull=h ;size=s}
;;
let center_sketch sk {xc=x;yc=y}=
  let {xc=x';yc=y'} = sketch_center sk
  in transform_sketch (translation (x-.x',y-.y'))sk;;



let fit_sketch_in_frame (sk:sketch) f =
   let t = frame_to_frame_transform (sk.frame) f
   in  transform_sketch t sk;;

let force_sketch_in_frame f sk =   
  if not (get_exact_frame_mode())
   then {path=sk.path;frame=f;exact_frame=false; hull=[]; size=sk.size}
   else failwith "force_sketch_in_frame: not authorized in exact frame mode";;


let scale_and_center_sketch (hscale,vscale) (sk:sketch) =
  let sct=  scaling (hscale,vscale) in
   center_sketch (transform_sketch sct sk) (sketch_center sk);;

let scale_sketch = scale_and_center_sketch;;


let translate_sketch  (htrans,vtrans) = 
     transform_sketch  (translation (htrans,vtrans));;


let vflip_sketch sk =
   let s = vsymmetry (sketch_center sk).xc
   in  transform_sketch s sk;;

let hflip_sketch sk =
   let s = hsymmetry (sketch_center sk).yc
   in  transform_sketch s sk;;

let rotate_sketch a sk =
     transform_sketch (rotation (sketch_center sk) a ) sk;;  
    

let besides_sketch (sk1:sketch) (sk2:sketch) =    
    let {xmin=a; xmax=b; ymin=c; ymax=d} = sk1.frame
    and  width2 = sk2.frame.xmax -. sk2.frame.xmin 
in  group_sketches [sk1 ;fit_sketch_in_frame  sk2 {xmin=b;xmax=b+.width2;ymin=c;ymax=d}]  ;;

let over_sketch (sk1:sketch) (sk2:sketch) =    
    let {xmin=a; xmax=b; ymin=c; ymax=d} = sk1.frame
    and  height2 = sk2.frame.ymax -. sk2.frame.ymin 
in group_sketches [sk1; fit_sketch_in_frame  sk2 {xmin=a;xmax=b;ymin=c-.height2;ymax=c}]  ;;



let extend_sketch_frame str k {path=p;frame=fr;exact_frame=b; hull=h; size=n} =
  if not (get_exact_frame_mode())
   then {path=p;frame=extend_frame str k fr;exact_frame=false; hull=[]; size=n}
   else failwith "extend_sketch_frame: not authorized in exact frame mode";;






