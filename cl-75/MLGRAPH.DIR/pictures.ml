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


(* $Id: pictures.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* pictures.ml                                                           *)
(*              Emmanuel Chailloux & Guy Cousineau                       *)
(*              Tue Jan 21 1992                                          *)




#open "MLgraph";;

#open "compatibility";;
#open "prelude";;
#open "sketches";;
#open "bitmaps";;
#open "fonts";;
#open "texts";;
#open "frames";;
#open "paint";;
#open "geometry";;
#open "graphics_defaults";;




(* To compute hulls *)

let rec compute_pict_hull = function
  Blank(fr,c)  -> convex_hull_of_frame fr
| Draw(p,_,_,_,_) -> compute_path_hull p
| Fill(p,_,_,_)  -> compute_path_hull p
| Clip(_,path,pict,_) ->  compute_path_hull path
| Bitmap b  -> bitmap_hull b
| Bitmapmask(b,_,_) -> bitmap_hull b
| Text({t_string=s; t_font=fnt},_)  -> convex_hull_of_frame(text_frame  fnt s)
| Tpict(transfo,p) -> let h= compute_pict_hull p
                      in map (transform_point transfo) h
| Cpict pict_list  -> 
     merge_convex_hulls (map  compute_pict_hull pict_list)
;;

let  recompute_picture_hull  ({pict=pict; frameP=_; exact_frameP=b; hullP=_;
                            input_interface=ip; output_interface=op} as p)
=  if b then p else
   let h= compute_pict_hull pict in
   let fr = frame_of_convex_hull h in
   begin  p.frameP<-fr;p.exact_frameP<-true; p.hullP<-h;p end;;

        




(* Operations to access picture components                *)

let picture_frame (pict:picture) = pict.frameP;;

let has_exact_frame_picture  (pict:picture) = (pict.exact_frameP=true);;
let has_exact_frame = has_exact_frame_picture;;

let picture_hull  (pict:picture) =
   if has_exact_frame pict 
          then pict.hullP
          else failwith "picture hull not available"
;;

let picture_center p = frame_center (picture_frame p);;

let picture_input_interface p = p.input_interface;;

let picture_output_interface p = p.output_interface;;


(* To obtain informations about pictures                   *)

let picture_height p =
  let fr= picture_frame p
  in fr.ymax -. fr.ymin;;
  
let picture_width p =
  let fr= picture_frame p
  in fr.xmax -. fr.xmin;;


let color_pict pict = 
match pict with 
  Draw(_,_,c,_,_) -> c
| Fill (_,_,c,_)  -> c
| Text (_,c) -> c
| Bitmapmask (_,c,_) -> c
| Blank (_,c) -> c
| _ -> failwith "color get operation apply only to draw, fill or text picture";;
 

(* Operations that modify characteristics of a picture     *)
(* in a non destructive way                                *)

let change_color_picture c {pict=pict; frameP=f; exact_frameP=b; hullP=h;
                            input_interface=ip; output_interface=op}
= match pict with
    (Draw(p,lsty,_,b,n)) -> {pict=Draw(p,lsty,c,b,n); frameP=f; exact_frameP=b; hullP=h;
                            input_interface=ip; output_interface=op}
  | (Fill(p,fsty,_,n)) -> {pict=Fill(p,fsty,c,n); frameP=f; exact_frameP=b; hullP=h;
                            input_interface=ip; output_interface=op}
  |  (Text(t,_))        -> {pict=Text(t,c); frameP=f; exact_frameP=b; hullP=h;
                            input_interface=ip; output_interface=op}
  |  (Bitmapmask (bt,_,bo)) -> {pict=Bitmapmask(bt,c,bo);frameP=f; exact_frameP=b; hullP=h;
                               input_interface=ip; output_interface=op}
  |  (Blank (ff,oc))     ->  {pict= Blank(ff,c);frameP=f; exact_frameP=b; hullP=h;
                               input_interface=ip; output_interface=op}
  |  _  -> 
   failwith "color change operation apply only to draw, fill or text picture";;



let change_linestyle_picture lsty {pict=pict; frameP=f; exact_frameP=bb; hullP=h;
                          input_interface=ip; output_interface=op}
= match pict with
    (Draw(p,_,c,b,n))
      -> {pict=Draw(p,lsty,c,b,n);
           frameP=f; exact_frameP=bb; hullP=h;
           input_interface=ip; 
           output_interface=op}
    |  _  -> 
             failwith "linestyle change operation apply only to draw picture";;

let change_linewidth_picture lw {pict=pict; frameP=f; exact_frameP=bb; hullP=h;
                            input_interface=ip; output_interface=op}
= match pict with
    (Draw(p,{linewidth=_;linecap=lc;linejoin=lj;dashpattern=dp},c,b,n))
     -> {pict=Draw(p,{linewidth=lw;linecap=lc;linejoin=lj;dashpattern=dp},c,b,n);
         frameP=f; exact_frameP=bb; hullP=h;
         input_interface=ip; 
         output_interface=op}
    |  _  -> 
       failwith "linewidth change operation apply only to draw picture";;


let set_picture_interfaces {pict=p ; frameP=f; exact_frameP=bb; hullP=h} (p1,p2) =
    {pict=p ; frameP=f; exact_frameP=bb; hullP=h; input_interface=p1; output_interface=p2} ;;



(* Operations on interfaces            *)


let rec transform_interface t = function
       No_handle ->  No_handle
    |  One_handle (pt1,pt2) -> One_handle(transform_point t pt1,
                                      transform_point t pt2)
    |  Handles l -> Handles (map (function n,(pt1,pt2) ->
                              (n,(transform_point t pt1,
			          transform_point t pt2)))
                           l);;


let rec find_handle n  = 
  function  No_handle  -> failwith ("Could not find port "^n)
         |  One_handle (_,_) -> failwith ("Could not find port "^n)
         |  Handles l  -> try assoc n l
                  with _ -> failwith ("Could not find port "^n)
;;

let rec rem_assoc n =
     function []  ->  failwith ("Could not remove element "^n)
         |  p::l' ->  let n',_ = p in if n=n' 
	                           then l'
                                   else p::rem_assoc n l';;

let rem_port n = function (Handles l) -> Handles (rem_assoc n l)
                     |     h -> failwith "pictures__remport";;

let join_interfaces =
  function (Handles l1,Handles l2) -> Handles (l1@l2)
   |  (ports,No_handle)  -> ports
   |  (No_handle,ports)  -> ports
   |  (One_handle (p1,p2), _)   ->  One_handle (p1,p2)
   |  (Handles l, _)            -> Handles l;;
   
let group_interfaces il = 
   list_it
     (fun i1 i2 -> join_interfaces (i1,i2))
     il
     No_handle;;

     

(* To make blank pictures  *)

let make_blank_picture (h,w)
  = let fr = {xmin=0.0; xmax=w; ymin=0.0; ymax=h}
    in if get_exact_frame_mode()
         then failwith "It is not possible to make blank pictures en exact frame mode"
         else
      {pict= Blank (fr, (Gra 1.0)) ; 
       frameP = fr;
       exact_frameP=true;
       hullP= [origin;{xc=w;yc=0.};{xc=w;yc=h};{xc=0.;yc=h}];
       input_interface=No_handle;
       output_interface=No_handle};;
let blank_rectangle = make_blank_picture;;
let blank_square x = blank_rectangle (x,x);;

  

(* To make pictures from sketches  *)

let make_draw_picture ((lsty:linestyle),col) 
                {path=p; frame = {xmin=a;xmax=b;ymin=c;ymax=d}; 
                 exact_frame=bb; hull=h;size=sz}  
=
  let dx = lsty.linewidth/.2.0 in
      {pict= Draw(p,lsty,col,false,sz) ; 
       frameP = {xmin=a-.dx;xmax=b+.dx;ymin=c-.dx;ymax=d+.dx};
       exact_frameP=bb; hullP=h;
       input_interface=No_handle;
       output_interface=No_handle};;

let make_closed_draw_picture ((lsty:linestyle),col) 
                {path=p; frame = {xmin=a;xmax=b;ymin=c;ymax=d}; 
                 exact_frame=bb; hull=h;size=sz}  
=
  let dx = lsty.linewidth/.2.0 in
      {pict= Draw(p,lsty,col,true,sz) ; 
       frameP = {xmin=a-.dx;xmax=b+.dx;ymin=c-.dx;ymax=d+.dx};
       exact_frameP=bb; hullP=h;       
       input_interface=No_handle;
       output_interface=No_handle};;

let make_default_draw_picture  s =
    make_draw_picture 
       (default_linestyle (sketch_frame s),default_color())
       s;;

let make_default_closed_draw_picture  s =
    make_closed_draw_picture 
       (default_linestyle (sketch_frame s),default_color())
       s;;

let make_fill_picture (fsty,col) {path=p ; frame = f; exact_frame=bb; hull=h;size=sz} =
      {pict= Fill(p,fsty,col,sz) ; frameP = f;
       exact_frameP=bb; hullP=h;       
       input_interface=No_handle;
       output_interface=No_handle};;

let make_default_fill_picture  =
   make_fill_picture (default_fillstyle(),default_color());;

let clip_picture clipsty sk p =
 if  get_exact_frame_mode() 
 then
   let h= if p.exact_frameP then p.hullP else compute_pict_hull p.pict in
   let fr =  if sk.exact_frame then sk.frame
                 else frame_of_convex_hull h in 
     {pict=Clip(clipsty,sk.path,p.pict,sk.size); 
      frameP = fr;
      exact_frameP = true;
      hullP = h;
      input_interface=p.input_interface; 
      output_interface=p.output_interface}
 else
     {pict=Clip(clipsty,sk.path,p.pict,sk.size); 
      frameP = sk.frame;
      exact_frameP = false;
      hullP = [];
      input_interface=p.input_interface; 
      output_interface=p.output_interface}
;;


(* To make pictures from bitmaps  *)

let make_bitmap_picture b =
 {pict= Bitmap b; 
  frameP = bitmap_frame b;
  exact_frameP=true;
  hullP= convex_hull_of_frame (bitmap_frame b);
  input_interface=No_handle;
  output_interface=No_handle}  ;;

let make_bitmap_mask_picture b col bo =
 {pict= Bitmapmask (b,col,bo); 
  frameP = bitmap_frame b;
  exact_frameP=true;
  hullP= convex_hull_of_frame (bitmap_frame b);
  input_interface=No_handle;
  output_interface=No_handle}  ;;

let make_default_bitmap_mask_picture  b =
    make_bitmap_mask_picture b (default_color()) false
;;

(* To make pictures from texts  *)

let make_text_picture font color str= 
  let t = make_text  str font
  in
    {pict=Text (t,color); 
     frameP= text_frame font str;
     exact_frameP=true;
     hullP= convex_hull_of_frame(text_frame font str);
     input_interface=No_handle;
     output_interface=No_handle}  ;;

let make_default_text_picture str = 
  let t = make_default_text str 
  in
    {pict=Text (t,default_color()); 
     frameP= text_frame (default_font ()) str;
     exact_frameP=true;
     hullP= convex_hull_of_frame(text_frame  (default_font ()) str);
     input_interface=No_handle;
     output_interface=No_handle}  ;;
     


(* To make pictures from frames  *)


let make_frame_picture (lsty,col) =
  compose (make_closed_draw_picture (lsty,col)) frame_sketch;;

let make_default_frame_picture  =
  compose make_default_closed_draw_picture frame_sketch;;

(* To make pictures from hulls *)

let make_hull_picture (lsty,col) =
  compose (make_draw_picture (lsty,col)) hull_sketch;;

let make_default_hull_picture =
  compose (make_default_draw_picture) hull_sketch;;


(* To obtain frames or hulls of given pictures as pictures *)

let get_picture_frame_as_picture_with_lsty_and_color p (lsty,col) =
   make_frame_picture (lsty,col) (picture_frame p);;

let get_picture_frame_as_picture p =
 let lsty = {linewidth= 1.00; linecap= Roundcap; linejoin= Miterjoin; dashpattern=[]} in
   make_frame_picture (lsty,black) (picture_frame p);;

let get_picture_hull_as_picture_with_lsty_and_color p (lsty,col) =
   recompute_picture_hull p;
   let h=picture_hull p in
   make_hull_picture (lsty,col)  (h@[hd h]);;

let get_picture_hull_as_picture p =
 let lsty = {linewidth= 1.00; linecap= Roundcap; linejoin= Miterjoin; dashpattern=[]} in
   recompute_picture_hull p;
   let h=picture_hull p in
   make_hull_picture (lsty ,black) (h@[hd h]);;






(* Operations on pictures   *)

let group_pictures pictl
=
 if  get_exact_frame_mode() 
 then let pictl' = map recompute_picture_hull pictl in
      let  h = merge_convex_hulls (map (fun p -> p.hullP) pictl') in
      let fr = frame_of_convex_hull h in
         {pict = Cpict (map (fun p -> p.pict) pictl') ; 
          frameP= fr;
          exact_frameP=true; 
          hullP=h;
          input_interface = group_interfaces (map (fun p -> p. input_interface) pictl);
          output_interface = group_interfaces (map (fun p -> p. output_interface) pictl)}
 else
      {pict = Cpict (map (fun p -> p.pict) pictl); 
       frameP= merge_frames  (map (fun p -> p.frameP) pictl);
       exact_frameP=false;
        hullP=[];
       input_interface = group_interfaces  (map (fun p -> p. input_interface) pictl);
       output_interface = group_interfaces (map (fun p -> p. output_interface) pictl)}
;;

let ungroup_picture  p =
match p.pict with
  Cpict pl -> map (fun p -> let h= compute_pict_hull p in
                            let fr = frame_of_convex_hull h in
                               {pict=p; frameP=fr; exact_frameP=true; hullP=h;
                                input_interface=No_handle;
                                output_interface=No_handle})
              pl
| _  -> [p];;



let transform_picture t ({pict= p ; frameP =f; exact_frameP=b; hullP=h;
                         input_interface=inp; output_interface=outp} as pict) =
  if not (get_exact_frame_mode())
  then
  let f' = transform_frame t f
  in
   match p with (Tpict (t',p')) -> {pict=(Tpict (ctrans t t',p')) ; 
                                 frameP= f';exact_frameP = false; hullP=[];
                                 input_interface=inp; output_interface=outp}

           | p -> {pict = Tpict(t,p); frameP= f';exact_frameP = false; hullP=[];
                   input_interface=inp; output_interface=outp}
  else
   let h= if b then  map (transform_point t) h
               else  map (transform_point t) (compute_pict_hull p) in
   let fr = frame_of_convex_hull h in
   begin
   pict.frameP<-fr; pict.exact_frameP<-true; pict.hullP<-h;
   match p with (Tpict (t',p')) -> {pict=(Tpict (ctrans t t',p')) ; 
                                 frameP= fr;exact_frameP = true; hullP=h;
                                 input_interface=inp; output_interface=outp}

           | p -> {pict = Tpict(t,p); frameP= fr;exact_frameP = true; hullP=h;
                   input_interface=inp; output_interface=outp}
  end
;;

let center_picture pict {xc=x;yc=y}=
  let {xc=x';yc=y'} = picture_center pict
  in transform_picture (translation (x-.x',y-.y')) pict;;

		   
let translate_picture  (htrans,vtrans) = 
     transform_picture  (translation (htrans,vtrans));;
let translate = translate_picture;;
     
let rotate_picture  center angle = 
     transform_picture  (rotation center angle);;
let rotate = rotate_picture;;

(*
let scale_picture (hscale,vscale) (pict:picture) =
  let a= pict.frameP.xmin and c=pict.frameP.ymin
  in let t1 = translation (-.a,-.c)
     and s = scaling (hscale,vscale)
     and t2 = translation (a,c)
     in
       transform_picture (compose_transformations [t2;s;t1]) pict;;
*)

let scale_and_center_picture (hscale,vscale) (pict:picture) =
  let sct=  scaling (hscale,vscale) in
   center_picture (transform_picture sct pict) (picture_center pict);;
let scale_picture  = scale_and_center_picture;;
let scale = scale_picture;;

let scale_and_center_picture (hscale,vscale) (pict:picture) =
  let sct=  scaling (hscale,vscale) in
   center_picture (transform_picture sct pict) (picture_center pict);;


let fit_picture_in_frame (pict:picture) f =   
   let t = frame_to_frame_transform (pict.frameP) f
   in  transform_picture t pict;;
let fit_in_frame = fit_picture_in_frame;;

let force_picture_in_frame f p =   
  if not (get_exact_frame_mode())
   then   {pict=p.pict;frameP=f;exact_frameP=false;hullP=[];
           input_interface=p.input_interface;
           output_interface=p.output_interface}
   else failwith "force_picture_in_frame: not authorized in exact frame mode";;

let force_in_frame =  force_picture_in_frame;;

let add_frame_to_picture  p =
  group_pictures [p;make_default_frame_picture  (picture_frame p)];;
let add_frame = add_frame_to_picture;;

let vflip_picture pict =
   let s = vsymmetry (picture_center pict).xc
   in  transform_picture s pict;;
let vflip = vflip_picture;;

let hflip_picture pict =
   let s = hsymmetry (picture_center pict).yc
   in  transform_picture s pict;;
let hflip = hflip_picture;;

let rotate_picture a pict =
     transform_picture (rotation (picture_center pict) a ) pict;;  
let rotate =  rotate_picture;;



let  subpicture_fn fn  pred p  picture =
let rec 
  trans tp tl q = 
    if pred p.pict q then [fn tp tl q]
    else (match q with
      (Cpict pl) -> list_it append (map (trans tp tl) pl) []
    | (Tpict(t,q1)) -> trans tp (t::tl) q1
    | (Clip(_,p1,q1,_)) ->  trans ((p1,tl)::tp) tl q1
    | _ -> [])
in  trans [] [] picture.pict;;


let eq_path p1 p2 = 
   match (p1,p2) with 
     (Draw (pa1,_,_,_,_), Draw (pa2,_,_,_,_)) ->  pa1 == pa2
   | (Fill (pa1,_,_,_), Fill (pa2,_,_,_)) ->  pa1 == pa2
   | _,_ -> false
;;

let eq_frame p1 p2 = 
   match (p1,p2) with 
     (Blank (f1,_), Blank (f2,_)) -> f1 == f2
   |  _,_ -> false
;;


let eq_picture p1 p2 = p1 == p2;;

let subpicture_transformations  = 
  subpicture_fn 
    ((fun f g a x y -> f (g x)) compose_transformations rev) 
    eq_path
;;

let subpicture_colors  = 
  subpicture_fn 
    (fun x a b -> color_pict b)
    eq_frame
;;

let subpicture_clips = 
  subpicture_fn 
    (fun x a b ->
      match x with
        []   -> Spath []
       | x   ->  let (p,tl) = (hd x) in 
                   let t = compose_transformations (rev tl) in
                   p)
    eq_path
;;

let subpictures picture = 
let change_pict picture p = 
  {pict=p; 
   frameP=picture.frameP;
   exact_frameP=picture.exact_frameP;
   hullP=picture.hullP;
   input_interface=picture.input_interface;
   output_interface=picture.output_interface}
in 
let rec sub_rec p = 
match p with 
  Cpict pl  ->  map (change_pict picture) pl
   | p -> [change_pict picture p]
in 
  sub_rec picture.pict
;;


   
(*  Horizontal and Vertical Alignments    *)
(*
type alignment = Align_Right | Align_Left | Align_Center
               | Align_Top | Align_Bottom;;
*)


let besides_picture' align (pict1:picture)  (pict2:picture) =
 let fr1 = pict1.frameP
 and fr2 = pict2.frameP
 in
  let ((x1,y1),(x2,y2)) =
  match align
  with Align_Center 
      ->  ((fr1.xmax, (fr1.ymax +. fr1.ymin)/.2.0),
           (fr2.xmin, (fr2.ymax +. fr2.ymin)/.2.0))
    |  Align_Top    
       -> ((fr1.xmax,fr1.ymax),(fr2.xmin,fr2.ymax))
    |  Align_Bottom
       -> ((fr1.xmax,fr1.ymin),(fr2.xmin,fr2.ymin))
    |    _  -> failwith "Wrong alignment"
  
   in
	 group_pictures[pict1 ;transform_picture (translation (x1-.x2,y1-.y2)) pict2]

;;
	 
	 

let align_horizontally_picture_list align_mode = function 
  [] -> failwith "pictures__align_horizontally_picture : empty picture list"
| (p::pl) ->   it_list (besides_picture' align_mode) p pl;;

let align_horizontally =   align_horizontally_picture_list;;


let over_picture' align (pict1:picture)  (pict2:picture) =
 let fr1 = pict1.frameP
 and fr2 = pict2.frameP
 in
  let ((x1,y1),(x2,y2)) =
  match align
  with Align_Center 
      ->  (((fr1.xmax +. fr1.xmin)/.2.0,fr1.ymin),
           ((fr2.xmax +. fr2.xmin)/.2.0,fr2.ymax)) 
    |  Align_Left    
       -> ((fr1.xmin,fr1.ymin),(fr2.xmin,fr2.ymax))
    |  Align_Right
       -> ((fr1.xmax,fr1.ymin),(fr2.xmax,fr2.ymax))
    |    _  -> failwith "Wrong alignment"
  
   in
	 group_pictures[pict1 ; transform_picture (translation (x1-.x2,y1-.y2)) pict2]
;;



          
let align_vertically_picture_list align_mode = function 
  [] -> failwith "pictures__align_vertically_picture : empty picture list"
| (p::pl) ->   it_list (over_picture' align_mode) p pl;;

let align_vertically = align_vertically_picture_list;;


(*  Horizontal and Vertical Compositions    *)
    
let besides_picture (pict1:picture) (pict2:picture) =    
    let h1 = picture_height pict1  
    and h2 = picture_height pict2 
in align_horizontally
      Align_Center
      [pict1;scale_picture (h1/.h2,h1/.h2) pict2];;

let besides = besides_picture;;

let over_picture (pict1:picture) (pict2:picture) =    
    let w1 = picture_width pict1  
    and w2 = picture_width pict2 
in align_vertically
      Align_Center
      [pict1;scale_picture (w1/.w2,w1/.w2) pict2];;

let over = over_picture;;


let compose_horizontally_picture_list = function 
  [] -> failwith "pictures__compose_horizontally_picture_list : empty picture list"
| (p::pl) ->  it_list besides_picture p pl;;

let compose_horizontally =  compose_horizontally_picture_list;;

let compose_vertically_picture_list = function 
  [] -> failwith "pictures__compose_vertically_picture_list : empty picture list"
| (p::pl) ->  it_list over_picture p pl;;

let compose_vertically = compose_vertically_picture_list;;


let extend_picture_frame str k {pict=p;frameP=fr;exact_frameP=b; hullP=h;
                                input_interface=ip;output_interface=op} =
  if not (get_exact_frame_mode())
   then    {pict=p;frameP=extend_frame str k fr;exact_frameP=false; hullP=[];
            input_interface=ip;output_interface=op}
   else failwith "extendpictureh_frame: not authorized in exact frame mode";;




(* Operations on pictures using interfaces *)

let named_attach_pictures (p1,p2) (a1,a2) =
  let handle1 = find_handle a1 p1.output_interface
  and handle2 = find_handle a2 p2.input_interface
  in
    let tr = handle_transform handle1 handle2
    in
    if not (get_exact_frame_mode())
    then
      {pict=Cpict [p1.pict;Tpict(tr,p2.pict)];
       frameP=merge_frames [p1.frameP; transform_frame tr p2.frameP];
       exact_frameP=false;
       hullP=[];
       input_interface= 
               join_interfaces (p1.input_interface,
                                rem_port a2 (transform_interface tr 
			                      p2.input_interface));
       output_interface= 
               join_interfaces (rem_port a1 p1.output_interface,
                                transform_interface tr p2.output_interface)}
    else let h1 = if p1.exact_frameP then p1.hullP
                     else compute_pict_hull p1.pict
         and h2 = if p2.exact_frameP then p2.hullP
                     else compute_pict_hull p2.pict in
         let h2' = map (transform_point tr) h2 in
         let h = merge_convex_hulls [h1; h2'] in
         let fr= frame_of_convex_hull h in
      begin
      p1.frameP<-frame_of_convex_hull h1;p1.exact_frameP<-true;p1.hullP<-h1;
      p2.frameP<-frame_of_convex_hull h2;p1.exact_frameP<-true;p2.hullP<-h2;
      {pict=Cpict[p1.pict;Tpict(tr,p2.pict)];
       frameP=fr;  exact_frameP=true;  hullP=h;
       input_interface= 
               join_interfaces (p1.input_interface,
                                rem_port a2 (transform_interface tr 
			                      p2.input_interface));
       output_interface= 
               join_interfaces (rem_port a1 p1.output_interface,
                                transform_interface tr p2.output_interface)}
     end
;;

let attach_pictures (p1,p2) =
   match (p1.output_interface,p2.input_interface)
   with  (One_handle (a,b) (* handle1 *) , One_handle (c,d) (* handle2*) )
         ->     let tr = handle_transform (a,b) (*handle1*) (c,d) (* handle2*)
        in
    if not (get_exact_frame_mode())
    then
      {pict=Cpict[p1.pict;Tpict(tr,p2.pict)];
       frameP=merge_frames [p1.frameP; transform_frame tr p2.frameP];
       exact_frameP=false;
       hullP=[];
       input_interface= p1.input_interface;
       output_interface=transform_interface tr p2.output_interface}
    else let h1 = if p1.exact_frameP then p1.hullP
                     else compute_pict_hull p1.pict
         and h2 = if p2.exact_frameP then p2.hullP
                     else compute_pict_hull p2.pict in
         let h2' = map (transform_point tr) h2 in
         let h = merge_convex_hulls [h1; h2'] in
         let fr= frame_of_convex_hull h in
      begin
      p1.frameP<-frame_of_convex_hull h1;p1.exact_frameP<-true;p1.hullP<-h1;
      p2.frameP<-frame_of_convex_hull h2;p1.exact_frameP<-true;p2.hullP<-h2;
      {pict=Cpict[p1.pict;Tpict(tr,p2.pict)];
       frameP=fr;  exact_frameP=true;  hullP=h;
       input_interface=  p1.input_interface;
       output_interface= transform_interface tr p2.output_interface}
      end
    |   (No_handle,_) | (_,No_handle) -> failwith "Port missing"
    |   _ -> failwith "Use \"named_attach_pictures\" for pictures with named ports"
;;




let make_textblock_picture align spacing font c sl =
 let rec mtbp sl =
  match sl
  with
     []      ->     failwith "make_textblock_picture: empty string list"
   | [s]     ->     [make_text_picture font c s]
   | (s::sl) ->
       (let p = make_text_picture  font c s
       in let h= spacing
          and h'= -. (picture_frame p).ymin
	  in
	   let blank= make_blank_picture (h-.h',h-.h')
	   in
	     p::blank::mtbp sl)
	     
 in align_vertically align (mtbp sl);;

