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

(* $Id: graph.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* graph.ml        Graph and line algorithms for mlPicTex                *)
(*                 Ascander Suarez                                       *)
(*                 Mon Jun 21 1993                                       *)





#open  "MLgraph";;

#open "compatibility";;
#open "prelude";;
#open "geometry";;
#open "paint";;

#open "sketches";;

#open "texts";;
#open "pictures";;
#open "option";;




(* general combinators and list operations  *)


let fold f =  
  let rec fold_f a1 = function
    [] -> a1,[]
  | (b1::bl) -> let a2,c2 = f a1 b1 in let a,cl = fold_f a2 bl in a,c2::cl
  in fold_f;;

let try_find f = 
  let rec tryRec = function
  [] -> raise Not_found | (x::l) -> (try f x with _ -> tryRec l)
  in  tryRec;;

let nth i l = 
  let rec nthRec = function
     (_,[]) -> raise (Failure "nth")
   | (0,(x::_)) -> x
   | (i,(_::l)) -> nthRec(i-1,l)
  in if i<0 then raise (Failure "nth") else nthRec (i,l);;

let substituteNth i fn (l: (graph * pos) list) = 
  let rec nthRec = function
     (_,[]) -> raise (Failure "nth")
   | (0,(x::l)) -> fn x::l
   | (i,(x::l)) -> x::nthRec(i-1,l)
  in if i<0 then raise (Failure "nth") else nthRec (i,l);;

(* different algorithms for producing lines and arrows from control points *)

(* Operations on floats *)

let sqr x = x*.x;;

(* Operations on points *)

let pAdd p1 p2 = {xc=p1.xc+.p2.xc; yc= p1.yc+.p2.yc}
and pSub p1 p2 = {xc=p1.xc-.p2.xc; yc= p1.yc-.p2.yc};;
let pi = acos (-.1.);;
let circlePoint r ang = {xc=r*.cosinus ang;yc=r*.sinus ang};;
let pMult p x = {xc=p.xc*.x;yc=p.yc*.x};;

(* Operations on lines *)
let lengthOfLine(p1,p2) = 
  let dx = p2.xc-.p1.xc and dy = p2.yc-.p1.yc in sqrt(sqr dx +. sqr dy) ;;

let slopeOfLine (p1,p2) = 
  let dx = p2.xc-.p1.xc and dy = p2.yc-.p1.yc in
  let long = sqrt(sqr dx +. sqr dy) in
   if le_float 0. dy  then 180.*.acos (dx/.long)/.pi
    else     360. -. 180.*.acos (dx/.long)/.pi;;

(* A general draw call with optional arguments
sketch : float -> sketch -> picture
sketchGen : options -> float -> sketch -> picture
  (background, closed, dashPattern, dashedLine, fill,
   fillStyle, foreground, lineCap, lineJoin, lineWidthCoef)

  bool "dashedLine" false
  color "background" white
  color "foreground" black
  dashPattern (if dashedLine then [4;2] else [])
  fillStyle Nzfill
  float "lineWidthCoef" 1.0
  lineCap Roundcap
  lineJoin Roundjoin
  option "closed" Absent
  option "fill" Absent
*)

let sketchGen opts d sk = 
  if theOption opts "fill" or theOption opts "fillStyle" then 
      make_fill_picture
        (theFillStyle opts Nzfill,theColor opts "background" white) sk
  else
   let f = (if theOption opts "closed" then make_closed_draw_picture
            else make_draw_picture)
   and w = d*.theFloat opts "lineWidthCoef" 1.0*.0.01 in
   let sty = {linewidth=w;
              linecap=theLineCap opts Roundcap;
              linejoin=theLineJoin opts Roundjoin;
              dashpattern=if theBool opts "dashedLine" false then [4;2] else 
                          theDashPattern opts []} in
   if theBool opts "shadowSketch" true then
       group_pictures
        [f (sty,theColor opts "shadow" (Gra 1.0))
           (transform_sketch (translation(w/.2.0,-.w/.2.)) sk);
         f (sty,theColor opts "foreground" black) sk]
   else f (sty,theColor opts "foreground" black) sk;;

let sketch = sketchGen [];;

(* Computes the point and the tangent angle of the x'th part of a curve  *)
let curvePos (p1,p2,p3,p4) x =
  if lt_float x 0. or gt_float x  1. then raise (Failure "curvePos");
  match int_of_float(x*.32.) with
    0 -> p1,slopeOfLine(p1,p2)
 | 32 -> p4,slopeOfLine(p3,p4)
 | i ->  
   let rec 
   findPos (p1,p2,p3,p4) (n,m) = (* m < i < m+n *)
     let p2'  = pMult(pAdd p1 p2) 0.5
     and pc   = pMult(pAdd p2 p3) 0.5
     and p3'' = pMult(pAdd p3 p4) 0.5 in
     let p3'  = pMult(pAdd p2' pc) 0.5
     and p2'' = pMult(pAdd pc p3'') 0.5 in
     let pc'  = pMult(pAdd p3' p2'') 0.5
     and j=n+m in
      if i=j then pc', slopeOfLine(p3',p2'')
      else if i<j then findPos(p1,p2',p3',pc') (n,m/2)
                  else findPos(pc',p2'',p3'',p4) (j,m/2)
   in findPos (p1,p2,p3,p4) (0,16);;

(* Draws an arrow directed to form at p tangent to ang *)
let arrowFormGen args (p,ang) form = 
   let long = theFloat args "arrowLengthCoef" 1.0*.0.05 in
    match form with
     "B" -> [[Seg[pAdd p (circlePoint long (ang+.45.)); p;
                  pAdd p (circlePoint long (ang-.45.))]]]
    | ("FB"|"BF") ->
            [[Seg[pAdd p (circlePoint long (ang+.135.)); p;
                  pAdd p (circlePoint long (ang-.45.))]];
             [Seg[pAdd p (circlePoint long (ang-.135.)); p;
                  pAdd p (circlePoint long (ang+.45.))]]]
            (* make_sketch[Seg [p]; Arc(p,20.,30.,330.);Seg[p]] *)
    | _ ->  [[Seg[pAdd p (circlePoint long (ang+.135.)); p;
              pAdd p (circlePoint long (ang-.135.))]]];;


(* drawing Text 
text : string -> picture
textGen : options -> string -> picture
 (font, fontSize, foreground)
 color "foreground" black
 float "fontSize" 12.0
 font Courier
*)

let textGen opts s =
  let fontName = theFont opts Courier
  and fontSize = theFloat opts "fontSize" 12.0
  and col = theColor opts "foreground" black in
  make_text_picture (make_font fontName fontSize) col s;;

let text  = textGen [];;


(* Operations on frames *)

let diagOfFrame fr = sqrt(sqr(fr.xmax-.fr.xmin)+.sqr(fr.ymax-.fr.ymin));;

let blankSketch opts sk =
   let d = 1.5*.diagOfFrame (sketch_frame sk) in
   group_pictures [sketchGen(option "fill"::opts) d sk;
                   sketchGen(option "closed"::opts) d sk];;

let rectOfFrame opts {xmin=xmin;xmax=xmax;ymin=ymin;ymax=ymax} =
     let sep = theFloat opts "frameDistanceCoef" 1.0*.5.0 in
     blankSketch opts (make_sketch[
        Seg[{xc=xmin-.sep;yc=ymin-.sep}; {xc=xmin-.sep;yc=ymax+.sep};
	    {xc=xmax+.sep;yc=ymax+.sep}; {xc=xmax+.sep;yc=ymin-.sep} ]]);;

(*
rectangle : picture -> picture
rectangleGen : options -> picture -> picture
  (background, dashPattern, dashedLine, fillStyle,
   foreground, frameDistanceCoef, lineCap, lineJoin, lineWidthCoef)
*)

let rectangleGen opts p = (* defaults: {Sep = 5.0} *)
   let fr= picture_frame p in group_pictures 
             [rectOfFrame opts fr;p];;

let rectangle = rectangleGen [];;

let circOfFrame opts {xmin=xmin;xmax=xmax;ymin=ymin;ymax=ymax} =
 let sep = theFloat opts "frameDistanceCoef" 1.0*.5.0 in
 let dx = (xmax-.xmin)/.2. and dy = (ymax-.ymin)/.2. in
 blankSketch opts (make_sketch[
        Arc({xc=xmin+.dx;yc=ymin+.dy},sqrt(dx*.dx+.dy*.dy)+.sep,0.,360.)]);;

(*
circle : picture -> picture
circleGen : options -> picture -> picture
  (background, dashPattern, dashedLine, fillStyle,
   foreground, frameDistanceCoef, lineCap, lineJoin, lineWidthCoef)
*)

let circleGen opts p =  (* defaults: {Sep = 5.0} *)
   let fr= picture_frame p in group_pictures 
             [circOfFrame opts fr;p];;

let circle = circleGen [];;

let ovalOfFrame =
 let sq = make_sketch[Arc({xc=0.0;yc=0.0},sqrt 2.0,0.,360.)] in
 fun opts {xmin=xmin;xmax=xmax;ymin=ymin;ymax=ymax} ->   (* defaults: {Sep = 5.0} *)
 let sep = theFloat opts "frameDistanceCoef" 1.0*.5.0 in
 let dx = (xmax-.xmin+.sep)/.2. and dy = (ymax-.ymin+.sep)/.2. in
 let tr = scaling(dx,dy) in  (blankSketch opts (transform_sketch tr sq));;

(*
oval : picture -> picture
ovalGen : options -> picture -> picture
  (background, dashPattern, dashedLine, fillStyle,
   foreground, frameDistanceCoef, lineCap, lineJoin, lineWidthCoef)
*)

let ovalGen opts p =
   let fr= picture_frame p in group_pictures 
             [ovalOfFrame opts fr ;center_picture p origin];;

let oval = ovalGen [];;

(* Draw (oriented) segments from a list of points *)
let linesOfPoints opts = function 
   [] -> []
 | (p1::pl as pts) -> if theOption opts "noLine" then [] else 
    let p = opts,[[Seg pts]]
    and form = theString opts "arrowDir" "" in
    if form="" then [p] else
    let pos = theFloat opts "arrowPos" 0.5
    and aOpts =  (bool "dashedLine" false::dashPattern []::opts) in
    [p;aOpts, it_list append [] (*flat_map (fun x -> x)*)
             (snd(fold (fun p1 p2 -> p2,
               arrowFormGen opts (pMult(pAdd p1 p2) pos,slopeOfLine(p1,p2)) form)
                 p1 pl))];;

(* Draw a continous (oriented) curve from p1to p2
  such that for each i (1<=i<=n),the curve is tangent
  at point pi to a line of direction di *)

let curveOfPoints opts = function 
   [] -> []
 | ((p1,a1)::pl) ->  if theOption opts "noLine" then [] else 
     let openCoef = theFloat opts "openCoef" 1.0*. 0.5
     and form = theString opts "arrowDir" "" in
     let _,elems = 
      fold (fun (p1,a1) (p2,a2) -> 
         let c1,c2 = if p1<>p2 then
            pAdd p1 (circlePoint openCoef a1),
            pSub p2(circlePoint openCoef a2)
         else 
            pAdd p1(circlePoint (openCoef*.2.) a1),
            pSub p2(circlePoint (openCoef*.2.) a2) in
         (p2,a2),Curve(p1,c1,c2,p2)) (p1,a1) pl in
    let p = opts,[elems] in
    if form="" then [p] else
    let pos = theFloat opts "arrowPos" 0.5
    and aOpts =  (bool "dashedLine" false::dashPattern []::opts) in
    [p;aOpts, it_list append [] 
              (map (function (Curve (a,b,c,d)) 
                             -> arrowFormGen opts (curvePos (a,b,c,d) pos) form
                            | _ -> raise (Failure "curveOfPoints"))
       elems)];;

(* draw symmetric (oriented) curves between points pi and pi+1.
   if p_i=p_{i+1} draw a loop centered to direction "loopDir" (the north) *)

let symmetricCurvesOfPoints opts = function 
   (_,[]) -> []
 | (p1,pl) ->  if theOption opts "noLine" then [] else 
     let openCoef = theFloat opts "openCoef" 1.0*. 0.5
     and loopDir = theFloat opts "loopDir" 90.0 
     and form = theString opts "arrowDir" "" in
     let _,elems = 
      fold (fun p1 (dAng,p2) -> 
         let c1,c2 =
            if p1=p2 then
                pAdd p1 (circlePoint (openCoef*.2.) (loopDir+.dAng)),
                pAdd p2 (circlePoint (openCoef*.2.) (loopDir-.dAng))
            else let a = slopeOfLine(p1,p2) in
                pAdd p1 (circlePoint openCoef (a+.dAng)),
                pAdd p2 (circlePoint openCoef (a-.180.-.dAng)) in
         p2,Curve(p1,c1,c2,p2)) p1 pl in
    let p = opts,[elems] in
    if form="" then [p] else
    let pos = theFloat opts "arrowPos" 0.5
    and aOpts =  (bool "dashedLine" false::dashPattern []::opts) in
    [p;aOpts,it_list append [] (
      map (function (Curve (a,b,c,d)) 
                      -> arrowFormGen opts (curvePos (a,b,c,d) pos) form
                    | _ -> raise (Failure "curveOfPoints")) elems)];;

(* Draw a continuous curve external points p1,..,pn,p1 at a distance "frameDistanceCoef".
   Points most be given in a clockwise order *)

let hullOfPoints opts = 
   let long = theFloat opts "frameDistanceCoef" 1.0*.0.3
   and openCoef = theFloat opts "openCoef" 1.0*.1.48 in function 
     [] -> [] 
   | [p1] -> if theOption opts "noLine" then [] else
      [opts,[[Arc(p1,long,0.,360.)]]]
   | [p1;p2] ->  if theOption opts "noLine" then [] else
       let ang = slopeOfLine(p2,p1) in
       let pPar,pPerp= circlePoint long ang,circlePoint (long*.openCoef) (ang+.90.) in
       let c1,c4 = pAdd p1 pPar,pSub p2 pPar in
      [opts,[[Curve(c1,pAdd c1 pPerp,pAdd c4 pPerp,c4); 
              Curve(c4,pSub c4 pPerp,pSub c1 pPerp,c1)]]]
   | (p1::p2::p3::l) ->  if theOption opts "noLine" then [] else
      let ang = 
          if p1=p3 then slopeOfLine (p1,p2)-.90. else slopeOfLine(p1,p3) in
      let pPerp,pPar1 = circlePoint long (ang+.90.),circlePoint (long*.openCoef) ang in
      let c1 = pAdd p2 pPerp in 
      let _,elems = fold
        (fun(c1,pPar1,p1,p2) p3 -> 
          let ang = 
              if p1=p3 then slopeOfLine (p1,p2)-.90. else slopeOfLine(p1,p3) in
          let pPerp,pPar4 = circlePoint long (ang+.90.),circlePoint (long*.openCoef) ang in
          let c4 = pAdd p2 pPerp in
          (c4,pPar4,p2,p3),Curve(c1,pAdd c1 pPar1,pSub c4 pPar4,c4)) 
               (c1,pPar1,p2,p3) (l@[p1;p2;p3]) in
      [opts,[elems]];;

(* Draw a continuous curve external points p1,..,pn,p1 at a distance "frameDistanceCoef".
   Points most be given in a clockwise order *)

(* let cHullOfPoints opts pts = 
   let long = theFloat opts "frameDistanceCoef" 1.0*.0.3 in
   if theOption opts "noLine" then [] else
      [opts,map (fun p -> [Arc(p,long,0.,360.)]) pts]@hullOfPoints opts pts;; *)



(* associates a float to a direction *)
let degOfDir = 
  function De -> 0. | Dne -> 45. | Dn -> 90. | Dnw -> 135.
    | Dw -> 180. | Dsw -> 225. | Ds -> 270. | Dse -> 315.
    | (Deg x) -> x;;


let transformGraph tr = function
   (TGraph(tr1,g)) -> TGraph(ctrans tr tr1,g)
 | g -> TGraph(tr,g);;

let graphPoint s g =
  let rec findPoint =function
   (Graph sg) -> if s=sg then origin else raise Not_found
 | (PGraph(g,pts)) -> (try findPoint g with _ -> assoc s pts)
 | (LGraph(g,_)) -> findPoint g
 | (TGraph(tr,g)) -> transform_point tr (findPoint g)
 | (CGraph(g1,g2)) -> try findPoint g1 with _ -> findPoint g2
  in try findPoint g with _ -> raise (Failure ("Point \""^s^"\" not in graph"));;


let graphLineLabel (r,pos) g  s =
 let rec findLine =
 function (Graph sg) ->  raise Not_found
   | (PGraph(g,_)) ->  findLine g
   | (LGraph(g,lines)) ->
       (try findLine g with _ ->
         try_find 
         (function (lOpts,(Seg(p1::p2::l)::_)::_) -> 
                 (match assoc "lineName" lOpts with
                  SOption name -> if s=name then
                  pAdd (pMult (pAdd p1 p2) pos) (circlePoint r (slopeOfLine(p1,p2)+.90.))
                  else raise Not_found
                  | _ -> raise Not_found)
            | (lOpts,(Curve(c1,c2,c3,c4)::_)::_) -> 
                 (match assoc "lineName" lOpts with
                  SOption name -> if s=name then
                    let p,ang = curvePos (c1,c2,c3,c4) pos in
                    pAdd p (circlePoint r (ang+.90.))
                  else raise Not_found
                  | _ -> raise Not_found)
            | _ -> raise Not_found)  lines)
   | (TGraph(tr,g)) -> transform_point tr (findLine g)
   | (CGraph(g1,g2)) -> try findLine g1 with _ -> findLine g2
  in try findLine g with _ -> raise (Failure ("Line "^s^" not in graph"));;

let nodeGraph name = Graph name;;
let addLines g =
  function [] -> g
    | lines ->
  let skl = it_list append [] (map
   (function (GLine (opts,l)) ->
         linesOfPoints opts (map (fun s -> graphPoint s g) l)
     | (GCurve(opts,l)) ->
         curveOfPoints opts (map (fun (s,d) -> graphPoint s g,degOfDir d) l)
     | (GSCurve(opts,s1,l)) ->
         symmetricCurvesOfPoints opts
           (graphPoint s1 g,map (fun (a,s) -> a,graphPoint s g) l)
     | (GHull(opts,l)) ->
         hullOfPoints opts (map (fun s -> graphPoint s g) l)) lines)
   in LGraph(g,skl);;

let addPoints opts g pts0 =
  let sep = theFloat opts "frameDistanceCoef" 1.0*.0.5 in
  PGraph(g, map (fun (l,(p,pos)) -> p,graphLineLabel (sep,pos) g l) pts0);;

let polyGraph cName nodes =
  let n = list_length nodes in
  let ang = 360.0/.float_of_int n in
  let r = 1./.lengthOfLine({xc=1.;yc=0.},circlePoint 1. ang) in
  let _,pts = it_list 
    (fun x y -> match (x,y) with
         (deg,pts) , "" -> (deg+.ang,pts)
       | (deg,pts) , name -> (deg-.ang,(name,circlePoint r deg)::pts))
    (90.,[]) nodes in addLines (PGraph(Graph cName,pts));;


let tabularGraph name style table =
  let _,rows = it_list 
    (fun (j,l) line -> j-.1.0,(j,it_list 
      (fun x y -> match (x,y) with
        (l,(i,mx,mn)) ,(V n) -> 
        let i'=i+.n in (l,(i',max_float mx i',min_float mn i'))
         | (l,(i,mx,mn)) , (L s) -> ((s,i)::l,(i+.1.,mx,mn)))
      ([],(0.,0.,0.)) line)::l) (0.,[]) table in 
   let points = match style with
      Left -> it_list append [] (map (fun (y,(points,_)) -> 
                  map (fun (s,x) -> (s,{xc=x;yc=y})) points) rows)
    | Right -> it_list append [] (map (fun (y,(points,(_,dx,_))) -> 
                  map (fun (s,x) -> (s,{xc=x-.dx;yc=y})) points) rows)
    | Center -> it_list append [] (map (fun (y,(points,(_,mx,mn))) -> 
                  map (let dx=(mx-.mn)/.2. in
                       fun (s,x) -> (s,{xc=x-.dx;yc=y})) points) rows) in
   addLines (PGraph(Graph name,rev points));;

let linkGraphs (g1,s1) (g2,s2)  =
  let p1 = graphPoint s1 g1
  and p2 = graphPoint s2 g2 in
  addLines (CGraph(g1,transformGraph (translation (p1.xc-.p2.xc,p1.yc-.p2.yc)) g2));;

let composeGraphs (g1,s1,s2) (g2,s3,s4) =
  let p1 = graphPoint s1 g1
  and p2 = graphPoint s2 g1
  and p3 = graphPoint s3 g2
  and p4 = graphPoint s4 g2 in
  addLines(CGraph(g1,transformGraph (handle_transform(p1,p2) (p3,p4)) g2));;

let insLineGen objs (opts,a,dir,b) =
 let pointOfGraphs a = 
  let rec find i = function
    [] -> raise (Failure ("Point \""^a^"\" not in Graph"))
  | ((g,AnyPos)::l) ->
      (try i,graphPoint a g with (Failure _) -> find (i+1) l)
  | ((g,RelPos j)::l) ->
      (try let p=graphPoint a g in derefObj j,p
       with (Failure _) -> find (i+1) l)
 and derefObj i = match nth i objs with (_,AnyPos) -> i | (_,RelPos j) -> derefObj j 
   in find 0 objs 

in
 let i,pa = pointOfGraphs a
 and j,pb = pointOfGraphs b in
  if i=j then objs,
    if pa=pb then
      GSCurve((float"loopDir" (degOfDir dir)::opts),a,[45.,b])
    else GSCurve(opts,a,[degOfDir dir-. slopeOfLine(pa,pb),b])
  else
   let trFun = 
     let pt = pAdd (pSub pa pb)
          (circlePoint (theFloat opts "lineLengthCoef" 1.) (degOfDir dir))
 in
   transformGraph (translation (pt.xc,pt.yc)) in
   map (function (g,RelPos k as pair) -> if k=j then (trFun g,RelPos i) else pair
          | pair -> pair)
       (substituteNth j (fun (g,_) -> g,RelPos j) objs),
    GLine(opts,[a;b])
;;

let assembleGraphs gl pts lines =
  let objs = map (fun s -> nodeGraph s,AnyPos) pts@map (fun g -> g,AnyPos) gl in
  match fold insLineGen objs lines with
  (((g1,_)::newObjs),gLines) ->
    addLines (it_list (fun g1 (g2,_) -> CGraph(g1,g2)) g1 newObjs) gLines
 | _ -> raise (Failure "assembleGraphs");;

let pictOfLines tr d gOpts l = it_list append [] (map
  (fun (lOpts,skl) ->
         let opts = lOpts@gOpts in
         map (compose (sketchGen opts d)
                (compose  (transform_sketch tr) (make_sketch))) skl) l);;

let rec skeletonOfGraphGen opts (tr1,d) = function
    (TGraph(tr2,g)) -> skeletonOfGraphGen opts (ctrans tr1 tr2,d) g
  | (LGraph(g1,lines)) ->
       skeletonOfGraphGen opts (tr1,d) g1@pictOfLines tr1 d opts lines
  | (CGraph(g1,g2)) ->
       skeletonOfGraphGen opts (tr1,d) g1@skeletonOfGraphGen opts (tr1,d) g2
  | (PGraph(g1,pts2)) ->
       skeletonOfGraphGen opts (tr1,d) g1
  | (Graph name) -> [];;


let graphGen opts g nodes =
  let tr =
   match theString opts "graphStyle" "frame" with
     "diagonal" -> 
      let d = theFloat opts "nodesCoef" 1.0*.
          1.5 *.list_it 
                (compose max_float
                   (compose diagOfFrame (compose picture_frame snd)))
                nodes 
                10.0 in
       (scaling(d,d),d)
   | "frame" -> 
      let h,w = list_it (fun (_,n) (h,w)-> let f = picture_frame n in
                    max_float (f.ymax-.f.ymin) h,max_float (f.xmax-.f.xmin) w)
                 nodes (10.0,10.0)
      and coef = theFloat opts "nodesCoef" 1.0 in
       (scaling(1.5*.coef*.w,2.0*.coef*.h),1.5*.coef*.sqrt(sqr w+.sqr h))
   | str -> raise(Failure ("Unknown graphStyle "^str)) in
  let skel = skeletonOfGraphGen opts tr g in
  let g1 = transformGraph (fst tr) g in
  group_pictures (skel@
    map (fun (name,p) -> center_picture p (graphPoint name g1)) nodes);;

let graph  = graphGen [];;

(*

let mkLab s = (* pacman s *) circle(text(string_of_int s)) in
let labn = [1;2;3;4] in
let labs = map string_of_int labn in
  eps_file (graphGen [string"graphStyle" "diagonal"]
  (transformGraph (scaling(1.,1.))
   (polyGraph "p1" labs [GCurve([bool "dashedLine" true;
                                 string "arrowDir" "F"],
                                ["1",De;"2",Ds;"3",Dw;"4",Dn;"1",De]);
                         GSCurve([string "arrowDir" "F"],
                                 "1",[90.,"3";90.,"1";30.,"1"]);
                         GSCurve([string "arrowDir" "F"],
                                 "1",[90.,"2";90.,"1"])]))
   (map (fun s -> string_of_int s, mkLab s) labn)) "p1";;


let mkLab  s = circle(text s) (* pacman(int_of_string s) *) in
let labs1 = ["1";"2";"3";"4";"5";"6"] in
let labs2 = ["7";"8";"9";"10";"11";"12"] in
  eps_file (graphGen [string"graphStyle" "diagonal"]
   (transformGraph (Orotation (-.30.))
   (composeGraphs (polyGraph "p1" labs1 [GLine([],labs1@["1"])],"p1","2")
     (polyGraph "p2" labs2 [GLine([],labs2@["7"])],"7","9")
     [GHull([float "frameDistanceCoef" 1.],["5";"7";"8";"12";"4"]);
      GHull([float "frameDistanceCoef" 1.],["6";"1"])]))
   (map (fun s -> s,mkLab s) (labs1@["7";"8";"10";"12"]))) "p2";;

let mkLab  s = s,circle(text s) (* pacman(int_of_string s) *) in
eps_file (graphGen [string"graphStyle" "diagonal"]
   (polyGraph "g"
       ["a";"b";"c";"d";"e";"f"]
       [GHull([],
                  ["a";"b";"g";"c";"d";"g";"e";"f";"g"]);
        GHull([],["a";"b"])]) 
   (map mkLab ["a";"b";"c";"d";"e";"f";"g"])) "h1";;

let mkLab  s = s,circle(text s) (* pacman(int_of_string s) *) in
eps_file (graphGen [string"graphStyle" "diagonal"]
   (polyGraph "g"
       ["a";"b";"c";"d";"e";"f"]
       [GHull([],
                  ["a";"b";"c";"d";"e";"f"])]) 
   (map mkLab ["a";"b";"c";"d";"e";"f";"g"])) "h2";;

let mkLab  s = s,circle(text s) (* pacman(int_of_string s) *) in
eps_file (graphGen [string"graphStyle" "diagonal"]
  (assembleGraphs [] ["n";"s";"e";"w";"o"]
    [[],"o",Dn,"n";[],"o",Ds,"s";[],"o",De,"e";[],"o",Dw,"w";
     [],"n",Ds,"w";[],"n",Ds,"e";[],"s",Dn,"e";[],"s",Dn,"w"])
   (map mkLab ["n";"s";"e";"w";"o"])) "h3";;

let mkLab  s = s,circle(text s) (* pacman(int_of_string s) *) in
let mkLab2  s = s,text s (* pacman(int_of_string s) *) in
let p1 = assembleGraphs [] ["a";"b"]
   [[option "noLine";float "lineLengthCoef" 1.5],"a",De,"b";
    [string "lineName" "F";string "arrowDir" "F"],"a",Dnw,"b";
    [string "lineName" "G";string "arrowDir" "F"],"b",Dse,"a";
    [string "lineName" "IDA";string "arrowDir" "F"],"a",Dw,"a";
    [string "lineName" "IDB";string "arrowDir" "F"],"b",De,"b"]
in let p2 = addPoints []
             (addPoints [float "frameDistanceCoef" 0.5] p1 ["F",("f",0.5);"G",("g",0.5)])
                             ["IDA",("ida",0.5);"IDB",("idb",0.5)]
in eps_file(graph (transformGraph (scaling(1.0,0.8)) p2)
   (map mkLab ["a";"b"]@map mkLab2["f";"g";"ida";"idb"])) "c1";;


*)
