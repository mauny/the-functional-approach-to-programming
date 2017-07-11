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

(* $Id: tree.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* tree.ml         Tree drawing algorithms for mlPicTex                  *)
(*                 Guy Cousineau & Ascander Suarez                       *)
(*                 Tue Jun 30 1992                                       *)

(*  Author:  Guy Cousineau                                               *)
(*  Creation: 30/6/92                                                    *)
(*  Updates: 4/5/92 (Ascander Suarez) btrees => trees and proof trees    *)
(*  This file contains functions to draw trees                           *)
(*  The main function is makeTreePicture                                 *)





#open  "MLgraph";;

#open "compatibility";;
#open "prelude";;
#open "geometry";;
#open "frames";;
#open "paint";;
#open "fonts";;
#open "texts";;
#open "sketches";;
#open "pictures";;
#open "option";;
#open "graph";;




let fold f =  
  let rec fold_f a1 = function
    [] -> a1,[]
  | (b1::bl) -> let a2,c2 = f a1 b1 in let a,cl = fold_f a2 bl in a,c2::cl
  in fold_f;;

let rec tree_it f t x =
match t with 
   Nil ->  x
| Node {info=a;sons=sons;label=lab} 
       ->  list_it (tree_it f)  sons (f a x lab);;

let rec tree_map f t =
match t with 
   Nil ->  Nil
| Node {info=a;sons=sons;label=lab} 
       ->  Node{info=f a;sons=map (tree_map f)  sons;label=lab};;


(* function drawTree requires parameters                               *)
(* drn : the function for drawing nodes  (drawTree assumes that this   *)
(*       function operates in "fill" mode)                             *)
(* drl : the function for drawing labels (drawTree assumes that this   *)
(*       function operates in "fill" mode)                             *)
(* h: the height (distance between tree levels)                        *)
(* d: the distance between 2 brother nodes at level 1                  *)
(* cl : the coefficient list which gives for each level the ratio      *)
(*      between distance at level (n+1) and at level n                 *)
(* pt: the point where the root should be placed                       *)

let drawTree opts  (drn,drl) (h,d,cl,pt) = 
  let draw = sketchGen opts d
  in let rec drawR (d,cl,({xc=x; yc=y} as pt),ori) = function 
     Nil -> failwith "Cannot draw an empty tree"
   | Node{info=a;sons=[];label=lab}
      -> (center_picture (drn a) pt::
          (match lab with Nolabel -> []|Label b -> [drl (ori,pt) b]))
   | Node{info=a;sons=sons;label=lab}
      -> let d=d*.(hd cl) and coef = float_of_int(1-list_length sons)/.2.0
         in let pts,_ = list_it 
             (fun t (l,c) -> ((t,{xc=x+.c*.d;yc=y-.h})::l),c+.1.0)
             (rev sons) ([],coef)
         in 
             it_list append [] (map (function (Nil,_) -> []
                         | (t1,pt1) -> 
		          draw (make_sketch [Seg [pt;pt1]])::
                          drawR (d,tl cl,pt1,pt) t1) pts)
            @(center_picture (drn a) pt::
              (match lab with Nolabel -> []|Label b -> [drl (ori,pt) b]))
      in compose group_pictures (drawR (d,cl,pt,{xc=pt.xc;yc=pt.yc+.h}));;

(* function drawProofTree requires parameters                          *)
(* h: the height (distance between tree levels)                        *)
(* d: the distance between 2 brother nodes at level 1                  *)
(* cl : the coefficient list which gives for each level the ratio      *)
(*      between distance at level (n+1) and at level n                 *)
(* pt: the point where the root should be placed                       *)

let widthOfInfo = function
   Nil -> raise (Failure "widthOfInfo")
 | Node{info=a;sons=sons;label=lab} -> let f=picture_frame a in (f.xmax-.f.xmin);;

let drawProofTree opts sep (h,d,cl,pt) tree =
  let pos = lt_float 0. (theFloat opts "treeLabelPos" 0.5)
  and draw = sketchGen opts ((h+.d)/.2.0)
  and h2 = h/.2.0
  in let rec drawR (d',cl) (t,({xc=x; yc=y} as pt)) = match t with
     Nil -> failwith "Cannot draw an empty tree"
   | Node{info=a;sons=[];label=lab}
      -> [center_picture a pt]
   | Node{info=a;sons=sons;label=lab}
      -> let d=d'*.(hd cl) and coef = float_of_int(list_length sons-1)/.2.0
         in match fst(list_it 
             (fun arg1 arg2 -> match (arg1,arg2) with
                   Nil ,(l,c) -> l,c-.1.0
                |   t , (l,c) -> ((t,{xc=x+.c*.d;yc=y+.h})::l),c-.1.0)
             sons ([],coef))
         with [] -> [center_picture a pt]
         | pts ->
           let lw = widthOfInfo t/.2.0 in
           let wLeft = max_float (coef*.d+.widthOfInfo (fst(hd pts))/.2.0) lw
           and wRight = max_float (coef*.d+.widthOfInfo (fst(hd (rev pts)))/.2.0) lw in 
              it_list append [] (map (drawR (d,tl cl)) pts)
              @(draw
                (make_sketch [Seg [
                   {xc=x-.wLeft;yc=y+.h2};
                (* {xc=x;yc=y+.h2};{xc=x;yc=y+.h2-.10.0*.LS.linewidth};{xc=x;yc=y+.h2}; *)
                   {xc=x+.wRight;yc=y+.h2}]])::
               center_picture a pt::
               (match lab with Nolabel -> []|Label b ->
                [center_picture b
                 {xc=x+.(if pos then wRight+.sep else -.(wLeft+.sep));
                  yc=y+.h2}]))
      in group_pictures(drawR (d,cl) (tree,pt));;


(*  Various functions for drawing nodes 


let drawNode r a =
    make_fill_picture  (Nzfill,white)
           (make_sketch [Arc(origin,r,0.0,360.0)]);;

let drawStringNode r a =
  let s= center_picture 
           (make_text_picture (make_font Helvetica r) black a)
	   origin
  and f= make_fill_picture  (Nzfill,white)
           (make_sketch [Arc(origin,r,0.0,360.0)])
  and c= make_draw_picture  ({linewidth= r*.0.1;linecap=Buttcap;
                              linejoin=Miterjoin;dashpattern=[]}
			     ,black)
           (make_sketch [Arc(origin,r,0.0,360.0)])
  in group_pictures [f;c;s];;

let drawIntNode r n = drawStringNode r (string_of_int n);;   
*) 

(*  How to compute the coefficient list                                    *)

(*  The function "computeCoefList" recursively computes for each subtree   *)
(*  an information which has shape (cl,trl)                                *)
(*  cl is the list of reduction coefficients to be applied at each level   *)
(*       it is the information which will finally by used by "drawTree"    *)
(*  trl is a list of triples  (l,r,c) where                                *)
(*       l is the horiz distance between tree root and leftmost node       *)
(*         at the given level                                              *)
(*       r is the horiz distance between tree root and rightmost node      *)
(*         at the given level                                              *)
(*       c is the ration between distance between brother node at the      *)
(*         given level and the same distance at level 1                    *)

(*  For a given binary tree t= N(t1,t2) the function "computeCoefList"     *)
(*  first computes (cl1,trl1) and (cl2,trl2) for t1 and t2                 *)
(*  Then cl1 and cl2 are combined by taken the minimum coefficient at each *)
(*  level giving a new list cl                                             *)
(*  Then, using this new coef list, trl1 and trl2 are recomputed by        *)
(*  function "recomputeTriples" giving trl1' and trl2'                     *)
(*  Then, the function "computeHeadCoef" computes  for each level what     *)
(*  should be the reduction coefficient to be applied at the root of tree  *)
(*  in order to have the rightmost node of t1 and the leftmost node of t2  *)
(*  be separated by distance c and takes the minimum of all these          *)
(*  coefficients                                                           *)
(*  The method is the following:                                           *)
(*  If t1 and t2 where drawn using cl1 and cl2, then the distance between  *)
(*  their roots should be at least r1-l2+c for t1 and t2 to behave nicely  *)
(*  at the given level. Therefore the root coefficient should be           *)
(*                  1/(r1-l2+c)                                            *)

(* The function "makeTreePicture" uses the final coef list by              *)
(*  dividing by their product  dMin, the minimal distance between          *)
(*  nodes to obtain the distance between the two sons of the root          *)


let rec minl = function
   [] -> []
 | [l] -> l
 | ([]::ll) -> minl ll
 | ((x::l)::ll) -> 
    let c,ll' = list_it
          (fun x y -> match x,y with
             [] , b -> b | (x::l),  (c,ll) -> min_float x c,(l::ll)) 
          ll (x,[l])
    in c::minl ll';;


let recomputeTriples cl = 
let rec recomp (n,cl) =
function   []     -> []
  | ((l,r,c)::ll) -> (l*.n/.c,r*.n/.c,n):: recomp (n*.(hd cl),tl cl) ll
in  recomp (hd cl,tl cl);;


let computeHeadCoef (trl1,trl2) =
  let rec compCoef =
function ([],_)    ->   []
    |    (_,[])    ->   []
    | ((_,r1,c)::ll1,(l2,_,_)::ll2)
          -> abs_float(1.0/.(r1-.l2+.c)) :: compCoef (ll1,ll2) 
in it_list min_float 1.0 (compCoef (trl1,trl2));;

let combineTriples coef x (trl1,trl2) = 
 let rec comb = 
function        [],[]    -> []
   | (l1,r1,c)::ll1 , [] -> (coef/.x+.l1,coef/.x+.r1,c) :: comb(ll1,[])
   | [] , (l2,r2,c)::ll2 ->
       ((coef+.1.0)/.x+.l2,(coef+.1.0)/.x+.r2,c) :: comb([],ll2)
   | (l1,r1,c)::ll1 , (l2,r2,_)::ll2 ->
       (coef/.x+.l1,(coef+.1.0)/.x+.r2,c) :: comb(ll1,ll2)
in (*(-.0.5,0.5,1.0)::*)comb (trl1,trl2) ;;

let scaleTriple x (l1,r1,c) = (l1*.x,r1*.x,c*.x);;

let computeCoefList t =   
  let  rec comp = function
      Nil   -> [1.0],[]
   |  Node {info=_;sons=[];label=_} -> [1.0],[]
   |  Node {info=_;sons=[t1];label=_}
         -> let (cl,trl) = comp t1
            in  (1.0::cl,(0.0,0.0,1.0)::trl)
   |  Node {info=_;sons=sons;label=_}
         -> let trls = map comp sons in
             let cl = minl(map fst trls) 
             and coef = float_of_int(1-list_length trls)/.2.0 in
             let rec compSons (pos,x) = function
            [] -> failwith "tree__compSons : empty list"
          | [trl1] -> (1.0::x::tl cl,
                       (coef,-.coef,1.0)::map (scaleTriple x) trl1)
          | (trl1::trl2::ll) -> 
                let x' = (computeHeadCoef (trl1,trl2))
                in compSons (pos+.1.0,(min_float x x')) 
                             (combineTriples pos x' (trl1,trl2)::ll)
             in compSons (coef,1.0) (map (compose (recomputeTriples cl) snd) trls)
in (compose fst  comp) t;;


let makeTreePictureGen opts drn (height,dMin,root) t =
  let coefList = computeCoefList t
  in let totalCoef = it_list mult_float 1.0 coefList
     in let d= dMin/.totalCoef
        in drawTree opts drn (height,d,coefList,root)  t;;

let makeTreePicture drn hdr t = makeTreePictureGen [] drn hdr t;;

let treeLabelPos (pos,sep) (p1,p2) =
      ({xc=p1.xc+.(p2.xc-.p1.xc)*.(min_float (pos+.0.1) 1.0)+.
           (if le_float p1.xc p2.xc  then sep else -.sep);
        yc=p1.yc+.(p2.yc-.p1.yc)*.(min_float (pos(* +.0.1 *)) 1.0)});;

let treeGen opts t = 
  let height,width = 
   tree_it (fun p (h,w) _ -> let fr=picture_frame p in
               max_float (fr.ymax-.fr.ymin) h,max_float (fr.xmax-.fr.xmin) w)
            t (0.0,0.0) in
  let h = theFloat opts "treeHeightCoef" 1.0*.2.0*.height
  and w = theFloat opts "treeWidthCoef" 1.0*.1.65*.width in
  let pos = theFloat opts "treeLabelPos" 0.5
  and sep = theFloat opts "sep" 1.0*.0.2*.w in
  makeTreePictureGen opts
       ((fun x->x),
        (fun arg x -> center_picture x (treeLabelPos(pos,sep) arg)))
       (h,w,origin) t;;

let tree = treeGen [];;

let makeProofTreePictureGen opts drn sep (height,dMin,root) t' =
  let t = tree_map drn t' in
  let coefList = computeCoefList t
  in let totalCoef = it_list mult_float 1.0 coefList
     in let d= dMin/.totalCoef
        in drawProofTree opts sep (height,d,coefList,root)  t;;

let makeProofTreePicture drn sep hdr t = makeProofTreePictureGen [] drn sep hdr t;;

let proofTreeGen opts t = 
  let height,width,lWidth = 
   tree_it (fun p (h,w,lw) lab -> let fr=picture_frame p in
        max_float (fr.ymax-.fr.ymin) h,max_float (fr.xmax-.fr.xmin) w,
        max_float (match lab with Nolabel -> 0.0
                      | Label(p) -> let fr = picture_frame p in (fr.xmax-.fr.xmin)) lw)
              t (0.0,0.0,0.0) in
  let sep = theFloat opts "sep" 1.0*.0.2*.width in
  makeProofTreePictureGen opts (fun x->x) sep
    (1.5*.height,max_float (1.1*.width) (width+.lWidth+.sep),origin) t;;

let proofTree = proofTreeGen [];;

let treeGraphGen = fun opts name t -> 
  let coefList = computeCoefList t in
  let d = 1./.list_it mult_float coefList 1. in
  let rec pointsOfTree =
    (fun x y -> match x,y with
          Nil , b  -> ([] : (string * point ) list)
       | (Node{info=name;sons=sons;label=lab}) ,
          ((d',(c::coefList)),({xc=x;yc=y} as ori)) ->
          let pos = float_of_int(1-list_length sons)/.2.0  and d = d'*.c in
          (name,ori)::it_list append []
            (snd(fold (fun (d,p) t ->
                        (d,(p+.1.0)),pointsOfTree t ((d,coefList),{xc=x+.p*.d;yc=y-.1.0}))
                       (d,pos) sons))
       | _, _ -> raise (Failure "treeGraph")) in
   let points = pointsOfTree t ((d,coefList),origin) in
  let g1 = PGraph(Graph name,points) in
  if theOption opts "treeLines" then
  let rec lines =
    function Nil -> []
      | (Node{info=a;sons=sons;label=lab}) ->
        it_list append [] (map (function Nil -> [] | (Node{info=b;sons=ssons;label=slabs} as t) ->
                  (match lines t with
                    [] -> [[assoc a points;assoc b points]]
                  | (l1::l) -> (assoc a points::l1)::l)) sons)
    in addLines (LGraph(g1,[opts,map (fun l -> [Seg l]) (lines t)]))
  else addLines g1;;

let treeGraph name t = treeGraphGen [] name t;;

(* Ex

let f s = Node{info=s;sons=[];label=Nolabel};;
let b s l = Node{info=s;sons=l;label=Nolabel};;


let t1 = b "a" [b "e" [f"f"];b "b" [b "d" [b "c" [f "h"]]];
               b "b" [b "d" [b "c" [f "h"]]]];;
eps_file(makeTreePicture(circPict o mkText,(fun _ x ->x)) (30.,30.,origin) t1) "t1";;

let f s = Node{info=drawIntNode 12. s;sons=[];label=Label(mkText(string_of_int s))};;
let b s l = Node{info=drawIntNode 12. s;sons=l;label=Label(mkText(string_of_int s))};;
let t2 = b 30 [b 12 [b 8 [b 4 [f 1;f 2;f 6;f 7];f 10];
                     b 20 [b 16 [f 14;f 18];b 24 [f 22;f 25;b 28 [f 26;Nil]]]];f 31;
               b 46 [b 38 [b 34 [f 32;f 36];b 42 [f 40;f 44]];
                     b 50 [f 48;b 52 [Nil;f 54]]]];;

eps_file(scale_picture (0.8,0.8) (treeGen[] t2))   "t2";;

let t3 = b 4 [f 2;b 40 [b 20 [b 12 [b 8 [f 6;f 10];b 16 [f 14;f 18]];
                              b 30 [f 24;f 34]];
                        f 21]];;

eps_file(scale_picture (0.8,0.8) (treeGen[] t2))   "t3";;

let t4  deg =
  (let f s = Node{info=(rotate deg (mkText s));sons=[Nil];label=Label(rotate deg (mkText s))} 
 and b s l = Node{info=(rotate deg (mkText s));sons=l;label=Label(rotate deg (mkText s))} in
 b "e" [f "0"; b "1" 
          [b "10" [f "100"; f "101"]; f"11";f"11";
           b "12" [f"120"; f"121"]]]);;

eps_file(translate(100.,100.) (rotate (90.) (proofTree (t4 (-.90.)))))
     "t4";;

let t5 = b"a"[b"b"[f"c";b"d"[Nil;Nil;f"e"];f"f"];b"g"[f"h";Nil;Nil]];;
eps_file(tree (tree_map (circPict o mkText) t5)) "t5";;
eps_file(makeGraphPictureGen 
  (treeGraphGen[option "treeLines"] "t" t5
               [GLine([],["h";"e"])])
         (tree_it (fun a l _ -> (a,circPict(mkText a))::l) t5 [])) "t6";;

*)



