#directory "../MLGRAPH.DIR";;
#open "MLgraph";;
#open "binary_trees";;
#open "lexer";;
#open "prelude";;

#infix "o";;

(* +tree_style+ *)
type tree_style =
   {vdist:float;
    hdist:float;
    coef_list:float list; 
    tlsty:linestyle;
    tcolor: color};;

(* +tree_style+ *)
(* +draw_btree+ *)

let draw_btree tsty t=
  let rec draw_r d cl ({xc=x; yc=y} as pt) = fun
    Empty -> make_blank_picture (0.0,0.0)
  | (Bin(Empty,pict,Empty))
    -> center_picture pict pt
  | (Bin(t1,pict,t2))
    -> let d=d*.(hd cl) in
       let pt1 = {xc=x-.d/.2.0;yc=y-.tsty.vdist}
       and pt2 = {xc=x+.d/.2.0;yc=y-.tsty.vdist} in
       let line1= make_draw_picture (tsty.tlsty,tsty.tcolor)
                    (make_sketch [Seg [pt;pt1]])
       and line2= make_draw_picture (tsty.tlsty,tsty.tcolor)
                    (make_sketch [Seg [pt;pt2]]) in
       match (t1,t2) with
         (_,Empty)
         -> group_pictures
              [line1; center_picture pict pt; draw_r d (tl cl) pt1 t1]
       | (Empty,_)
         -> group_pictures
              [line2; center_picture pict pt; draw_r d (tl cl) pt2 t2]
       | _
         -> group_pictures
              [line1; line2; center_picture pict pt;
               draw_r d (tl cl) pt1 t1; draw_r d (tl cl) pt2 t2]
  in draw_r tsty.hdist tsty.coef_list origin t;;

(* +draw_btree+ *)
(* Various functions for drawing nodes *)
(* +draw_string_node+ *)

let draw_string_node r a =
  let s = center_picture
            (make_text_picture (make_font Helvetica r) black a)
            origin
  and f = make_fill_picture (Nzfill,white)
            (make_sketch [Arc(origin,r,0.0,360.0)])
  and c = make_draw_picture ({linewidth= r*.0.1;linecap=Buttcap;
                              linejoin=Miterjoin;dashpattern=[]},
                             black)
            (make_sketch [Arc(origin, r, 0.0, 360.0)])
  in group_pictures [f;c;s];;

(* +draw_string_node+ *)
(* +draw_text_node+ *)

let draw_text_node r a =
  let s = center_picture
            (make_text_picture (make_font Helvetica r) black a)
            origin
  and f = make_fill_picture (Nzfill,white)
            (make_sketch [Arc(origin, r, 0.0, 360.0)])
  in group_pictures [f;s];;

(* +draw_text_node+ *)

(* +draw_int_node+ *)
let draw_int_node r n = draw_string_node r (string_of_int n);;

(* +draw_int_node+ *)

let draw_node r a =
    make_fill_picture  (Nzfill,white)
           (make_sketch [Arc(origin,r,0.0,360.0)]);;


(*
let draw_string_node r a =
 let t=make_text_picture (make_font Helvetica r) black a
 in let f= make_fill_picture (Nzfill,white)
               (frame_sketch (extend_frame Vertic_ext 0.5
                                 (picture_frame t)))
    in center_picture 
        (group_pictures [f;t])
	origin;;
*)


(* +compute_height_width+ *)

let compute_height_width (hcoef,wcoef) t=
  let (h,w)= it_btree
               (fun (x,y) p -> (max_float x (picture_height p),
                                max_float y (picture_width p)))
               (0.0, 0.0)
               t
  in (h*.hcoef, w*.wcoef);;

(* +compute_height_width+ *)


(*  How to compute the coefficient list                                    *)

(*  The function "compute_coef_list" recursively computes for each subtree *)
(*  an information which has shape (cl,trl)                                *)
(*  cl is the list of reduction coefficients to be applied at each level   *)
(*       it is the information which will finally by used by "draw_btree"  *)
(*  trl is a list of triples  (l,r,c) where                                *)
(*       l is the horiz distance between tree root and leftmost node       *)
(*         at the given level                                              *)
(*       r is the horiz distance between tree root and rightmost node      *)
(*         at the given level                                              *)
(*       c is the ration between distance between brother node at the      *)
(*         given level and the same distance at level 1                    *)

(*  For a given binary tree t= N(t1,t2) the function "compute_coef_list"   *)
(*  first computes (cl1,trl1) and (cl2,trl2) for t1 and t2                 *)
(*  Then cl1 and cl2 are combined by taken the minimum coefficient at each *)
(*  level giving a new list cl                                             *)
(*  Then, using this new coef list, trl1 and trl2 are recomputed by        *)
(*  function "recompute_triples" giving trl1' and trl2'                    *)
(*  Then, the function "compute_head_coef" computes  for each level what   *)
(*  should be the reduction coefficient to be applied at the root of tree  *)
(*  in order to have the rightmost node of t1 and the leftmost node of t2  *)
(*  be separated by distance c and takes the minimum of all these          *)
(*  coefficients                                                           *)
(*  The method is the following:                                           *)
(*  If t1 and t2 where drawn using cl1 and cl2, then the distance between  *)
(*  their roots should be at least r1-l2+c for t1 and t2 to behave nicely  *)
(*  at the given level. Therefore the root coefficient should be           *)
(*                  1/(r1-l2+c)                                            *)

(* The function "make_btree_picture" uses the final coef list by           *)
(*  dividing by their product  d_min, the minimal distance between         *)
(*  nodes to obtain the distance between the two sons of the root          *)

(* +compute_coef_list+ *)
let rec minl = fun
          ([],l2) -> l2
       |  (l1,[]) -> l1
       | (a1::l1,a2::l2) -> min_float a1 a2 :: minl(l1,l2);;


let recompute_triples cl =
  recomp (hd cl,tl cl)
  where rec recomp (n,cl) = fun
    [] -> []
  | ((l,r,c)::ll) -> (l*.n/.c ,r*.n/.c, n):: recomp (n*.(hd cl), tl cl) ll;;


let compute_head_coef (trl1,trl2) =
  it_list min_float 1.0 (comp_coef (trl1,trl2))
  where rec comp_coef = fun
    ([],_) -> []
  | (_,[]) -> []
  | ((_,r1,c)::ll1,(l2,_,_)::ll2)
    -> let d=(r1-.l2+.c) in
       (if d<=.0.0 then 1.0 else (1.0/.d))
       :: comp_coef (ll1,ll2);;

let combine_triples x (trl1,trl2) =
  (-.0.5, 0.5, 1.0) :: comb (trl1,trl2)
  where rec comb = fun
    ([],[]) -> []
  | ((l1,r1,c)::ll1 , []) -> (-.0.5+.x*.l1, -.0.5+.x*.r1, c*.x) :: comb(ll1,[])
  | ([] , (l2,r2,c)::ll2) -> (0.5+.x*.l2, 0.5+.x*.r2, c*.x) :: comb([],ll2)
  | ((l1,r1,c)::ll1 , (l2,r2,_)::ll2)
    -> (-.0.5+.x*.l1, 0.5+.x*.r2, c*.x) :: comb(ll1,ll2);;


let compute_coef_list t =
  fst (comp t)
  where rec comp = fun
    Empty -> [],[]
  | (Bin (Empty,_,Empty)) -> [1.0],[]
  | (Bin (t1,_,Empty))
    -> let (cl,trl) = comp t1 in
       (1.0::cl, (-.0.5,-.0.5,1.0)
                 ::map (fun (l,r,c) -> (-.0.5+.l, -.0.5+.r, c))
                     trl)
  | (Bin (Empty,_,t2))
    -> let (cl,trl) = comp t2 in
       (1.0::cl, (0.5,0.5,1.0)
                 ::map (fun (l,r,c) -> (0.5+.l, 0.5+.r, c))
                     trl)
  | (Bin (t1,_,t2))
    -> let (cl1,trl1) = comp t1
       and (cl2,trl2) = comp t2 in
       let cl = minl(cl1,cl2) in
       let trl1' = recompute_triples cl trl1
       and trl2' = recompute_triples cl trl2 in
       let x = compute_head_coef (trl1',trl2') in
       (1.0::x::tl cl, combine_triples x (trl1',trl2'));;

(* +compute_coef_list+ *)
(* +make_btree_picture+ *)

let make_btree_picture drn (vcoef,hcoef) color t =
  let tp = map_btree drn t
  and coef_list = compute_coef_list t in
  let (height,width) = compute_height_width (vcoef,hcoef) tp in
  let start_width = width /. (it_list mult_float 1.0 coef_list) in
  let tsty = {vdist=height; hdist=start_width;
              coef_list=coef_list;
              tlsty={linewidth= height/.50.0;
                     linecap=Buttcap;
                     linejoin=Beveljoin;dashpattern=[]};
              tcolor=color}
  in draw_btree tsty tp;;

(* +make_btree_picture+ *)



(* To use MLgraph drawing primitives *)
(*
let rec convert_btree = function
  Empty -> MLgraphNil
| Bin(Empty,a,Empty)
  -> MLgraphNode{mlgraphinfo=a;
                 mlgraphsons=[];
                 mlgraphlabel= Nolabel}
| Bin(t1,a,Empty)
  -> MLgraphNode{mlgraphinfo=a;
                 mlgraphsons=[convert_btree t1];
                 mlgraphlabel=Nolabel}
| Bin(Empty,a,t2)
  -> MLgraphNode{mlgraphinfo=a;
                 mlgraphsons=[convert_btree t2];
                 mlgraphlabel= Nolabel}
| Bin(t1,a,t2)
  -> MLgraphNode{mlgraphinfo=a;
                 mlgraphsons=[convert_btree t1;
                              convert_btree t2];
                 mlgraphlabel= Nolabel};;
*)

(* ancienne fonction draw_btree 

let draw_btree drn (h,d,cl,pt) = 
  let LS = {linewidth= h*.0.01;linecap=Buttcap;
            linejoin=Miterjoin;dashpattern=[]}
  in let rec draw_r (d,cl,({xc=x; yc=y} as pt)) =
    function 
       Empty -> failwith "Cannot draw an empty tree"
   | Bin(Empty,a,Empty)
      -> center_picture (drn a) pt
   | Bin(t1,a,t2)
      -> let d=d*.(hd cl)
         in let pt1 = {xc=x-.d/.2.0;yc=y-.h}
            and pt2 = {xc=x+.d/.2.0;yc=y-.h}
            in match (t1,t2) with
                 (_,Empty) -> group_pictures
		              [make_draw_picture
			            (LS,black)
                                    (make_sketch [Seg [pt;pt1]]);
                               center_picture (drn a) pt;
		 	       draw_r (d,tl cl,pt1) t1]
               | (Empty,_) ->  group_pictures
		              [make_draw_picture
			            (LS,black)
                                    (make_sketch [Seg [pt;pt2]]);
                               center_picture (drn a) pt;
			       draw_r (d,tl cl,pt2) t2]
               |   _     ->  group_pictures
		              [make_draw_picture
			            (LS,black)
                                    (make_sketch [Seg [pt;pt1]]);
                               make_draw_picture
			            (LS,black)
                                    (make_sketch [Seg [pt;pt2]]);
                               center_picture (drn a) pt;
			       draw_r (d,tl cl,pt1) t1;
			       draw_r (d,tl cl,pt2) t2]

	       
      in draw_r (d,cl,pt)
;;

*)
