
#directory "../MLGRAPH.DIR";;
#open "MLgraph";;
#directory "../Util";;
#open "graph";;
#open "prelude";;
#open "orders";;
#infix "o";;
#open "binary_trees";;
#open "binary_trees_drawing";;
#open "lexer";;
#open "binary_trees_parser";;
(* #open "binary_trees_images";; *)



(* +dates+ *)

let draw_oval_int_node r n =
  let s= center_picture 
           (make_text_picture (make_font Helvetica r) black (string_of_int n))
	   origin
  and f= ovalOfFrame [] {xmin= -.2.0*.r;xmax=2.0*.r;ymin= -.r/.2.0;ymax=r/.2.0}
  in group_pictures [f;s];;



let t = parse_btree parse_int
       
"1848(1834(1824(1819,1830),1840(1839,1841)),1869(1867(1863,1868),1882(1880,1892)))"

in let p = make_btree_picture 
                           (draw_oval_int_node 8.0)
                           (2.0,1.2) black t
   in eps_file p "../../PS/dates";;

(* +dates+ *)


(* +peintres+ *)

let draw_oval_string_node r a =
  let s= center_picture 
           (make_text_picture (make_font Helvetica r) black a)
	   origin
  and f= ovalOfFrame [] {xmin= -.2.0*.r;xmax=2.0*.r;ymin= -.r/.2.0;ymax=r/.2.0}
  in group_pictures [f;s];;



let t = parse_btree parse_ident
       
"Matisse(Degas(Boudin(Bonnard,Braque),Gauguin(Derain,Jongkind)),Seurat(Pissaro(Monet,Redon),Sisley(Signac,Vuillard)))"

in let p = make_btree_picture 
                           (draw_oval_string_node 8.0)
                           (2.0,1.2) black t
   in eps_file p "../../PS/peintres";;

(* +peintres+ *)


(* +peintres2+ *)
let draw_oval_string_int_node r (n,d) =
 let t1=make_text_picture (make_font Helvetica r) black n
 and t2=make_text_picture (make_font Helvetica r) black
                          (string_of_int d)
 in let t= center_picture (align_vertically Align_Center [t1;t2])
                          origin
    and f= ovalOfFrame [] {xmin= -.2.0*.r;xmax=2.0*.r;ymin= -.r/.2.0;ymax=r/.2.0}
    in  group_pictures [f;t];;


let t = parse_btree parse_string_int
       
"[Matisse;1869]([Degas;1834]([Boudin;1824]([Bonnard;1867],[Braque;1882]),[Gauguin;1848]([Derain;1880],[Jongkind;1819])),[Seurat;1863]([Pissaro;1830]([Monet;1840],[Redon;1840]),[Sisley;1839]([Signac;1863],[Vuillard;1868])))"

in let p = make_btree_picture 
                           (draw_oval_string_int_node 8.0)
                           (2.0,1.2) black t

   in eps_file p "../../PS/peintres2";;

(* +peintres2+ *)

(* +BTREES+ *)
let t1 = parse_btree parse_int "5(7,())"
and t2 = parse_btree parse_int "5((),3)"
and t3 = parse_btree parse_int "5(7,3)"
and txt1= make_textblock_picture  Align_Left 7.0
           (make_font Courier 10.0) black 
           ["Bin(Bin(Empty,7,Empty)";
            "    ,5,Empty)"]
             
and txt2= make_textblock_picture  Align_Left 7.0 
           (make_font Courier 10.0) black 
           ["Bin(Empty,5,";
            "    Bin(Empty,3,Empty))"]
             
and txt3= make_textblock_picture  Align_Left 7.0 
           (make_font Courier 10.0) black
           ["Bin(Bin(Empty,7,Empty),5,";
            "    Bin(Empty,3,Empty))"]
             

 in let p1= align_horizontally Align_Center 
             [extend_picture_frame  Right_ext 0.5 txt1;
              make_btree_picture 
                           (draw_int_node 8.0)
                           (3.0,2.0) black t1]
    and p2= align_horizontally Align_Center 
             [extend_picture_frame  Right_ext 0.5 txt2;
              make_btree_picture 
                           (draw_int_node 8.0)
                           (3.0,2.0) black t2]
    and p3= align_horizontally Align_Center 
             [extend_picture_frame  Right_ext 0.5 txt3;
              make_btree_picture 
                           (draw_int_node 8.0)
                           (3.0,2.0) black t3]
    in let p= align_vertically Align_Left
              [extend_picture_frame Bottom_ext 0.2 p1;
               extend_picture_frame Bottom_ext 0.2 p2;
               p3]
       in  eps_file p "../../PS/BTREES";;

(* +BTREES+ *)


(* +parsebtree+ *)
let t = parse_btree parse_ident
"matisse(degas(boudin(bonnard,braque),gauguin(derain,jongkind)),seurat(pissaro(monet,redon),sisley(signac,vuillard)))"

and txt= make_textblock_picture  Align_Left 7.0
           (make_font Courier 10.0) black 
           ["parse_btree parse_ident";
            "    (Matisse";
            "      (Degas";
            "         (Boudin";
            "            (Bonnard,Braque)";
            "         ,Gauguin";
            "            (Derain,Jongkind))";
            "       ,Seurat";
            "          (Pissaro";
            "             (Monet,Redon)";
            "           ,Sisley";
            "             (Signac,Vuillard))))"]
in  let p = compose_horizontally
                [extend_picture_frame  Right_ext 0.2 txt; 
                 make_btree_picture 
                           (draw_oval_string_node 8.0)
                           (3.0,1.2) black t]
    in  eps_file p "../../PS/parsebtree";;

(* +parsebtree+ *)

(* +BST1+ *)
let t1 = parse_btree parse_int "2(1,20(10(6(4,8),15(12,17)),21))"
and t2 = parse_btree parse_int "10(4(1((),2),6((),8)),15(12,20(17,21)))"
 in let p1= make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black t1
    and p2= make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black t2
    and pb= make_blank_picture (50.0,50.0)
    in  eps_file (align_horizontally Align_Center [p1;pb;p2])
       "../../PS/BST1";;

(* +BST1+ *)

(* +BSTconstr+ *)
let a1 = Bin(Empty,10,Empty) in
let a2= add_bottom_to_bst (fun x y -> x) int_comp a1 15 in
let a3= add_bottom_to_bst (fun x y -> x) int_comp a2 12 in
let a4= add_bottom_to_bst (fun x y -> x) int_comp a3 4 in
let a5= add_bottom_to_bst (fun x y -> x) int_comp a4 6 in
let a6= add_bottom_to_bst (fun x y -> x) int_comp a5 21 in
let a7= add_bottom_to_bst (fun x y -> x) int_comp a6 8 in
let a8= add_bottom_to_bst (fun x y -> x) int_comp a7 1 in
let a9= add_bottom_to_bst (fun x y -> x) int_comp a8 17 in
let a10= add_bottom_to_bst (fun x y -> x) int_comp a9  2 in
let p=
  align_vertically Align_Left
       [align_horizontally Align_Top
           (map (extend_picture_frame Left_ext 0.4
                 o (make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black ))
                [a1;a2;a3;a4;a5;a6;a7]);
        align_horizontally Align_Top
           (map (extend_picture_frame Left_ext 0.4
                 o (make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black ))
                [a8;a9;a10])]
   in eps_file p "../../PS/BSTconstr";;

(* +BSTconstr+ *)

(* +cut+ *)
let make_arrow_picture l l' w w' color=
  let p= make_fill_picture
             (Nzfill,color)
             (make_sketch [Seg[origin;{xc=0.;yc=w};
                               {xc=l;yc=w};{xc=l;yc=w'};
                               {xc=l+.l';yc=0.};{xc=l;yc= -.w'};
                               {xc=l;yc= -.w};{xc=0.;yc= -.w};
                               origin]])
  in extend_picture_frame Horiz_ext 0.5 p;;
let t = parse_btree parse_int "2(1,20(10(6(4,8),15(12,17)),21))"
in let t1,x,t2=cut_bst int_comp 11 t
 in let p= make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black t
    and p1= make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,3.6) black t1
    and p2= make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,3.6) black t2
    and pb= make_blank_picture (50.0,50.0)
    and arrow= make_arrow_picture 50. 10. 1. 3. black
    in  eps_file (align_horizontally Align_Center [p;arrow;p1;pb;p2])
       "../../PS/cut";;

(* +cut+ *)


(* +BSTconstr2+ *)
let int_comp= mk_preorder (prefix <,prefix =);;
let a1 = Bin(Empty,10,Empty)
in let a2= add_root_to_bst (fun x y -> x) int_comp 15 a1 
   in let a3= add_root_to_bst (fun x y -> x) int_comp 12 a2
      in let a4= add_root_to_bst (fun x y -> x) int_comp 4 a3
         in let a5= add_root_to_bst (fun x y -> x) int_comp 6 a4
            in let a6= add_root_to_bst (fun x y -> x) int_comp 21 a5
               in let a7= add_root_to_bst (fun x y -> x) int_comp 8 a6
                  in let a8= add_root_to_bst (fun x y -> x) int_comp 1 a7
                     in let a9= add_root_to_bst (fun x y -> x) int_comp 17 
a8
                       in let a10= add_root_to_bst (fun x y -> x) int_comp 
2 a9 
in let p= 
  align_vertically Align_Left
       [align_horizontally Align_Top
           (map (extend_picture_frame Left_ext 0.4
                 o (make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black ))
                [a1;a2;a3;a4;a5;a6;a7]);
        align_horizontally Align_Top
           (map (extend_picture_frame Left_ext 0.4
                 o (make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black))
                [a8;a9;a10])]
   in eps_file p "../../PS/BSTconstr2";;

(* +BSTconstr2+ *)


(* +BSTrem+ *)
let a1 = mk_bst (fun x y -> x) int_comp [10;15;12;4;6;21;3]
in let a2= rem_from_bst  int_comp 10 a1 
   in let a3= rem_from_bst int_comp 15 a2 
      in let a4= rem_from_bst int_comp 6 a3
in let p= 
  align_horizontally Align_Top
           (map (extend_picture_frame Left_ext 0.4
                 o (make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black ))
                [a1;a2;a3;a4])
   in eps_file p "../../PS/BSTrem";;

(* +BSTrem+ *)

(* +rotation_rd_rg+ *)

let rec convert' = function
  Bin(Empty,a,Empty)
    -> Bin(Empty,draw_text_node 8.0 a,Empty)
| Bin(t1,a,t2)
    -> Bin(convert' t1,draw_string_node 8.0 a,convert' t2)
| Empty -> Empty;;

let t1= convert' (parse_btree parse_ident "q(p(u,v),w)")
and t2= convert' (parse_btree parse_ident "p(u,q(v,w))") in
   let p1= make_btree_picture
             (fun x -> x)
             (2.2,1.8) black t1
   and p2= make_btree_picture 
             (fun x -> x)
             (2.2,1.8) black t2
   and arrow= make_arrow_picture 50. 10. 1. 3. black
    in  eps_file (align_horizontally Align_Center [p1;arrow;p2])
                 "../../PS/rotation_rd";
        eps_file (align_horizontally Align_Center [p2;arrow;p1])
                 "../../PS/rotation_rg";;

(* +rotation_rd_rg+ *)


(* +rotation_rgd+ *)
let t1= convert' (parse_btree parse_ident "r(p(t,q(u,v)),w)")
and t2= convert' (parse_btree parse_ident "q(p(t,u),r(v,w))")
in let p1= make_btree_picture 
             (fun x -> x)
             (2.,2.) black t1
   and p2= make_btree_picture
             (fun x -> x)
             (2.2,1.8) black t2
   and arrow= make_arrow_picture 50. 10. 1. 3. black
    in  eps_file (align_horizontally Align_Center [p1;arrow;p2])
                 "../../PS/rotation_rgd";;
(* +rotation_rgd+ *)

(* +rotation_rdg+ *)
let t1= convert' (parse_btree parse_ident "p(t,r(q(u,v),w))")
and t2= convert' (parse_btree parse_ident "q(p(t,u),r(v,w))")
in let p1= make_btree_picture
             (fun x -> x)
             (2.,2.) black t1
   and p2= make_btree_picture
             (fun x -> x)
             (2.2,1.8) black t2
   and arrow= make_arrow_picture 50. 10. 1. 3. black
    in  eps_file (align_horizontally Align_Center [p1;arrow;p2])
                 "../../PS/rotation_rdg";;
(* +rotation_rdg+ *)


(* +comp_eq+ *)
let t1 = parse_btree parse_int "5(3(2,4),7(6,()))"
and t2 = parse_btree parse_int "4(2(1,3),5(6,7))"
 in let p1= make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black t1
    and p2= make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black t2
    in  eps_file p1 "../../PS/comp_eq1"; eps_file p2 "../../PS/comp_eq2";
        eps_file  (align_horizontally Align_Center
                     [p1;make_blank_picture (50.,50.);p2])
                "../../PS/comp_eq";;

(* +comp_eq+ *)


(* +degen+ *)
let t = mk_bst (fun x y -> x) int_comp [10;7;2;5;3]
in let p= make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black t
   in eps_file p "../../PS/degen";;

(* +degen+ *)

(* +add_avl+ *)
let t1= parse_btree parse_int  "10(7(2,()),())"
and t2= parse_btree parse_int  "7(2,10)"
and t3= parse_btree parse_int  "7(2((),5(3,())),10)"
and t4= parse_btree parse_int  "7(3(2,5),10)"
 in let p1= make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black t1
    and p2= make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black t2
    and p3= make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black t3
    and p4= make_btree_picture 
                           (draw_int_node 8.0)
                           (2.2,1.8) black t4
    and pb= make_blank_picture (50.0,50.0)
    and arrow= make_arrow_picture 50. 10. 1. 3. black
    in  eps_file (align_horizontally Align_Center [p1;arrow;p2]) "../../PS/add_avl1"
       ;eps_file (align_horizontally Align_Center [p3;arrow;p4]) "../../PS/add_avl2" ;;

(* +add_avl+ *)

(* +AVL1+ *)
let int_comp= mk_preorder(prefix <,prefix =);;
let draw_avl_int_node r (n,b) =
  let p = draw_int_node r n
  in match b with
        Balanced -> p
     |  Left -> group_pictures
                  [p; (make_fill_picture
                            (Nzfill,black)
                            (make_sketch [Arc({xc= -.1.0*.r;yc=0.0},
                                              r/.3.0,0.0,360.0)]))]
     |  Right -> group_pictures
                  [p; (make_fill_picture
                            (Nzfill,Gra 0.0)
                            (make_sketch [Arc({xc=1.0*.r;yc=0.0},
                                              r/.3.0,0.0,360.0)]))];;

let t = mk_avl (fun x y -> x) int_comp [2;1;20;11;21;6;15;4;8;12;17;3;5;7;9]
in let p = make_btree_picture 
                           (draw_avl_int_node 8.0)
                           (2.2,1.8) black t
   in  eps_file p "../../PS/AVL1";;

(* +AVL1+ *)
(* +AVLconstr+ *)
let a1 = Bin(Empty,(10,Balanced),Empty);;
let a2= add_to_avl (fun x y -> x) int_comp a1 15;;
let a3= add_to_avl (fun x y -> x) int_comp a2 12;;
let a4= add_to_avl (fun x y -> x) int_comp a3 4;;
let a5= add_to_avl (fun x y -> x) int_comp a4 6;;
let a6= add_to_avl (fun x y -> x) int_comp a5 21;;
let a7= add_to_avl (fun x y -> x) int_comp a6 8;;
let a8= add_to_avl (fun x y -> x) int_comp a7 1;;
let a9= add_to_avl (fun x y -> x) int_comp a8 17;;
let a10= add_to_avl (fun x y -> x) int_comp a9 2;;

let draw_avl_int_node s (n,_) = draw_int_node s n;;
let p= 
  align_vertically Align_Left
       [align_horizontally Align_Top
           (map (extend_picture_frame Left_ext 0.4
                 o (make_btree_picture 
                           (draw_avl_int_node 8.0)
                           (2.2,1.8) black))
                [a1;a2;a3;a4;a5;a6;a7]);
        align_horizontally Align_Top
           (map (extend_picture_frame Left_ext 0.2
                 o (make_btree_picture 
                           (draw_avl_int_node 8.0)
                           (2.2,1.8) black))
                [a8;a9;a10])]
   in eps_file (center_picture p {xc=300.; yc=500.} )"../../PS/AVLconstr";;

(* +AVLconstr+ *)


(* +AVLrem+ *)
let b1=a10;;
let b2= remove_from_avl int_comp b1 21;;
let b3= remove_from_avl int_comp b2 17;;
let b4= remove_from_avl int_comp b3 2;;
let b5= remove_from_avl int_comp b4 4;;
let b6= remove_from_avl int_comp b5 15;;
let b7= remove_from_avl int_comp b6 12;;
let b8= remove_from_avl int_comp b7 8;;
let b9= remove_from_avl int_comp b8 10;;
let b10= remove_from_avl int_comp b9 6;;

let p= 
  align_vertically Align_Left
       [align_horizontally Align_Top
           (map (extend_picture_frame Left_ext 0.2
                 o (make_btree_picture 
                           (draw_avl_int_node 8.0)
                           (2.2,1.8) black))
                [b1;b2;b3]);
        extend_picture_frame Top_ext 0.2
         (align_horizontally Align_Top
           (map (extend_picture_frame Left_ext 0.4
                 o (make_btree_picture 
                           (draw_avl_int_node 8.0)
                           (2.2,1.8) black ))
                [b4;b5;b6;b7;b8;b9;b10]))]
   in eps_file (center_picture p {xc=300.; yc=500.} )"../../PS/AVLrem";;


(* +AVLrem+ *)
