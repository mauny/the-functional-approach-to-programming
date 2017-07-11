#directory "../MLGRAPH.DIR";;
#open "MLgraph";;
#open "option";;
#open "graph";;
#open "prelude";;
#open "binary_trees";;
#open "binary_trees_parser";;
#open "binary_trees_drawing";;
#open "poly_tree";;


let draw_tree drn (h,d,cl,pt) = 
  let LS = {linewidth= h*.0.01;linecap=Buttcap;
            linejoin=Miterjoin;dashpattern=[]}
  in let rec draw_r (d,cl,({xc=x; yc=y} as pt)) =
    function 
     Leaf n -> center_picture (drn n) pt
   | Node(a1,a2)
      -> let d=d*.(hd cl)
         in let pt1 = {xc=x-.d/.2.0;yc=y-.h}
            and pt2 = {xc=x+.d/.2.0;yc=y-.h}
            in group_pictures
		              [make_draw_picture
			            (LS,black)
                                    (make_sketch [Seg [pt;pt1]]);
                               make_draw_picture
			            (LS,black)
                                    (make_sketch [Seg [pt;pt2]]);
			       draw_r (d,tl cl,pt1) a1;
			       draw_r (d,tl cl,pt2) a2]

	       
      in draw_r (d,cl,pt)
;;

let rec convert=
function Leaf _ -> Bin(Empty,1,Empty)
   | Node(a1,a2) -> Bin (convert a1,1,convert a2);;


let make_tree_picture drn (height,d_min,root) t =
 let t' =convert t
  in let coef_list = compute_coef_list t'
     in let total_coef = it_list mult_float 1.0 coef_list
        in let d= d_min/.total_coef
           in draw_tree drn (height,d,coef_list,root)  t;;

let draw_s_node r a =
 let t=make_text_picture (make_font Helvetica r) black a
 in let f= make_fill_picture (Nzfill,white)
               (frame_sketch (extend_frame Vertic_ext 0.5
                                 (picture_frame t)))
    in center_picture 
        (group_pictures [f;t])
	origin;;

let draw_i_node r n = draw_s_node r (string_of_int n);;

let t= Node(Leaf 3,Node(Leaf 4,Leaf 5))
in let p= make_tree_picture (draw_i_node 8.0)
                            (30.0,25.0,origin) t
   in eps_file p "../../PS/tree_ex";;


let FONT= make_font Courier 3.0;;

let g= assembleGraphs [] ["une";"deux"]
  [ [string "arrowDir" "F"; float "arrowPos" 0.65], "une" , De, "deux";
    [string "arrowDir" "F"; float "arrowPos" 0.968], "deux", Ds , "une"
  ];;

let make_string_node s = 
 let p= make_text_picture FONT black s
 in let fr= extend_frame All_ext 0.2 (picture_frame p)
    in group_pictures
        [make_fill_picture (Nzfill,white) (frame_sketch fr);p];;
           

let l= map (fun s -> s, (make_string_node ("\"" ^s^"\"") ))
           ["une";"deux"];;

let p= scale_picture (5.0,5.0) 
       (graphGen [float "lineWidthCoef" 0.5] g l);;

eps_file p "../../PS/boucle";;



let t= Node(Node(Node(Leaf "a",Leaf "b"),Node(Leaf "c",Leaf "d")),Leaf "e")
in let p= make_tree_picture (draw_s_node 10.0)
                            (30.0,25.0,origin) t
   in eps_file p "../../PS/tree_ex2";;
