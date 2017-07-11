#directory "../MLGRAPH.DIR";;
#open "MLgraph";;
#open "tree";;
#open "gentree";;


(*
let rec convert drn =
  function GenNode (x,l)
    -> Node  {info= drn x;
              sons= map (convert drn) l;
              label= Nolabel};;
*)

let rec convert drn =
  function GenNode (x,l)
    -> MLgraphNode  {mlgraphinfo= drn x;
                     mlgraphsons= map (convert drn) l;
                     mlgraphlabel= Nolabel};;


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
                  
let t= gentree_of_string string_of_ident "a(b,c(d,e),f)";;
let p= tree (convert (draw_string_node 10.0) t);;
let pp= translate_picture (300.0,800.0) p;;
eps_file pp "../../PS/gentree";;
