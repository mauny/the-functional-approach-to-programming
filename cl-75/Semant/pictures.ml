#directory "../MLGRAPH.DIR";;
#open "MLgraph";;
#open "option";;
#open "graph";;
#open "prelude";;


(* +confluence1+ *)
let confluence1_graph =
assembleGraphs [] ["e";"e1";"e2";"e3"]
  [ [string "arrowDir" "F"], "e" , Dsw, "e1";
    [string "arrowDir" "F"], "e" , Dse ,"e2";
    [string "arrowDir" "F"], "e1" , Dse ,"e3";
    [string "arrowDir" "F"], "e2" , Dsw ,"e3"
  ]
;;

let (t,t1,t2,t3) =
  match
    map (fun s ->
                 group_pictures
                   [make_fill_picture (Nzfill,white)
                      (make_sketch [Arc (origin,10.0,0.0,360.0)]);
                    center_picture
                      (make_text_picture (make_font Helvetica 12.0) black s)
                      origin])
      ["e";"e1";"e2";"e3"]
  with [t;t1;t2;t3] -> (t,t1,t2,t3)
     | _ -> failwith "picture error"
;;


let confluence1_pict=
  graphGen [float "lineWidthCoef" 1.0] 
           confluence1_graph
           (combine (["e";"e1";"e2";"e3"],[t;t1;t2;t3] ))
in eps_file confluence1_pict "../../PS/confluence1_pict";;

(* +confluence1+ *)

(* +confluence2+ *)
let confluence2_graph = 
assembleGraphs [] ["e";"e1";"e2";"e3";"e4"]
  [ [string "arrowDir" "F"], "e" , Dsw, "e1";
    [string "arrowDir" "F"], "e" , Dse ,"e2";
    [string "arrowDir" "F"], "e1" , Dse ,"e3";
    [string "arrowDir" "F"], "e2" , Dsw ,"e3";
    [string "arrowDir" "F"], "e3" , Ds ,"e4"
  ];;

let (t,t1,t2,t3,t4) =
  match
    map (fun s -> group_pictures
                    [make_fill_picture (Nzfill,white)
                       (make_sketch [Arc (origin,12.0,0.0,360.0)]);
                     center_picture
                       (make_text_picture (make_font Helvetica 12.0) black s)
                       origin])
      ["(2+3)*(4+5)";
       "5*(4+5)";
       "(2+3)*9";
       "5*9";
       "45"]
  with [t;t1;t2;t3;t4] -> (t,t1,t2,t3,t4)
     | _ -> failwith "picture error"
;;

let confluence2_pict=
  graphGen [float "lineWidthCoef" 0.5] 
           confluence2_graph
           (combine (["e";"e1";"e2";"e3";"e4"],[t;t1;t2;t3;t4] ))
in eps_file confluence2_pict "../../PS/confluence2_pict";;

(* +confluence2+ *)



(* +graph_reec1+ *)
let reec1_graph = 
assembleGraphs [] ["e";"e1";"e2";"e3";"e4";"e5";"e6"]
  [ [string "arrowDir" "F"], "e" , Dsw, "e1";
    [string "arrowDir" "F"], "e" , Dse ,"e2";
    [string "arrowDir" "F"], "e1" , Dsw ,"e3";
    [string "arrowDir" "F"], "e1" , Dse ,"e4";
    [string "arrowDir" "F"], "e3" , Dse ,"e5";
    [string "arrowDir" "F"], "e4" , Dsw ,"e5";
    [string "arrowDir" "F"], "e5" , Ds ,"e6";
    [string "arrowDir" "F"], "e2" , Ds ,"e5"
  ];;

let (t,t1,t2,t3,t4,t5,t6) =
  match
    map (fun s ->
                 group_pictures
                   [make_fill_picture (Nzfill,white)
                      (make_sketch [Arc (origin,12.0,0.0,360.0)]);
                    center_picture
                      (make_text_picture (make_font Helvetica 12.0) black s)
                      origin])
      ["(fun x->x*x)(2+3)";
       "(2+3)*(2+3)";
       "(fun x->x*x) 5";
       "5*(2+3)";
       "(2+3)*5";
       "5*5";
       "25"]
  with [t;t1;t2;t3;t4;t5;t6] -> (t,t1,t2,t3,t4,t5,t6)
     | _ -> failwith "picture error"
;;

let graph_reec1_pict =
  graphGen [float "lineWidthCoef" 0.5]
    reec1_graph
    (combine (["e";"e1";"e2";"e3";"e4";"e5";"e6"],[t;t1;t2;t3;t4;t5;t6] ))
in eps_file graph_reec1_pict "../../PS/graph_reec1_pict";;

(* +graph_reec1+ *)
