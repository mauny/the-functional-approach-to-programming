(* #use "load.ml" ;; *)

open Mlgraph
(* open Tree *)
open Binary_trees
open Binary_trees_drawing
open Prelud
open Gentree
(* open Gentree_images *)

let rec convert drn =
  function GenNode (x,l)
    -> Node  {info= drn x;
              sons= map (convert drn) l;
              label= Nolabel} ;;
(*
let rec convert drn = function
    GenNode (x, l)
    -> MLgraphNode  { mlgraphinfo = drn x;
                      mlgraphsons = List.map (convert drn) l;
                      mlgraphlabel = Nolabel } ;;
*)

let t1 = parse_gentree parse_string "Vn(U)"
and t2 = parse_gentree parse_string "Vn(Vm(A(I,I)))" ;;

let p1 = tree (convert (draw_string_node 10.0) t1)
and p2 = tree (convert (draw_string_node 10.0) t2) ;;

let p =
  align_horizontally Align_Top [p1; make_blank_picture (80.0, 80.0); p2] ;;

eps_file p "../PS/new_tyvar" ;;

exit 0 ;;
