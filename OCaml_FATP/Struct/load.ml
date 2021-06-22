(*
#use "topfind" ;;
#camlp4o ;; 
*)

#use "topfind.camlp5" ;;
#camlp5o ;; (* for [<>] *)


#directory "../MLgraph.lib" ;;
#load "mlgraph.cma" ;;
open Mlgraph ;;

#directory "../Util" ;;
#load_rec "prelud.cmo" ;;
#load_rec "lexxer.cmo" ;;

#directory "../Arbres" ;;
#load_rec "binary_trees.cmo" ;;
#load_rec "binary_trees_parser.cmo" ;;
#load_rec "binary_trees_drawing.cmo" ;;

(* #use "defs_struct.ml" ;; *)

#load "inttree.cmo" ;;
open Inttree ;;

#load "exp.cmo" ;;
open Exp ;;

#load "poly_tree.cmo" ;;
open Poly_tree ;;
