#directory "../MLgraph.lib" ;;
#load "mlgraph.cma" ;;
open Mlgraph ;;

#directory "../Util" ;;
#load_rec "prelud.cmo" ;;
open Prelud

#load_rec "lexxer.cmo" ;;
open Lexxer ;;

#directory "../Arbres" ;;
#load_rec "binary_trees.cmo" ;;
open Binary_trees ;;

#load_rec "binary_trees_parser.cmo" ;;
open Binary_trees_parser ;;

#load_rec "binary_trees_drawing.cmo" ;;
open Binary_trees_drawing ;;


#load "permutations.cmo" ;;
open Permutations ;;

#load "complexs.cmo" ;;
open Complexs ;;

#load "pavages1.cmo" ;;
open Pavages1 ;;

#load "pavages2.cmo" ;;
open Pavages2 ;;
