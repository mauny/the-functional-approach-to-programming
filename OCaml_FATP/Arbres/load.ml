(* Pour Unix
directory_concat_string:="/";;
change_graphics_directory "../MLgraph.lib";;
*)

#use "topfind.camlp5" ;;
#camlp5o ;; (* for [<>] *)


#directory "../MLgraph.lib" ;;
#load "mlgraph.cma" ;;
open Mlgraph ;;

#directory "../Util" ;;
#load_rec "prelud.cmo" ;;
open Prelud ;;

#load_rec "orders.cmo" ;;
open Orders ;;

#load_rec "lexxer.cmo" ;;
open Lexxer ;;


#load "binary_trees.cmo" ;;
open Binary_trees ;;

#load "dictionnaries.cmo" ;;
open Dictionnaries ;;

#load "sets.cmo" ;;
open Sets ;;

#load "binary_trees_parser.cmo" ;;
open Binary_trees_parser ;;

#load "binary_trees_drawing.cmo" ;;
open Binary_trees_drawing ;;
