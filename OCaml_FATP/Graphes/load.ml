#use "topfind.camlp5" ;;
#camlp5o ;; (* for [<>] *)

#directory "../MLgraph.lib" ;;
#load "mlgraph.cma" ;;
open Mlgraph ;;


#directory "../Util" ;;
#directory "../Arbres" ;;

#load_rec "prelud.cmo" ;;
open Prelud ;;

#load_rec "orders.cmo" ;;
open Orders ;;

#load_rec "binary_trees.cmo" ;;
open Binary_trees ;;

#load_rec "sets.cmo" ;;
open Sets ;;


#load "games.cmo" ;;
open Games ;;

#load "games_ane_rouge.cmo" ;;
open Games_ane_rouge ;;

#load "games_solit.cmo" ;;
open Games_solit ;;
