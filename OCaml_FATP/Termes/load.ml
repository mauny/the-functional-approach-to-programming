#use "topfind.camlp5" ;;
#camlp5o ;; (* for [<>] *)

#directory "../MLgraph.lib" ;;
#load "mlgraph.cma" ;;


#directory "../Util" ;;
#load_rec "prelud.cmo" ;;
open Prelud ;;

#load_rec "lexxer.cmo" ;;
open Lexxer ;;

#directory "../Eval" ;;
#load_rec "ml_strict.cmo" ;;
open Ml_strict ;;

#load_rec "parser_strict.cmo" ;;
open Parser_strict ;;

#load_rec "ml_ops.cmo" ;;
open Ml_ops ;;


#load "gentree.cmo" ;;
open Gentree ;;

#load "termes.cmo" ;;
open Termes ;;

#load "type_synthesis.cmo" ;;
open Type_synthesis ;;
