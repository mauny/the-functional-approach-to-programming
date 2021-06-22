#use "topfind.camlp5" ;;
#camlp5o ;; (* for [<>] *)

#directory "../Util" ;;
#load_rec "lexxer.cmo" ;;
open Lexxer ;;


#load "ml_exp1.cmo" ;;
open Ml_exp1 ;;

#load "ml1_parser.cmo" ;;
open Ml1_parser ;;

#load "code_simulator.cmo" ;;
open Code_simulator ;;

#load "ml1_compiler.cmo" ;;
open Ml1_compiler ;;
