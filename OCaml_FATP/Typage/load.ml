(*
#use "topfind" ;;
#camlp4o ;;
*)

#use "topfind.camlp5" ;;
#camlp5o ;; (* for [<>] *)


#directory "../Util" ;;
#load_rec "prelud.cmo" ;;
open Prelud ;;

#load_rec "lexxer.cmo" ;;
open Lexxer ;;

#directory "../Termes" ;;
#load_rec "termes.cmo" ;;
open Termes ;;

#load_rec "type_synthesis.cmo" ;;
open Type_synthesis ;;


#directory "../Compil" ;;
#load_rec "ml_exp1.cmo" ;;
open Ml_exp1 ;;

#load_rec "ml1_parser.cmo" ;;
open Ml1_parser ;;


#load "ml_type.cmo" ;;
open Ml_type ;;

#load "type_synth.cmo" ;;
open Type_synth ;;

#load "type_synth_destr.cmo" ;;
open Type_synth_destr ;;
