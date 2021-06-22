(*
#use "topfind" ;;
#camlp4o ;; 
*)

#use "topfind.camlp5" ;;
#camlp5o ;; (* for [<>] *)

#directory "../Util";;
#directory "../Arbres";;

#load_rec "prelud.cmo" ;;
open Prelud ;;


#load "astexpr.cmo" ;;
open Astexpr ;;

(* Recognition *)
#load "recognize.cmo" ;;
open Recognize ;;

(* Analysis *)
#load "parse_prelude.cmo" ;;
open Parse_prelude ;;

#load "recognize_val.cmo" ;;
open Recognize_val ;;

(* Stream parsers *)
#load "predict.cmo" ;;
open Predict ;;

(* Regular expression compilation *)

#load_rec "orders.cmo" ;;
open Orders ;;

#load_rec "binary_trees.cmo" ;;
open Binary_trees ;;

#load_rec "sets.cmo" ;;
open Sets ;;

#load "comp_re.cmo" ;;
open Comp_re ;;
