#use "topfind.camlp5" ;;
#camlp5o ;; (* for [<>] *)

#directory "../Util" ;;
#load_rec "prelud.cmo" ;;
open Prelud ;;

#load_rec "orders.cmo" ;;
open Orders ;;


#load "arith_list_nat.cmo" ;;
open Arith_list_nat ;;

#load "arith_circ_list_nat.cmo" ;;
open Arith_circ_list_nat ;;

#load "arith_big_int.cmo" ;;
open Arith_big_int ;;

#load "arith_rat.cmo" ;;
open Arith_rat ;;

#load "arith_pi.cmo" ;;
open Arith_pi ;;
