(*                                                                       *)
(*                     Projet      Formel                                *)
(*                                                                       *)
(* CAML-light: MLgraph library *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            LIENS                                      *)
(*                        45 rue d'Ulm                                   *)
(*                         75005 PARIS                                   *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)

(* $Id: compatibility.mlp,v 1.3 1997/08/14 14:38:52 emmanuel Exp $ *)
(*  compatibility  with Objective Caml                                   *)
(*                    Emmanuel Chailloux & Guy Cousineau                 *)
(*                    Fri Feb 16  1996                                   *)




let space_char = ` `;;
let lf_char = `\010`;;
let char_0 = `0`;;
let comma_char = `,`;;
let open_par_char = `(`;;
let close_par_char = `)`;;
let ascii_0 = int_of_char `0`
and ascii_9 = int_of_char `9`
and ascii_a = int_of_char `a`
and ascii_f = int_of_char `f`
and ascii_A = int_of_char `A`
and ascii_F = int_of_char `F`
;;
let append l1 l2 = l1 @ l2;;
let lnot x =  x lxor x;;
let format_float = float__format_float;;
let combine x y = combine (x,y);;







