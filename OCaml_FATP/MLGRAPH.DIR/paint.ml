(*************************************************************************)
(*                                                                       *)
(*                     Projet      Formel                                *)
(*                                                                       *)
(*                  Objective CAML: MLgraph library                      *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            LIENS                                      *)
(*                        45 rue d'Ulm                                   *)
(*                         75005 PARIS                                   *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)

(* $Id: paint.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)

(* paint.ml        painting  types                                       *)
(*                 Emmanuel Chailloux & Guy Cousineau                    *)
(*                 Mon Jan 20 1992                                       *)






type color = Rgb of float * float * float
           | Hsb of float * float * float
           | Gra of float;;

type linecap = Buttcap | Squarecap | Roundcap;;

type linejoin = Beveljoin | Roundjoin | Miterjoin;;

type linestyle = {linewidth:float;
                  linecap:linecap;
                  linejoin:linejoin;
                  dashpattern:int list};;

type fillstyle = Nzfill | Eofill;;
type clipstyle  = Nzclip | Eoclip;;


let black= Gra 0.0
and white = Gra 1.0
and red = Rgb (1.,0.,0.)
and green = Rgb (0.,1.,0.)
and blue = Rgb (0.,0.,1.)
and yellow = Rgb (1.,1.,0.)
and cyan = Rgb (0.,1.,1.)
and magenta = Rgb (1.,0.,1.);;


