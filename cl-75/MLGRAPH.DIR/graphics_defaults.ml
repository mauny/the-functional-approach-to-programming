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


(* $Id: graphics_defaults.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* graphics_defaults.ml                                                  *)
(*                        Emmanuel Chailloux & Guy Cousineau             *)
(*                        Tue Jan 21 1992                                *)




#open "MLgraph";;

#open "compatibility";;
#open "prelude";;
#open "paint";;
#open "frames";;




type graphic_state =
  {mutable gs_color : color;
   mutable gs_linewidthcoef: float;
   mutable gs_linecap:linecap;
   mutable gs_linejoin:linejoin;
   mutable gs_dashpattern:int list;
   mutable gs_closed_sketch: bool;
   mutable gs_fillstyle : fillstyle;
   mutable gs_miterlimit:float};;



let default_graphic_state =
    {gs_linewidthcoef = 0.01;
     gs_linecap = Squarecap;
     gs_linejoin = Miterjoin;
     gs_dashpattern=[];
     gs_closed_sketch=false;
     gs_fillstyle = Eofill;
     gs_color = Gra 0.0;
     gs_miterlimit=1.0};;

let default_linewidthcoef () =
  default_graphic_state.gs_linewidthcoef;;

let default_linecap () =
  default_graphic_state.gs_linecap;;

let default_linejoin () =
  default_graphic_state.gs_linejoin;;

let default_dashpattern () =
  default_graphic_state.gs_dashpattern;;

let default_color () =
  default_graphic_state.gs_color;;
   
let default_closed_sketch () = 
  default_graphic_state.gs_closed_sketch;;

let default_fillstyle () =
  default_graphic_state.gs_fillstyle;;
   
let default_miterlimit () =
  default_graphic_state.gs_miterlimit;;

let default_linestyle fr =
  let dgs=default_graphic_state
  in
   {linewidth= (let av_dim = 
                      (fr.xmax-.fr.xmin)+.(fr.ymax-.fr.ymin)/.2.0
                 in av_dim *. default_linewidthcoef ());
    linecap=dgs.gs_linecap;
    linejoin=dgs.gs_linejoin;
    dashpattern=dgs.gs_dashpattern};;
    


let set_default_linewidthcoef c =
   default_graphic_state.gs_linewidthcoef <- c;;

let set_default_color c =
   default_graphic_state.gs_color <- c;;

let set_default_closed_sketch b =
   default_graphic_state.gs_closed_sketch <- b;;

let set_default_fillstyle fsty =
   default_graphic_state.gs_fillstyle <- fsty;;

let set_default_linecap cap =
   default_graphic_state.gs_linecap <- cap;;

let set_default_linejoin join =
   default_graphic_state.gs_linejoin <- join;;
   
let set_default_dashpattern join =
   default_graphic_state.gs_dashpattern <- join;;

let set_default_miterlimit ml =
   default_graphic_state.gs_miterlimit <- ml;;


