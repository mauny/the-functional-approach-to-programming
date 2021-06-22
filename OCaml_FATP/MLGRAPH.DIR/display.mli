(* $Id: display.mlip,v 1.1 1997/08/14 11:34:50 emmanuel Exp $ *)



open  Frames ;;
open  Pictures ;;


type page_defaults =
  {mutable page_height: float;
   mutable page_width: float;
   mutable page_hmargin: float;
   mutable page_vmargin: float};;


val page_height : unit -> float;;
val page_width : unit -> float;;
val page_hmargin : unit -> float;;
val page_vmargin : unit -> float;;
val set_page_height : float -> unit;;
val set_page_width : float -> unit;;
val set_page_hmargin : float -> unit;;
val set_page_vmargin : float -> unit;;
val compute_display_frame : picture -> frame;;
val center_and_adjust_picture : picture -> picture;;
val center_and_adjust : picture -> picture;;
(*val make_ps_file : picture -> string -> bool -> unit;;*)
val set_frame_extension_coef : float -> unit;;
val ps_file : picture -> string -> unit;;
val eps_file : picture -> string -> unit;;
val ps : picture ->  unit;;
val eps : picture  -> unit;;
