

#open "MLgraph";;
#open "pictures";;


type page_defaults =
  {mutable page_height: float;
   mutable page_width: float;
   mutable page_hmargin: float;
   mutable page_vmargin: float};;


value page_height : unit -> float;;
value page_width : unit -> float;;
value page_hmargin : unit -> float;;
value page_vmargin : unit -> float;;
value set_page_height : float -> unit;;
value set_page_width : float -> unit;;
value set_page_hmargin : float -> unit;;
value set_page_vmargin : float -> unit;;
value compute_display_frame : picture -> frame;;
value center_and_adjust_picture : picture -> picture;;
value center_and_adjust : picture -> picture;;
(*value make_ps_file : picture -> string -> bool -> unit;;*)
value set_frame_extension_coef : float -> unit;;
value ps_file : picture -> string -> unit;;
value eps_file : picture -> string -> unit;;
value ps : picture ->  unit;;
value eps : picture  -> unit;;
