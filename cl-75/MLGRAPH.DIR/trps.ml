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


(* $Id: trps.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* trps.ml       translates graphics to postscript                       *)
(*              Emmanuel Chailloux & Guy Cousineau                       *)
(*              Tue Jan 21  1992                                         *)




#open "MLgraph";;

#open "compatibility";;
#open "prelude";;
#open "bitmaps";;
#open "fonts";;
#open "texts";;
#open "frames";;
#open "paint";;
#open "geometry";;
#open "graphics_defaults";;
#open "sketches";;
#open "pictures";;
#open "cps";;





let path_size_limit = ref 1500
and symbol_limit = ref 1500
and gsave_limit = ref 30
and dash_size_limit = ref 10;;

let set_path_size_limit x = path_size_limit:=x
and set_symbol_max x = symbol_limit:=x
and set_gsave_max x = gsave_limit:=x
and set_dash_size_limit x = dash_size_limit:=x
;;

exception OutSymbols of int;;

let display_first_elt = 
    function  (Seg [])  ->    ()
    | (Seg ({xc=x;yc=y}::ptl)) ->
             moveto_PS(x,y);do_list (fun {xc=x;yc=y} -> lineto_PS (x,y)) ptl
    | (Arc ({xc=x0;yc=y0},r,a1,a2))  ->
             moveto_PS (x0+.r*.(cosinus a1), y0 +. r*.(sinus a1));
             arc_PS (x0,y0) r a1 a2
    | (Curve(pt1,pt2,pt3,pt4)) ->
             moveto_PS (pt1.xc,pt1.yc);
             curveto_PS (pt2.xc,pt2.yc)
                        (pt3.xc,pt3.yc)
                        (pt4.xc,pt4.yc);;
let display_elt = 
    function  Seg l  ->  do_list (function {xc=x;yc=y} -> lineto_PS (x,y)) l 
    |  (Arc ({xc=x0;yc=y0},r,a1,a2))  ->
             arc_PS (x0,y0) r a1 a2
    | (Curve(pt1,pt2,pt3,pt4)) ->
             lineto_PS (pt1.xc,pt1.yc);
             curveto_PS (pt2.xc,pt2.yc)
                        (pt3.xc,pt3.yc)
                        (pt4.xc,pt4.yc);;

let ps_matrix {m11=a;m12=b;m13=c;m21=d;m22=e;m23=f} =
         PS_MATRIX [a;d;b;e;c;f];;


let gensym,reset_gensym = 
  let c = ref 0 in
    (function () -> c:= !c+1; 
                    if !c > !symbol_limit 
                      then raise (OutSymbols !c)
                      else ("symb"^(string_of_int !c))),
    (function () -> c:=0);;

let newgsave_PS,newgrestore_PS,reset_gsave = 
  let c = ref 0 in 
   (function () -> c:=!c+1;
                   if !c > !gsave_limit 
                   then raise (failwith ("Too much gsave ("^(string_of_int !c)^")"))
                   else gsave_PS()),
   (function () -> c:=!c-1;
                   if !c < 0 
                   then raise (failwith ("Too much grestore("^(string_of_int !c)^")"))
                   else grestore_PS()),
   (function () -> c:=0)
;;


let skel_assoc_list = ref ([]: (geom_element list * string)list)
and bitmap_assoc_list = ref ([]: (bitmap*string)list);;

let reset_trps () = skel_assoc_list:= [];
                    bitmap_assoc_list:= [];
                    reset_gensym();
                    reset_gsave();
                    ();;

let declare_skel_proc_name   skel =
    let n=gensym()
    in  skel_assoc_list := (skel,n)::!skel_assoc_list;
        beginproc_PS n;
        (match skel with [] -> ()
                    |   f::l -> display_first_elt f; 
		                do_list display_elt l;
                                ());
        endproc_PS ();
        n

and declare_bitmap_proc_name  f b = 
  let n = gensym() 
  in bitmap_assoc_list :=(b,n)::!bitmap_assoc_list;
        beginproc_PS n;
        f  b;
        endproc_PS ();
        n
;;

let find_skel_proc_name  skel =
  try let n = assq skel !skel_assoc_list 
      in 
        n 
  with (Not_found) 
          -> declare_skel_proc_name  skel

and find_bitmap_proc_name f b =
  try assq b !bitmap_assoc_list
  with (Not_found) 
           -> declare_bitmap_proc_name f b
;;

let optimized_mode = ref true
and use_dash = ref false;;
let set_optimized_mode b = optimized_mode := b
and set_use_dash b = use_dash:=b;;
(* Some postscript interpretors such as ghostscript do not allow this *)
(* optimization for obscure reasons                                   *)

let rec display_path  b =
  function (Spath skel) 
        -> if !optimized_mode 
              then 
                try 
                  callproc_PS (find_skel_proc_name  skel);
                  if b then closepath_PS()
                with OutSymbols n -> (set_optimized_mode false;display_path b (Spath skel))
              else (match skel 
	            with [] -> () 
                      | f::l -> display_first_elt f; 
		                do_list display_elt l;
                                (if b then closepath_PS());
                                ())
    | (Tpath(t,p)) -> concat_PS (ps_matrix t);
                     display_path b p ;
                     concat_PS (ps_matrix (inverse_transformation t))
    | (Cpath pl) -> do_list (display_path b) pl
;;

let special_display_path  p  s = 
match p with Spath [] -> ()
|  _ -> let o_m = !optimized_mode in 
        begin 
          optimized_mode:=false;
          display_path false p;
          print_string s;
          optimized_mode:=o_m
        end
;;

          
        
let transform_linewidth {m11=a;m12=b;m21=c;m22=d} w =   
   2.0*.w/.(sqrt (a*.a+.c*.c) +. sqrt (b*.b+.d*.d));;


(* - To  bitmaps  ---------------------------  *)

let print_bitmap_PS b =
    for i=0 to b.b_height-1
     do
      output_line_PS b.b_bits.(i)
     done
;;


let print_bitmap_to_hexastring_PS b =
   output_line_PS "<";
   print_bitmap_PS b;
   output_line_PS ">";
   ()
;;


let display_bitmap_PS b  mask mask_value =
  begin
    newgsave_PS();
    translate_PS(0.0,(float_of_int b.b_height) -. 1.0);
    (if mask 
    then 
      newimagemask_PS b.b_width b.b_height mask_value
              (PS_MATRIX [1.0;0.0;0.0;(-.1.0);0.0;1.0]) 
              (((b.b_depth*b.b_width)+7) / 8)
    else 
      newimage_PS b.b_width b.b_height b.b_depth
              (PS_MATRIX [1.0;0.0;0.0;(-.1.0);0.0;1.0]) 
              (((b.b_depth*b.b_width)+7) / 8));

    print_bitmap_PS b;
    newgrestore_PS()
  end
;;

let display_bitmap_from_hexastring_PS b mask mask_value =
  begin
    newgsave_PS();
    translate_PS(0.0,(float_of_int b.b_height) -. 1.0);
    callproc_PS (find_bitmap_proc_name print_bitmap_to_hexastring_PS b);
    (if mask
    then
      imagemask_PS b.b_width b.b_height mask_value
              (PS_MATRIX [1.0;0.0;0.0;(-.1.0);0.0;1.0]) 
              (((b.b_depth*b.b_width)+7) / 8)
    else
      image_PS b.b_width b.b_height b.b_depth
              (PS_MATRIX [1.0;0.0;0.0;(-.1.0);0.0;1.0]) 
              (((b.b_depth*b.b_width)+7) / 8));

    newgrestore_PS()
  end
;;



(* To display texts   *)

let actual_font = ref {font_style=Courier; font_size=12.0};;

let reset_actual_font() = actual_font:= {font_style=Courier; font_size=12.0};();;

let display_text_PS t c = 
  let df = find_font_description t.t_font 
  in
   let size = t.t_font.font_size 
   in
   if !optimized_mode 
   then 
     match c with 
         Gra n       -> f_gray_show_PS  df.font_descr_name size n       t.t_string
       | Rgb (x,y,z) -> f_rgb_show_PS   df.font_descr_name size [x;y;z] t.t_string
       | Hsb (x,y,z) -> f_hsb_show_PS   df.font_descr_name size [x;y;z] t.t_string
   else 
     begin
       f_PS df.font_descr_name size;
       begin 
         match c with
           Gra n       -> setgray_PS n
         | Rgb (x,y,z) -> setrgbcolor_PS [x;y;z]
         | Hsb (x,y,z) -> sethsbcolor_PS [x;y;z]
       end;
       moveto_PS (0.0,0.0);
       show_PS t.t_string
     end
;;

let display_linestyle  c lsty =  
let (nw,ne,nj,nd) = 
  match lsty 
  with {linewidth=w;
        linecap=e;
        linejoin=j;
        dashpattern=d} ->
  (w,  
   (match e with Buttcap   ->  0.0
              |  Squarecap ->  2.0
              |  Roundcap  ->  1.0),
   (match j with Beveljoin  ->  2.0
              |  Roundjoin  ->  1.0
              |  Miterjoin  ->  0.0),d)

in 
   begin
    match c with 
    Gra u ->  f_gray_linestyle_PS u nw ne nj
 |  Rgb (x,y,z) ->  f_rgb_linestyle_PS [x;y;z] nw ne nj 
 |  Hsb (x,y,z) ->  f_hsb_linestyle_PS [x;y;z] nw ne nj 
  end;
  if (nd<>[] or !use_dash) then 
  begin 
    set_use_dash true; 
    if (list_length nd) > !dash_size_limit 
    then failwith ("Too long dash description ("^(string_of_int (list_length nd))^")")
    else setdash_PS (map float_of_int nd) 0.0
  end
;;



let rec display_pict =   
 function (Blank (_,_))  -> ()
   | (Draw (p,lsty,c,b,n)) ->
     if !optimized_mode 
     then display_linestyle c lsty
     else
       begin
         (match c with Gra u -> setgray_PS u
                    |  Rgb (x,y,z) -> setrgbcolor_PS [x;y;z]
                    |  Hsb (x,y,z) -> sethsbcolor_PS [x;y;z]);
         begin
           match lsty with {linewidth=w;
                            linecap=e;
                            linejoin=j;
                            dashpattern=d} ->
            begin
              setlinewidth_PS w;
              (match e with Buttcap   -> setlinecap_PS 0.0
                         |  Squarecap -> setlinecap_PS 2.0
                         |  Roundcap  -> setlinecap_PS 1.0);
              (match j with Beveljoin  -> setlinejoin_PS 2.0
                         |  Roundjoin  -> setlinejoin_PS 1.0
                         |  Miterjoin  -> setlinejoin_PS 0.0);
              if (d<>[] or !use_dash) 
              then 
              begin 
                set_use_dash true; 
                if (list_length d) > !dash_size_limit 
                then failwith ("Too long dash description ("^(string_of_int (list_length d))^")")
                else setdash_PS (map float_of_int d) 0.0
              end
            end
         end
       end;

     if n > !path_size_limit
     then       
       failwith ("Sketch size is limited to "
                 ^ (string_of_int !path_size_limit)
                 ^ " in a fill picture")
     else                       
         begin
           display_path b p;  
           stroke_PS()
         end


| (Fill (p,fsty,c,n)) ->
  if n > !path_size_limit
    then failwith ("Sketch size is limited to "
                   ^ (string_of_int !path_size_limit)
                   ^ " in a fill picture")
    else
    ((match c with Gra u -> setgray_PS u
               |  Rgb (x,y,z) -> setrgbcolor_PS [x;y;z]
               |  Hsb (x,y,z) -> sethsbcolor_PS [x;y;z]);
     display_path false p;
     (match fsty with Nzfill -> fill_PS()
                |  Eofill -> eofill_PS()))
| (Clip (c,p,pict,n)) -> 
  if n > !path_size_limit
    then failwith ("Sketch size is limited to "
                   ^ (string_of_int !path_size_limit)
                   ^ " in a clip picture")
    else
                   newgsave_PS();
                   display_path false p; (* (flat_path p); *)
                   (match c with Nzclip -> clip_PS()
                              |  Eoclip -> eoclip_PS());
		    newpath_PS();
                    display_pict pict;
                    newgrestore_PS()
| (Bitmap b) -> if b.b_depth > 16
                then 
                   failwith ("Bitmap: display_bitmap:"^"more than 16 planes ("^
                             (string_of_int b.b_depth)^ ")")
                else
                  if !optimized_mode 
                  then 
                    try (find_bitmap_proc_name print_bitmap_to_hexastring_PS b;
                        display_bitmap_from_hexastring_PS b false false)
                    with OutSymbols n -> (set_optimized_mode false; display_bitmap_PS b false false)
                  else display_bitmap_PS b false false
| (Bitmapmask (b,c,mask_value)) -> if b.b_depth > 1
                then 
                   failwith ("Bitmapmask: display_bitmap:"^"more than 1 plane ("^
                             (string_of_int b.b_depth)^ ")")
                else
                  begin
                      (match c with Gra u -> setgray_PS u
                           |  Rgb (x,y,z) -> setrgbcolor_PS [x;y;z]
                           |  Hsb (x,y,z) -> sethsbcolor_PS [x;y;z]);
                   if !optimized_mode 
                   then 
                     try (find_bitmap_proc_name print_bitmap_to_hexastring_PS b;
                          display_bitmap_from_hexastring_PS b true mask_value)
                     with OutSymbols n -> (set_optimized_mode false; display_bitmap_PS b true mask_value)
                    else display_bitmap_PS b true mask_value
                  end
| (Text (t,c))   -> display_text_PS t c
| (Tpict(t,pict))  -> newgsave_PS();
                  concat_PS (ps_matrix t); display_pict pict;
                  newgrestore_PS()
| (Cpict pictl)  ->  do_list display_pict pictl;;



let display_picture pict = display_pict (pict.pict);;




