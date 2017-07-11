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


(* $Id: cps.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* cps.ml       A interface between Caml and PostScript                  *)
(*              Emmanuel Chailloux                                       *)
(*              Mon Jan 20  1992                                         *)




#open "compatibility";;
#open "prelude";;




(*                           Body of module CPS                          *)
(*                           __________________                          *)



(*                            New Types                                  *)

type ps_int	= PS_INT of int;;
type ps_float   = PS_FLOAT of float;;
type ps_bool	= PS_BOOL of bool;;
type ps_array	= PS_ARRAY of float list;;
type ps_string 	= PS_STRING of string;;
type ps_matrix 	= PS_MATRIX of float list;;
type ps_font 	= PS_FONT of string;;
type ps_vm	= PS_VM of string;;
type ps_image	= PS_IMAGE of string;;
type ps_channel = PS_FILE  of out_channel * string;;


(*                      Strings convertions                              *)

(* prend 2 fonctions et 2 chaines 
         calcule f ps1 s1 s2 sur tous les elements de s1
         et ramene g s1 s2
*)

let map_string f g s1 s2 = 
  let rec map_string_int p  l = 
    if p=l then (g s1 s2) else (f p s1 s2 ;map_string_int (p+1) l)
  in map_string_int 0( string_length s1) 
;;


let n_replace_string s2 s' p =
let k = ref 0 
and l = string_length s' 
in
 while (!k<l) do (set_nth_ascii(p+ !k,s2,(nth_ascii(!k,s')));incr k) done;
 s2
;;

let substitute_lchar lchar lchar' =
let rec subst_rec  s1 = 
    let s2 = make_string (2*(string_length s1)) space_char
    and p2 = ref 0 
    in
      let f p s1 s2 = 
        let c = nth_ascii (p,s1) 
        in 
          let ch = ascii c 
          in
          if mem ch lchar 
          then 
            let s' = nth lchar' ((index ch lchar)+1)
            in 
              (replace_string  s2 s' !p2;
               p2:=!p2+(string_length s');
               s2)

          else (set_nth_ascii (!p2, s2,c);incr p2;s2)
      and g s1 s2 = s2
      in
      map_string f g s1 s2
in  subst_rec ;;

let substitute_brackets s = substitute_lchar ["[";"]"] ["";""] s;;

let substitute_space s    = substitute_lchar [" "] [""] s ;;

let substitute_brackets_and_space s = 
	substitute_lchar  ["[";"]";" "] ["";"";""] s;; 

let filter_RCLF s =
	substitute_lchar ["\R";"\L"] ["";""] s;;





(*                       Convertion to stringP	                          *)

let stringPS_of_int (i:int) = string_of_int  i;;

let format_float_value = "%f";;
let format_float_PS = ref format_float_value;;
let set_format_float_PS format = format_float_PS:=format;;
let get_format_float_PS () = !format_float_PS;;
let reset_format_float_PS () = format_float_PS:=format_float_value;;


let stringPS_of_float n = 
   let s = format_float "%f" (*!format_float_PS*) n in
   if s = "NaN" then failwith "Not a number";
	let p = index_string s "/"  in
	if (p= -1) then s
	   	  else (set_nth_char s p space_char;(s^" div"));;

let stringPS_of_string s =  
	let gs =  substitute_lchar  ["("  ;")"  ;"\T" ;"\R" ;"\L" ;"\S"] 
				    ["\\(";"\\)";"\\t";"\\r";"\\n";" "] s in
   "("^gs^")";;

let stringPS_of_bool t = string_of_bool t;;

let rec string_lengthlist = 
  function [] -> 0 
       | h::l -> (string_length h)+ (string_lengthlist l)
;;


let rec stringPS_of_float_list = function
    [] -> ""
  | h::t -> ((stringPS_of_float h)^" "^(stringPS_of_float_list t));;

let stringPS_of_array l = ("[ "^(stringPS_of_float_list l)^" ]");;

let stringPS_of_matrix  = fun (PS_MATRIX l) -> (
	if (list_length l) = 6 then  (stringPS_of_array l)
			  else failwith " string_of_matrixPS" );;

let stringPS_of_point = fun  (x,y) -> 
	((stringPS_of_float x)^" "^(stringPS_of_float y));;

let stringPS_of_angle a = (stringPS_of_float a)^" ca ";;

let stringPS_of_font = fun (PS_FONT s) -> s;;

let stringPS_of_vm = fun (PS_VM v) -> v;;

let stringPS_of_image = fun (PS_IMAGE im) -> im;;




(*                            Convertion from stringPS                    *)


let int_of_stringPS s = int_of_string (substitute_space s);;

let float_of_stringPS s =  float_of_string (substitute_space s);;

let string_of_stringPS s =  
   substitute_lchar	["\\(";"\\)";"\\t";"\\r";"\\n";" " ;"\R";"\L"]
	     		["("  ;")"  ;"\T" ;"\R" ;"\L" ;"\S";""  ;""] s ;;

let bool_of_stringPS s = bool_of_string s;;

let array_of_stringPS s = map float_of_string ( words  s);;     

let matrix_of_stringPS  s = PS_MATRIX (map float_of_string (  words   s));;

let point_of_stringPS  s =
    let l = map float_of_string ( words   s) in (nth l 1,nth l 2);;   

let points_of_stringPS s = 
      let points = function
              h1::h2::h3::h4::[] -> ( (h1,h2), (h3,h4))
             | _        -> failwith "Bad conversion in points_of_stringPS"    in points (array_of_stringPS s)
;;

let angle_of_stringPS s = float_of_string s;;

let font_of_stringPS  s = PS_FONT (substitute_lchar ["/"] [" "] s );;

let vm_of_stringPS  s = PS_VM s;;

let image_of_stringPS  s = PS_IMAGE s;;


(*              communication functions  PS <--> CAML                   *)
(*              ---------------------------------------	                *)

(* with                                                                 *) 

(*      default_channel_PS : the PostScript channel                     *)
(*      copy_stream : to copy a file to a open channel                  *)
(*      send_file_PS : to copy a file to the PostScript channel         *)
(*      send_and_flush_command_PS : to send a string as a PS command    *)    
(*      exec_PS  : to execute a PostScript command                      *)
(*      exec_and_send_file_PS : to execute a command and to send a file *)
(*      ask_PS   : to execute a PostScript command and wait for an answer *)
(*	ask_and_exec_PS : ask and execute realy the command		*)


let cps_file_prelude  ()    = !header_lib_directory^"cps_file.ps";;
let cps_pages = ref 0;;
let default_channel_value =  (PS_FILE (std_out,""));;
let default_channel_PS = ref default_channel_value;;

let reset_PS () = default_channel_PS := default_channel_value;;


let rec read_linef i = filter_RCLF (input_line i);;

let immediate_output_line x y = output_line x y ; flush x;;

let copy_stream f b = 
   let a = open_in f in
        try 
	( while true  
	do output_line b (input_line a) done)
        with End_of_file ->  (flush b;close_in a)
;;


let send_file_PS file =
  match !default_channel_PS 
  with  PS_FILE    (ps,name)	  -> copy_stream file ps
;;


let send_and_flush_command_PS  chan com = 
  immediate_output_line chan ("{"^com^"}t")
;;

let exec_PS  s =
  match !default_channel_PS 
  with  PS_FILE (ps,name)	-> output_line ps s
;;

(*  last modifications : send a postscript comment   :  send_comment_PS
 *                       define a postscript symbol  : beginproc_PS, endproc_PS
 *                       execute a postscript symbol : callproc_PS
 *                       write a string on the output PS channel  : 
                                                       output_line_PS 
 *)

let send_comment_PS s = 
  match !default_channel_PS 
  with  PS_FILE (ps,name)  -> output_line ps ("%"^s)
;;

let beginproc_PS  s =
  match !default_channel_PS 
  with PS_FILE (ps,name)	-> output_line ps ("/"^s);output_line ps "{"
;;

let endproc_PS  () =
  match !default_channel_PS 
  with PS_FILE (ps,name)	-> output_line ps "} def"
;;

let callproc_PS = exec_PS;;

let output_line_PS s = 
  match !default_channel_PS 
  with PS_FILE (ps,name)	-> output_line ps s
;;


(* end of the modifications *)


let exec_and_send_file_PS  s file  = 
(* find_file file;  *)
 match !default_channel_PS 
 with PS_FILE (ps,name) -> (output_line ps s; send_file_PS file)   
;;

let ask_PS a  = a;;
let ask_and_exec_PS a  = a;;


(* Open and close the communication channel between Caml <-> PS		*)
(* ------------------------------------------------------------		*)

let close_PS sflag  =
  let close_psfile ps = 
	((if sflag then output_line ps " showpage ");
         cps_pages:=!cps_pages+1;
         output_line ps "end";
         flush ps) in
    match !default_channel_PS 
    with   PS_FILE (ps,name) -> (close_psfile ps;
                                 if  name <> "" then close_out ps; 
		                 reset_PS());
    ();;




let open_PS filename =
  cps_pages:=0;
  if  filename <> "" then 
    let psfile = (filename^".ps") 
    in
      default_channel_PS:=PS_FILE(open_out psfile,filename)
  else
     reset_PS();
  !default_channel_PS
;;

let copy_prelude_PS () = 
  match !default_channel_PS 
  with  PS_FILE(ps,_)  -> 
    if !body_prelude = [] then copy_stream (cps_file_prelude ()) ps
	else do_list (output_line ps) !body_prelude 
;;


(*		    Arithmetic and math operators			*)


let add_PS a b=float_of_stringPS(ask_PS 
 	((stringPS_of_float_list (a::[b]))^" add"));;

let div_PS a b=float_of_stringPS(ask_PS 
 	((stringPS_of_float_list (a::[b]))^" div"));;

let idiv_PS a b=int_of_stringPS(ask_PS 
 	((stringPS_of_float_list ((float_of_int a)::[(float_of_int b)]))^" idiv"));;

let mod_PS a b=int_of_stringPS(ask_PS 
 	((stringPS_of_float_list ((float_of_int a)::[(float_of_int b)]))^" mod"));;


let mul_PS a b=float_of_stringPS(ask_PS 
 	((stringPS_of_float_list (a::[b]))^" mul"));;

let sub_PS a b=float_of_stringPS(ask_PS 
 	((stringPS_of_float_list (a::[b]))^" sub"));;


let abs_PS a = float_of_stringPS (ask_PS ( (stringPS_of_float a)^" abs"));;

let neg_PS a = float_of_stringPS (ask_PS ( (stringPS_of_float a)^" neg"));;

let ceiling_PS a = 
	float_of_stringPS (ask_PS ( (stringPS_of_float a)^" ceiling"));;

let floor_PS a = float_of_stringPS (ask_PS ( (stringPS_of_float a)^" floor"));;

let round_PS a = float_of_stringPS (ask_PS ( (stringPS_of_float a)^" round"));;

let truncate_PS a = 
	float_of_stringPS (ask_PS ( (stringPS_of_float a)^" truncate"));;

let sqrt_PS a = float_of_stringPS (ask_PS ( (stringPS_of_float a)^" sqrt"));;

let atan_PS a b=float_of_stringPS(ask_PS 
 	((stringPS_of_float_list (a::[b]))^" atan"));;

let cos_PS a = float_of_stringPS (ask_PS ( (stringPS_of_float a)^" cos"));;

let sin_PS a = float_of_stringPS (ask_PS ( (stringPS_of_float a)^" sin"));;

let exp_PS a b=float_of_stringPS(ask_PS 
 	((stringPS_of_float_list (a::[b]))^" exp"));;

let ln_PS a = float_of_stringPS (ask_PS ( (stringPS_of_float a)^" ln"));;

let log_PS a = float_of_stringPS (ask_PS ( (stringPS_of_float a)^" log"));;

let rand_PS () = float_of_stringPS (ask_PS " rand");;

let srand_PS a = exec_PS ( (stringPS_of_int a)^" srand");;

let rrand_PS () = float_of_stringPS (ask_PS " rrand");;


(*			   Virtual memory operators			*)
(*			   ------------------------			*)

let save_PS () = vm_of_stringPS ( ask_PS " save sts") ;;

let restore_PS v = exec_PS ((stringPS_of_vm v)^" restore");;

let vmstatus_PS () =array_of_stringPS(ask_PS "vmstatus");;


(*			  Miscellaneous operators			*)
(*			  -----------------------			*)


let version_PS () = string_of_stringPS (ask_PS "version");;


(*			  Graphics state operators			*)
(*			  ------------------------			*)


let gsave_PS () =  exec_PS "gsave";;

let grestore_PS () = exec_PS "grestore";;

let grestoreall_PS () = exec_PS "grestoreall";;

let initgraphics_PS () = exec_PS "initgraphics";;

let setlinewidth_PS n = exec_PS ((stringPS_of_float n)^" setlinewidth");;

let currentlinewidth_PS () = float_of_stringPS ( ask_PS "currentlinewidth");;

let setlinecap_PS n = 
  exec_PS ((stringPS_of_int (int_of_float n))^" setlinecap");;

let currentlinecap_PS () = 
  (float_of_int(int_of_stringPS ( ask_PS "currentlinecap")));;

let setlinejoin_PS n = 
  exec_PS ((stringPS_of_int (int_of_float n))^" setlinejoin");;

let currentlinejoin_PS () = 
  (float_of_int(int_of_stringPS (ask_PS "currentlinejoin")));;

let setmiterlimit_PS n = exec_PS ((stringPS_of_float n)^" setmiterlimit");;

let currentmiterlimit_PS () = float_of_stringPS (ask_PS "currentmiterlimit");;

let setdash_PS arr off = exec_PS ((stringPS_of_array arr)^
                                   (stringPS_of_float off)^" setdash");;

let currentdash_PS () =  array_of_stringPS (ask_PS "currentdash");;

let setgray_PS n = exec_PS ((stringPS_of_float n)^" setgray");;

let currentgray_PS () = float_of_stringPS (ask_PS "currentgray");;

let sethsbcolor_PS l  = 
   match l with 
     [h;s;b] -> exec_PS((stringPS_of_float_list [h;s;b])^" sethsbcolor")
   |      _  -> failwith "Bad argument in sethsbcolor";;

let currenthsbcolor_PS () = array_of_stringPS (ask_PS "currenthsbcolor");;

let setrgbcolor_PS l  = 
   match l with 
     [r;g;b] ->exec_PS((stringPS_of_float_list [r;g;b])^" setrgbcolor")
   |      _   ->failwith "Bad argument in setrgbcolor";;

let currentrgbcolor_PS () = array_of_stringPS (ask_PS "currentrgbcolor");;

(*		Coordinate system and matrix operators			*)
(*		--------------------------------------			*)


let matrix_PS () = let r = ask_PS "matrix" in
	matrix_of_stringPS r
;;

let initmatrix_PS () = exec_PS "initmatrix";;

let identmatrix_PS m = let r = ask_PS ((stringPS_of_matrix m)^" identmatrix")
	in matrix_of_stringPS r
;;

let defaultmatrix_PS m = let r = ask_PS ((stringPS_of_matrix m)^
						" defaultmatrix")
	in matrix_of_stringPS r
;;

let currentmatrix_PS m = let r = ask_PS ((stringPS_of_matrix m)^
						" currentmatrix")
	in matrix_of_stringPS r
;;

let setmatrix_PS m =  exec_PS ((stringPS_of_matrix m)^" setmatrix")
;;

let translate_PS (x,y) = exec_PS 
	((stringPS_of_float x)^" "^(stringPS_of_float y)^" translate");;

let translatem_PS (x,y) l = let r = ask_PS 
	((stringPS_of_float x)^" "^(stringPS_of_float y)^" "^
	 (stringPS_of_matrix l)^" translate")
in
	matrix_of_stringPS r
;;

let scale_PS (x,y) = exec_PS 
        ((stringPS_of_float x)^" "^(stringPS_of_float y)^" scale");;


let scalem_PS (x,y) l = let r = ask_PS 
	((stringPS_of_float x)^" "^(stringPS_of_float y)^" "^
	 (stringPS_of_matrix l)^" scale")
in
	matrix_of_stringPS r
;;

let rotate_PS x = exec_PS 
	((stringPS_of_float x)^" rotate");;


let rotatem_PS x l = let r = ask_PS 
	((stringPS_of_float x)^" "^
	 (stringPS_of_matrix l)^" rotate")
in
	matrix_of_stringPS r
;;


let concat_PS l = exec_PS ((stringPS_of_matrix l)^" concat")
;;

let concatmatrix_PS l1 l2 l3 = let r = ask_PS 
	((stringPS_of_matrix l1)^" "^
	 (stringPS_of_matrix l2)^" "^
	 (stringPS_of_matrix l3)^" concatmatrix")
in
	matrix_of_stringPS r
;;

let transform_PS (x,y)  = let r = ask_PS 
	((stringPS_of_float x)^" "^(stringPS_of_float y)^" "^" transform")
in
	point_of_stringPS r
;;


let transformm_PS (x,y) l = let r = ask_PS 
	((stringPS_of_float x)^" "^(stringPS_of_float y)^" "^
	 (stringPS_of_matrix l)^" transform")
in
	point_of_stringPS r
;;

let dtransform_PS (x,y)  = let r = ask_PS 
	((stringPS_of_float x)^" "^(stringPS_of_float y)^" "^" dtransform")
in
	point_of_stringPS r
;;

let dtransformm_PS (x,y) l = let r = ask_PS 
	((stringPS_of_float x)^" "^(stringPS_of_float y)^" "^
	 (stringPS_of_matrix l)^" dtransform")
in
	point_of_stringPS r
;;

let itransform_PS (x,y) l = let r = ask_PS 
	((stringPS_of_float x)^" "^(stringPS_of_float y)^" "^" itransform")
in
	point_of_stringPS r
;;

let itransformm_PS (x,y) l = let r = ask_PS 
	((stringPS_of_float x)^" "^(stringPS_of_float y)^" "^
	 (stringPS_of_matrix l)^" itransform")
in
	point_of_stringPS r
;;

let idtransform_PS (x,y) l = let r = ask_PS 
	((stringPS_of_float x)^" "^(stringPS_of_float y)^" "^" idtransform")
in
	point_of_stringPS r
;;

let idtransformm_PS (x,y) l = let r = ask_PS 
	((stringPS_of_float x)^" "^(stringPS_of_float y)^" "^
	 (stringPS_of_matrix l)^" idtransform")
in
	point_of_stringPS r
;;

let invertmatrix_PS l1 l2 = let r = ask_PS 
	((stringPS_of_matrix l1)^" "^(stringPS_of_matrix l2)^" invertmatrix")
in
	matrix_of_stringPS r
;;


(*			Path construction operators			*)
(*			---------------------------			*)


let newpath_PS   () = exec_PS "newpath"
;;

let currentpoint_PS () =
 let s = "currentpoint" in point_of_stringPS (ask_PS s)
;;

let moveto_PS p = 
 let s = ((stringPS_of_point p)^" moveto") in  exec_PS s
;;

let rmoveto_PS p = 
 let s = ((stringPS_of_point p)^" rmoveto") in    exec_PS s
;;

let lineto_PS p =
 let s = ((stringPS_of_point p)^" lineto") in exec_PS s
;;

let rlineto_PS p = 
 let s = ((stringPS_of_point p)^" rlineto") in exec_PS s
;;


let arc_PS p r a1 a2 = 
 let s = ((stringPS_of_point p)^" "^(stringPS_of_float r)^" "^
          (stringPS_of_angle a1)^" "^(stringPS_of_angle a2)^" arc") in
	exec_PS s
;;


let arcn_PS p r a1 a2 = 
 let s = ((stringPS_of_point p)^" "^(stringPS_of_float r)^" "^
          (stringPS_of_angle a1)^" "^(stringPS_of_angle a2)^" arcn") in
	exec_PS s
;;


let arcto_PS p1 p2 r =  
 let s = ((stringPS_of_point p1)^" "^(stringPS_of_point p2)^" "^
          (stringPS_of_float r)^" arcto") in
	(points_of_stringPS (ask_PS s));;




let curveto_PS p1 p2 p3 = 
 let s = ((stringPS_of_point p1)^" "^(stringPS_of_point p2)^" "^
          (stringPS_of_point p3)^" curveto") in
        exec_PS s
;;

let rcurveto_PS p1 p2 p3 = 
 let s = ((stringPS_of_point p1)^" "^(stringPS_of_point p2)^" "^
          (stringPS_of_point p3)^" rcurveto") in
        exec_PS s
;;

let closepath_PS () = exec_PS "closepath";;

let flattenpath_PS () = exec_PS "flattenpath";;

let reversepath_PS () = exec_PS "reversepath";;

let strokepath_PS () = exec_PS "strokepath";;

let charpath_PS s b = exec_PS ((stringPS_of_string s)^" "^
			        (stringPS_of_bool b)^" charpath");;

let clippath_PS () = exec_PS "clippath";;

let pathbbox_PS () = 
 let s = "pathbbox" in
	points_of_stringPS (ask_PS s);;

let initclip_PS () = exec_PS "initclip";;

let clip_PS () = exec_PS "clip";;

let eoclip_PS () = exec_PS "eoclip";;


(*			Painting Operator				*)
(*			-----------------				*)

let erasepage_PS () = exec_PS " erasepage";;

let fill_PS () = exec_PS "fill";;

let eofill_PS () = exec_PS "eofill";;

let stroke_PS () = exec_PS "stroke";;

let image_file_PS w h b m str p = 
try 
exec_and_send_file_PS 
        (("/picstr "^(stringPS_of_int p)^" string def ")^
	(stringPS_of_int w)^" "^
	(stringPS_of_int h)^" "^
	(stringPS_of_int b)^" "^
	(stringPS_of_matrix m )^" newimage " )
	(stringPS_of_image str)
  with _ -> 
        failwith "bitmap file not exist"
;;


    
let imagemask_file_PS w h i m str p = 
try 
   exec_and_send_file_PS 
      (("/picstr "^(stringPS_of_int p)^" string def ")^
	(stringPS_of_int w)^" "^
	(stringPS_of_int h)^" "^
        (stringPS_of_bool i)^" "^
	(stringPS_of_matrix m )^" newimagemask ") 
	(stringPS_of_image str) 
     with _ -> 
        failwith "bitmap file not exist"
;;


(* *)

let image_PS w h b m p = 
       output_line_PS 
       ((stringPS_of_int w)^" "^
	(stringPS_of_int h)^" "^
	(stringPS_of_int b)^" "^
	(stringPS_of_matrix m )^" {} image " )
;;

let imagemask_PS w h i m p = 
      output_line_PS
       ((stringPS_of_int w)^" "^
	(stringPS_of_int h)^" "^
        (stringPS_of_bool i)^" "^
	(stringPS_of_matrix m )^" {} imagemask ") 
;;

let newimage_PS w h b m p = 
       output_line_PS 
       (("/picstr "^(stringPS_of_int p)^" string def ")^
	(stringPS_of_int w)^" "^
	(stringPS_of_int h)^" "^
	(stringPS_of_int b)^" "^
	(stringPS_of_matrix m )^" newimage " )
;;


let newimagemask_PS w h i m p = output_line_PS
       (("/picstr "^(stringPS_of_int p)^" string def ")^
	(stringPS_of_int w)^" "^
	(stringPS_of_int h)^" "^
        (stringPS_of_bool i)^" "^
	(stringPS_of_matrix m )^" newimagemask ") 
;;

(*		Device setup and output operators			*)
(*		---------------------------------			*)


let showpage_PS  () = exec_PS "showpage";
                      cps_pages:=!cps_pages+1
;;

let copypage_PS  () = exec_PS "copypage";;


(*			Character and font operators			*)
(*			----------------------------			*)


let findfont_PS k =
 let s = ("/"^k^" findfont sts") in
  (font_of_stringPS ( ask_and_exec_PS s)) ;;


let scalefont_PS f n =
 let s = ((stringPS_of_font f) ^ " "^
	(stringPS_of_float_list [n])^" scalefont sts") 
   in (font_of_stringPS(ask_and_exec_PS s))
;;

let setfont_PS f =
 let s = ((stringPS_of_font f)^" setfont") in exec_PS s
;;

let show_PS s = 
  exec_PS ((stringPS_of_string s)^" show");;

let ashow_PS s (ax,ay) = 
  exec_PS ((stringPS_of_float ax)^" "^(stringPS_of_float ay)^" "^
           (stringPS_of_string s)^" ashow");;

let widthshow_PS s c (cx,cy) = 
  exec_PS ((stringPS_of_float cx)^" "^(stringPS_of_float cy)^" "^
           (stringPS_of_float c)^" "^(stringPS_of_string s)^" widthshow");;

let awidthshow_PS s (ax,ay) c (cx,cy) = 
  exec_PS ((stringPS_of_float cx)^" "^(stringPS_of_float cy)^" "^
           (stringPS_of_float c)^" "^
           (stringPS_of_float ax)^" "^(stringPS_of_float ay)^" "^
           (stringPS_of_string s)^" awidthshow");;

let stringwidth_PS s =
  let com = ((stringPS_of_string s)^" stringwidth ") in 
    let u = words (ask_PS com) in
  ( float_of_stringPS (nth u 1),float_of_stringPS  (nth u 2))
;;


(*             Extentions                                                  *)
(*             ----------                                                  *)

let f_PS s n = exec_PS ((stringPS_of_float n)^" /"^s^" F");;

let f_gray_show_PS fn sz g s  = 
  exec_PS ((stringPS_of_string s)^" "^
           (stringPS_of_float g)^" "^
           (stringPS_of_float sz)^" /"^fn^" F_gray_show");;

let f_rgb_show_PS fn sz lc (* [r;g;b] *) s =
  exec_PS((stringPS_of_string s)^" "^
          (stringPS_of_float_list lc)^" "^
          (stringPS_of_float sz)^" /"^fn^ " F_rgb_show")
;;


let f_hsb_show_PS fn sz lc (*[h;s;b]*)  str = 
  exec_PS((stringPS_of_string str)^" "^
          (stringPS_of_float_list lc)^" "^
          (stringPS_of_float sz)^" /"^fn^ " F_hsb_show");;


let f_gray_linestyle_PS u w e j = 
  exec_PS ((stringPS_of_float u)^" "^(stringPS_of_float w)^
           " "^(stringPS_of_int(int_of_float e))^" "^
           (stringPS_of_int(int_of_float j))^
           " F_gray_linestyle");;

let f_rgb_linestyle_PS lc (*[r;g;b]*)  w e j = 
  exec_PS ((stringPS_of_float_list lc)^" "^
           (stringPS_of_float w)^" "^
           " "^(stringPS_of_int(int_of_float e))^" "^
           (stringPS_of_int(int_of_float j))^
           " F_rgb_linestyle");;

let f_hsb_linestyle_PS lc (*[h;s;b]*)  w e j = 
  exec_PS ((stringPS_of_float_list lc)^" "^
           (stringPS_of_float w)^" "^
           " "^(stringPS_of_int(int_of_float e))^" "^
           (stringPS_of_int(int_of_float j))^
           " F_hsb_linestyle");;

let name_PS () = string_of_stringPS (ask_PS "nameps");;


(*                  Output interface of CPS module                         *)
(*                  ______________________________                         *)











