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


(* $Id: fonts.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* fonts.ml                                                              *)
(*            Emmanuel Chailloux & Guy Cousineau                         *)
(*            Mon Jan 20 1992                                            *)





#open "MLgraph";;

#open "compatibility";;
#open "prelude";;




(* somes variables of font description *)


let courier_descr =
{font_descr_filename="Cour.fnt"; font_descr_name = ""; font_descr_height=12.0; font_descr_width=7.2;
font_descr_descr=[||];
font_descr_descr_bbox=[||]
};;

let courier_Bold_descr =
{font_descr_filename="Cour-B.fnt"; font_descr_name=""; font_descr_height=12.0; font_descr_width=12.0;
font_descr_descr=[||];
font_descr_descr_bbox=[||]};;


let courier_Oblique_descr =
{font_descr_filename="Cour-O.fnt";  font_descr_name=""; font_descr_height=12.0; font_descr_width=12.0;
font_descr_descr=[||];
font_descr_descr_bbox=[||]};;


let courier_BoldOblique_descr =
{font_descr_filename="Cour-BO.fnt";  font_descr_name=""; font_descr_height=12.0; font_descr_width=12.0;
font_descr_descr=[||];
font_descr_descr_bbox=[||]};;


let times_Roman_descr =
{font_descr_filename="Time-R.fnt";  font_descr_name=""; font_descr_height=12.0; font_descr_width=12.0;
font_descr_descr=[||];
font_descr_descr_bbox=[||]};;


let times_Bold_descr =
{font_descr_filename="Time-B.fnt";  font_descr_name=""; font_descr_height=12.0; font_descr_width=12.0;
font_descr_descr=[||];
font_descr_descr_bbox=[||]};;


let times_Italic_descr =
{font_descr_filename="Time-I.fnt";  font_descr_name=""; font_descr_height=12.0; font_descr_width=12.0;
font_descr_descr=[||];
font_descr_descr_bbox=[||]};;


let times_BoldItalic_descr =
{font_descr_filename="Time-BI.fnt";  font_descr_name=""; font_descr_height=12.0; font_descr_width = 12.0; 
font_descr_descr=[||];
font_descr_descr_bbox=[||]};;



let helvetica_descr =
{font_descr_filename="Helv.fnt";font_descr_name=""; font_descr_height=12.0; font_descr_width=12.18;
font_descr_descr=[||];
font_descr_descr_bbox=[||]};;



let helvetica_Bold_descr =
{font_descr_filename="Helv-B.fnt"; font_descr_name=""; font_descr_height=12.0; font_descr_width=12.0;
font_descr_descr=[||];
font_descr_descr_bbox=[||]};;



let helvetica_Oblique_descr =
{font_descr_filename="Helv-O.fnt"; font_descr_name=""; font_descr_height=12.0; font_descr_width=12.0;
font_descr_descr=[||];
font_descr_descr_bbox=[||]};;



let helvetica_BoldOblique_descr =
{font_descr_filename="Helv-BO.fnt"; font_descr_name=""; font_descr_height=12.0; font_descr_width=12.0;
font_descr_descr=[||];
font_descr_descr_bbox=[||]};;



let symbol_descr =
{font_descr_filename="Symb.fnt";  font_descr_name=""; font_descr_height=12.0; font_descr_width=12.0;
font_descr_descr=[||];
font_descr_descr_bbox=[||]};;


exception Find of int;;


let pos_char_in_string s c b e = 
try 
 let l = string_length s in 
   for i=(max_int 0 b) to (min_int e (l-1)) do
      if nth_char s i = c then raise (Find i)
   done;
   -1
with Find x -> x
;;

let floatpair_of_string s = 
let ep = (string_length s)-1 in 
let pc = pos_char_in_string s comma_char 0 ep
and op = pos_char_in_string s open_par_char 0 ep 
and cp = pos_char_in_string s close_par_char 0 ep
in
  if (pc<0) or (op < 0) or (cp <0)  then raise (Failure ("floatpair_of_string "^"bad format"))
  else
   (float_of_string (sub_string s (op+1) (pc-op-1)),
    float_of_string (sub_string s (pc+1) (cp-pc-1)))
;;

let bbox_of_string s = 
let ep = (string_length s) -1 in
let pc = pos_char_in_string s comma_char ((pos_char_in_string s comma_char 0 ep)+1) ep
and op = pos_char_in_string s open_par_char 0 ep
and cp = pos_char_in_string s close_par_char 
            ((pos_char_in_string s close_par_char 
                ((pos_char_in_string s close_par_char 0 ep)+1) ep) +1)
         ep
in
  if (pc<0) or (op < 0) or (cp <0)  then raise (Failure ("bbox_of_string "^"bad format"))
  else
   (floatpair_of_string (sub_string s (op+1) (pc-op-1)),
    floatpair_of_string (sub_string s (pc+1) (cp-pc-1)))
;;


let load_font filename = 
let name = !font_lib_directory^filename in
let chan = (try open_in name with e -> prerr_endline (" cannot open file : " ^ name ); raise e)
 in 
let r = ref "" 
and c = ref 0
and n = ref ""
and h = ref 0.0
and w = ref 0.0
and d = ref (make_vect 255 0.0)
and db = ref (make_vect 255 ((0.0,0.0),(0.0,0.0)))
in 
  try 
    while true 
    do 
      r := input_line chan;
      match !r with
            "Name"   ->  n:= input_line chan;()
        |   "Height" ->  h:= float_of_string (input_line chan);()
        |   "Width"  ->  w:= float_of_string (input_line chan);()
        |   "Descr"  ->  r:= input_line chan; 
                         if !r="empty" then (d:=[||];())
                         else 
                          (!d.(0)<- float_of_string (!r);
                           for i=1 to 254 do r:=input_line chan; !d.(i)<-float_of_string(!r) done)
        |   "Descr_bbox" -> 
              for i=0 to 254 
                 do 
                   r:=input_line chan;
                   !db.(i)<-bbox_of_string (!r) 
                 done
        |   _ -> ()
    done;
   close_in chan;
   {font_descr_filename=filename;font_descr_name= !n;font_descr_height= !h;font_descr_width= !w;font_descr_descr= !d;font_descr_descr_bbox= !db}
  with End_of_file -> 
   (close_in chan;
     {font_descr_filename=filename;font_descr_name= !n;font_descr_height= !h;font_descr_width= !w;font_descr_descr= !d;font_descr_descr_bbox= !db})
;;

 
