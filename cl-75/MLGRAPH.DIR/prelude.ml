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

(* $Id: prelude.mlp,v 1.2 1997/08/18 13:52:01 emmanuel Exp $ *)
(*  prelude :         basic functions                                    *)
(*                    Emmanuel Chailloux & Guy Cousineau                 *)
(*                    Fri Jan 17  1992                                   *)


(* Compatibilite CAML light - Objective CAML *)
(* Caml Light *)








#open "compatibility";;






(* function composition *)
let compose = fun f g x -> f(g(x));;
let o = compose;;

(* basic functions on integers *)
let max_int a b = if lt_int a  b then b else a;;
let min_int a b = if lt_int a  b then a else b;;

(* basic functions on floats *)

let max_float x y = if lt_float x  y then y else x;;
let min_float x y = if lt_float x  y then x else y;;

let pi = 2.0 *. (acos 0.0);;
let cva a = pi *. a /. 180.0;;

let sinus =  compose sin cva;;
let cosinus = compose cos cva;;


(* string operations *)

let explode s = 
  let l = ref ([]:char list) 
  in 
   for i=(string_length s)-1 downto 0 do 
     l := (nth_char s i)::!l
   done;
   !l;;

let explode_ascii s = 
  let l = ref ([]:int list) 
  in 
   for i=(string_length s)-1 downto 0 do 
     l := (int_of_char(nth_char s i))::!l
   done;
   !l;;



let nth_ascii (n,s) = int_of_char(nth_char s n);;
let set_nth_ascii (n,s,c) = set_nth_char s n (char_of_int c);;


let ascii x = let s = (create_string 1) in 
( set_nth_char s 0 (char_of_int x) ; s)
;;


let rec nth l i = match l with [] -> failwith "nth"
                          | h::t -> if i=1 then h else nth t (i-1)
;;

let extract_string = sub_string
;;

let string_of_bool = function true -> "true" | false -> "false"
;;

let bool_of_string = function 
  "true" -> true 
| "false" -> false 
| _ -> failwith "bool_of_string"
;;

let  index_string s c = 
  let long = string_length s 
  in 
    let rec irec i = 
      if i<long 
      then if sub_string s i 1 = c 
           then i 
           else irec (i+1) 
      else (-1)
      in 
        irec 0
;;

let words s = 
    let ns = s^" " in
    let l = string_length ns in
    let i = ref 0 
    and li = ref 0 
    and lres = ref ([]:string list) 
    in 
      while !i<l do 
        ((if (nth_char ns !i = space_char )
          then
            (if !i = !li 
             then (i := !i+1;li := !i)
             else (lres:= !lres@[sub_string ns !li (!i - !li)];
                   i:= !i+1;li:= !i))
          else i:= !i+1
         ))
      done;
      !lres
;;

(* I/O functions  *)

let message s = print_string s;print_newline();;

let directory_concat_string = ref "/";;
let graphics_directory = ref ("." ^ !directory_concat_string);;

let graphics_lib_directory = 
   ref (!graphics_directory ^ "MLgraph.lib" ^ !directory_concat_string );;
let header_lib_directory = 
   ref (!graphics_lib_directory ^ "Headers" ^ !directory_concat_string);;
let font_lib_directory = 
   ref (!graphics_lib_directory ^ "Fonts"  ^ !directory_concat_string);;
let bin_lib_directory = 
   ref (!graphics_lib_directory ^ "Bin" ^ !directory_concat_string);;

let change_graphics_directory s = 
  graphics_directory := s ^ !directory_concat_string;
  graphics_lib_directory := 
    !graphics_directory  ^ "MLgraph.lib" ^ !directory_concat_string;
  header_lib_directory :=
    !graphics_lib_directory ^ "Headers" ^ !directory_concat_string;
  font_lib_directory :=
    !graphics_lib_directory ^ "Fonts" ^ !directory_concat_string;
  bin_lib_directory :=
    !graphics_lib_directory ^ "Bin" ^ !directory_concat_string;
  ()
;;

let adobe_version = ref "1.0"
and mlgraph_version = ref "2.1"
;;

let begin_prelude1 = ref ("!PS-Adobe-" ^ !adobe_version);;
let begin_prelude2 = ref ("%Creator: MLgraph version " ^ !mlgraph_version);;
let begin_prelude3 = ref ("%pages: (atend)");;
let end_prelude   = ref "%EndComments";;

let body_prelude = ref ([]:string list);;
let modify_body_prelude s = body_prelude := s;;

let output_line outchannel s = 
  output_string outchannel s;
  output_char outchannel lf_char
;;

(* list functions *)

let hd = function [] -> failwith "hd" | a::l -> a
;;
let tl = function [] -> failwith "tl" | a::l -> l
;;
let rec last = function [] -> failwith "last" | a::[] -> a |a::l -> last l
;;

(* iterate functions *)

let rec iterate f n x = if n = 0 then x else iterate f (n-1) (f x)
;;



