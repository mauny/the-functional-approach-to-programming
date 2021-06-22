





















open Compatibility
open Prelude
open Geometry
open Frames
open Paint
open Fonts
open Texts
open Pictures
open Graphics_defaults

(* Les options *)

type option =
   SOption of string
 | IOption of int
 | FOption of float
 | BOption of bool
 | POption
 | COption of color
 | LOption of string*float
 | DOption of int list
 | JOption of linejoin
 | CapOption of linecap
 | FillOption of fillstyle
 | FontOption of font_style;;



val string : 'a -> string -> 'a * option;;
val bool : 'a -> bool -> 'a * option;;
val int : 'a -> int -> 'a * option;;
val float : 'a -> float -> 'a * option;;
val option : 'a -> 'a * option;;
val color : 'a -> color -> 'a * option;;
val dashPattern : int list -> string * option;;
val join : linejoin -> string * option;;
val cap : linecap -> string * option;;
val fillStyle : fillstyle -> string * option;;
val lineLabel : string -> float -> string * option;;
val font : font_style -> string * option;;
exception OptionError
;;

val theString : ('a * option) list -> 'a -> string -> string;;
val theInt : ('a * option) list -> 'a -> int -> int;;
val theFloat : ('a * option) list -> 'a -> float -> float;;
val theBool : ('a * option) list -> 'a -> bool -> bool;;
val theOption : ('a * 'b) list -> 'a -> bool;;
val theColor : ('a * option) list -> 'a -> color -> color;;
val theDashPattern : (string * option) list -> int list -> int list;;
val theLineJoin : (string * option) list -> linejoin -> linejoin;;
val theLineCap : (string * option) list -> linecap -> linecap;;
val theFillStyle : (string * option) list -> fillstyle -> fillstyle;;
val theLineLabel : (string * option) list -> option;;
val theFont : (string * option) list -> font_style -> font_style;;
val findOption : ('a * 'b) list -> 'a -> 'b;;
