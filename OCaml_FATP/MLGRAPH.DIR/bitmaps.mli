(* $Id: bitmaps.mlip,v 1.1 1997/08/14 11:34:50 emmanuel Exp $ *)

open Compatibility
open Geometry
open Frames


type bitmap = {b_width:int;
               b_height:int;
               b_depth:int;
               b_bits:string vect};;

val bitmap_width : bitmap -> int;;
val bitmap_height : bitmap -> int;;
val bitmap_depth : bitmap -> int;;
val create_bitmap : int -> int -> int -> bitmap;;

(*val ascii_code : char -> int;;*)
(*val ascii_0 : int;;*)
(*val ascii_9 : int;;*)
(*val ascii_a : int;;*)
(*val ascii_f : int;;*)
(*val ascii_A : int;;*)
(*val ascii_F : int;;*)
(*val conv_four_bits : int -> int;;*)
(*val iconv_four_bits : int -> int;;*)
(*val nth_conv : int * string -> int;;*)
(*val set_nth : int * string * int -> unit;;*)
(*val char_map_bitmap : (int -> int) -> bitmap -> bitmap;;*)
val sub_bitmap : bitmap -> int * int -> int * int -> bitmap;;
val copy_bitmap : bitmap -> bitmap;;
(*val mask0001 : int;;*)
(*val mask0010 : int;;*)
(*val mask0100 : int;;*)
(*val mask1000 : int;;*)
(*val mask0011 : int;;*)
(*val mask1100 : int;;*)
(*val mask1110 : int;;*)
(*val mask1101 : int;;*)
(*val mask1011 : int;;*)
(*val mask0111 : int;;*)
(*val mask1111 : int;;*)
(*val lnot : int -> int;;*)

val invert_bitmap : bitmap -> bitmap;;

(*val lshift : int * int -> int;;*)
(*val val1 : string -> int -> int -> int;;*)
(*val val2 : string -> int -> int -> int;;*)
(*val val4 : string -> int -> int;;*)
(*val val8 : string -> int -> int;;*)
(*val val16 : string -> int -> int;;*)
(*val change_val1 : string -> int -> int -> int -> unit;;*)
(*val change_val2 : string -> int -> int -> int -> unit;;*)
(*val change_val4 : string -> int -> int -> unit;;*)
(*val change_val8 : string -> int -> int -> unit;;*)
(*val change_val16 : string -> int -> int -> unit;;*)
(*val set_pixel1 : bitmap -> int -> int -> int -> unit;;*)
(*val set_pixel2 : bitmap -> int -> int -> int -> unit;;*)
(*val set_pixel4 : bitmap -> int -> int -> int -> unit;;*)
(*val set_pixel8 : bitmap -> int -> int -> int -> unit;;*)
(*val set_pixel16 : bitmap -> int -> int -> int -> unit;;*)
val set_pixel : bitmap -> int -> int -> int -> unit;;
(*val get_pixel1 : bitmap -> int -> int -> int;;*)
(*val get_pixel2 : bitmap -> int -> int -> int;;*)
(*val get_pixel4 : bitmap -> int -> int -> int;;*)
(*val get_pixel8 : bitmap -> int -> int -> int;;*)
(*val get_pixel16 : bitmap -> int -> int -> int;;*)
val get_pixel : bitmap -> int -> int -> int;;
(*val map_hexabyte1 : (int -> int) -> int -> int;;*)
(*val map_hexabyte2 : (int -> int) -> int -> int;;*)
(*val map_bitmap8 : (int -> int) -> bitmap -> bitmap;;*)

val convert_bitmap : int * (int -> int) -> bitmap -> bitmap;;
val map_bitmap : (int -> int) -> bitmap -> bitmap;;
val read_bitmap : int -> string -> bitmap;;
val write_bitmap : bitmap -> string -> unit;;
val bitmap_frame : bitmap -> frame;;
val bitmap_hull : bitmap -> point list;;
