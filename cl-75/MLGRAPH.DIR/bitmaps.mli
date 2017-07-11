
#open "MLgraph";;


type bitmap = {b_width:int;
               b_height:int;
               b_depth:int;
               b_bits:string vect};;

value bitmap_width : bitmap -> int;;
value bitmap_height : bitmap -> int;;
value bitmap_depth : bitmap -> int;;
value create_bitmap : int -> int -> int -> bitmap;;

(*value ascii_code : char -> int;;*)
(*value ascii_0 : int;;*)
(*value ascii_9 : int;;*)
(*value ascii_a : int;;*)
(*value ascii_f : int;;*)
(*value ascii_A : int;;*)
(*value ascii_F : int;;*)
(*value conv_four_bits : int -> int;;*)
(*value iconv_four_bits : int -> int;;*)
(*value nth_conv : int * string -> int;;*)
(*value set_nth : int * string * int -> unit;;*)
(*value char_map_bitmap : (int -> int) -> bitmap -> bitmap;;*)
value sub_bitmap : bitmap -> int * int -> int * int -> bitmap;;
value copy_bitmap : bitmap -> bitmap;;
(*value mask0001 : int;;*)
(*value mask0010 : int;;*)
(*value mask0100 : int;;*)
(*value mask1000 : int;;*)
(*value mask0011 : int;;*)
(*value mask1100 : int;;*)
(*value mask1110 : int;;*)
(*value mask1101 : int;;*)
(*value mask1011 : int;;*)
(*value mask0111 : int;;*)
(*value mask1111 : int;;*)
(*value lnot : int -> int;;*)

value invert_bitmap : bitmap -> bitmap;;

(*value lshift : int * int -> int;;*)
(*value val1 : string -> int -> int -> int;;*)
(*value val2 : string -> int -> int -> int;;*)
(*value val4 : string -> int -> int;;*)
(*value val8 : string -> int -> int;;*)
(*value val16 : string -> int -> int;;*)
(*value change_val1 : string -> int -> int -> int -> unit;;*)
(*value change_val2 : string -> int -> int -> int -> unit;;*)
(*value change_val4 : string -> int -> int -> unit;;*)
(*value change_val8 : string -> int -> int -> unit;;*)
(*value change_val16 : string -> int -> int -> unit;;*)
(*value set_pixel1 : bitmap -> int -> int -> int -> unit;;*)
(*value set_pixel2 : bitmap -> int -> int -> int -> unit;;*)
(*value set_pixel4 : bitmap -> int -> int -> int -> unit;;*)
(*value set_pixel8 : bitmap -> int -> int -> int -> unit;;*)
(*value set_pixel16 : bitmap -> int -> int -> int -> unit;;*)
value set_pixel : bitmap -> int -> int -> int -> unit;;
(*value get_pixel1 : bitmap -> int -> int -> int;;*)
(*value get_pixel2 : bitmap -> int -> int -> int;;*)
(*value get_pixel4 : bitmap -> int -> int -> int;;*)
(*value get_pixel8 : bitmap -> int -> int -> int;;*)
(*value get_pixel16 : bitmap -> int -> int -> int;;*)
value get_pixel : bitmap -> int -> int -> int;;
(*value map_hexabyte1 : (int -> int) -> int -> int;;*)
(*value map_hexabyte2 : (int -> int) -> int -> int;;*)
(*value map_bitmap8 : (int -> int) -> bitmap -> bitmap;;*)

value convert_bitmap : int * (int -> int) -> bitmap -> bitmap;;
value map_bitmap : (int -> int) -> bitmap -> bitmap;;
value read_bitmap : int -> string -> bitmap;;
value write_bitmap : bitmap -> string -> unit;;
value bitmap_frame : bitmap -> frame;;
value bitmap_hull : bitmap -> point list;;
