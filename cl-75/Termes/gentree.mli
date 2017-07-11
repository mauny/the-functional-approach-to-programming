(*+gentree+*)
type 'a gentree = GenNode of 'a * 'a gentree list;;
(*+gentree+*)

value int_of_string : string -> int;;
value string_of_ident : string -> string;;
value gentree_of_string : (string -> 'a) -> string -> 'a gentree;;

value parse_string : string -> string;;
value parse_gentree : (string -> 'a) -> string -> 'a gentree;;

(*
value ident_max_length : int;;
value ident_buf : string;;
value int_parser : token stream -> int;;
value parse_int : string -> int;;
value int_of_string : string -> int;;
value string_parser : token stream -> string;;
value parse_string : string -> string;;
value parse_ident : string -> string;;
value string_of_string : string -> string;;
value string_of_ident : string -> string;;
value string_int_parser : token stream -> string * int;;
value parse_string_int : string -> string * int;;
value string_of_stream : char stream -> string;;
value gentree_parser : (string -> 'a) -> char stream -> 'a gentree;;
value parse_gentree : (string -> 'a) -> string -> 'a gentree;;
value gentree_of_string : (string -> 'a) -> string -> 'a gentree;;
value print_gentree : ('a -> unit) -> 'a gentree -> unit;;
value print_gentree_list : ('a -> unit) -> 'a gentree list -> unit;;
value string_of_gentree : ('a -> unit) -> 'a gentree -> unit;;
*)
