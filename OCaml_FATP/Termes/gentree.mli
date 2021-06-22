(** +gentree+*)
type 'a gentree = GenNode of 'a * 'a gentree list ;;

val int_of_string : string -> int ;;
val string_of_ident : string -> string ;;
val gentree_of_string : (string -> 'a) -> string -> 'a gentree ;;

val parse_string : string -> string ;;
val parse_gentree : (string -> 'a) -> string -> 'a gentree ;;

(*
val ident_max_length : int ;;
val ident_buf : string ;;
val int_parser : token stream -> int ;;
val parse_int : string -> int ;;
val int_of_string : string -> int ;;
val string_parser : token stream -> string ;;
val parse_string : string -> string ;;
val parse_ident : string -> string ;;
val string_of_string : string -> string ;;
val string_of_ident : string -> string ;;
val string_int_parser : token stream -> string * int ;;
val parse_string_int : string -> string * int ;;
val string_of_stream : char stream -> string ;;
val gentree_parser : (string -> 'a) -> char stream -> 'a gentree ;;
val parse_gentree : (string -> 'a) -> string -> 'a gentree ;;
val gentree_of_string : (string -> 'a) -> string -> 'a gentree ;;
val print_gentree : ('a -> unit) -> 'a gentree -> unit ;;
val print_gentree_list : ('a -> unit) -> 'a gentree list -> unit ;;
val string_of_gentree : ('a -> unit) -> 'a gentree -> unit ;;
*)
