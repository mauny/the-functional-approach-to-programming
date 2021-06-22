type exp =
  | Constant of int
  | Variable of string
  | Addition of exp * exp
  | Multiplication of exp * exp ;;

val exp_parser : Lexxer.token Stream.t -> exp ;;
val parse_exp : string -> exp ;;
val exp_of_string : string -> exp ;;
