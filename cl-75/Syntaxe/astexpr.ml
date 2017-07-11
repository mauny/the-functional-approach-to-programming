(* +token+ *)

type token =
       NUM of float | IDENT of string
     | LPAR | RPAR
     | PLUS | MINUS | STAR | SLASH
;;

(* +token+ *)
(* +ast+ *)

type ast =
       Constant of float
     | Variable of string
     | Addition of ast * ast
     | Subtraction of ast * ast
     | Multiplication of ast * ast
     | Division of ast * ast
;;

(* +ast+ *)


