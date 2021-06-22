(* #use "load.ml" ;; *)

type token = LPAR | RPAR | COMMA | DOT | SEMIC | COLON
           | LBRACK | RBRACK | LBRACE | RBRACE
           | EQ | LT | LE | GT | GE
           | PLUS | MINUS | MULT | DIV
           | STAR | DOLLAR | SHARP | UNDERSCORE | CARET | BAR
           | IF | THEN | ELSE | FUN | ARROW
           | LET | REC | IN
           | INT of int
           | BOOL of bool
           | CONSTR0 of string
           | CONSTRN of string
           | IDENT of string ;;

let rec spaces =
  parser
| [< ' (' ' | '\t' | '\n'); _ = spaces >] -> ()
| [< >] -> () ;;

let int_of_digit = function
  | '0'..'9' as c -> (int_of_char c) - (int_of_char '0')
  | _ -> failwith "Not a Digit"  ;;

let rec integer n =
  parser
| [< ' ('0' .. '9' as c);
     r = (integer (10 * n + int_of_digit c)) >] -> r
| [< >] -> INT n ;;

let ident_max_length = 48 ;;

let ident_buf = Bytes.make ident_max_length ' ' ;;

let constructors0 = ["Nil"]
and constructorsn = ["Cons"] ;;

let rec ident len =
  parser
| [< ' ('a' .. 'z' as c);
     r = (if len >= ident_max_length then ident len
          else (Bytes.set ident_buf len c;
                ident (succ len))) >] -> r
| [< ' ('A' .. 'Z' as c);
     r = (if len >= ident_max_length then ident len
          else (Bytes.set ident_buf len c;
                ident (succ len))) >] -> r
| [< >] ->
  match (String.sub (Bytes.to_string ident_buf) 0 len) with
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "fun" -> FUN
  | "let" -> LET
  | "rec" -> REC
  | "in" -> IN
  | "true" -> BOOL true
  | "false" -> BOOL false
  | s -> if List.mem s constructors0 then CONSTR0 s
    else
    if List.mem s constructorsn then CONSTRN s
    else IDENT s ;;

let rec lexer str =
  spaces str;
  let restsub =
    parser
  | [< ''>' >] -> [< 'ARROW; lexer str >]
  | [< >] -> [< 'MINUS; lexer str >]
  in
  match str with
    parser
  | [< ''('; _ = spaces >] -> [< 'LPAR; lexer str >]
  | [< '')'; _ = spaces >] -> [< 'RPAR; lexer str >]
  | [< ''['; _ = spaces >] -> [< 'LBRACK; lexer str >]
  | [< '']'; _ = spaces >] -> [< 'RBRACK; lexer str >]
  | [< ''{'; _ = spaces >] -> [< 'LBRACE; lexer str >]
  | [< ''}'; _ = spaces >] -> [< 'RBRACE; lexer str >]
                                                | [< '','; _ = spaces >] -> [< 'COMMA; lexer str >]
                                                | [< ''.'; _ = spaces >] -> [< 'DOT; lexer str >]
                                                | [< '';'; _ = spaces >] -> [< 'SEMIC; lexer str >]
                                                | [< '':'; _ = spaces >] -> [< 'COLON; lexer str >]
                                                | [< ''|'; _ = spaces >] -> [< 'BAR; lexer str >]
                                                | [< ''+'; _ = spaces >] -> [< 'PLUS; lexer str >]
                                                | [< ''-'>] -> restsub str
                                                | [< ''*'; _ = spaces >] -> [< 'MULT; lexer str >]
                                                | [< ''/'; _ = spaces >] -> [< 'DIV; lexer str >]
                                                | [< ''='; _ = spaces >] -> [< 'EQ; lexer str >]
                                                | [< ''<'; _ = spaces >] -> [< 'LT; lexer str >]
                                                | [< ''>'; _ = spaces >] -> [< 'GT; lexer str >]  
                                                | [< ' ('0' .. '9' as c); tok = (integer (int_of_digit c)); _ = spaces >] -> [< 'tok; lexer str >]
                                                | [< ' ('a' .. 'z' as c); tok = (Bytes.set ident_buf 0 c; ident 1) ; _ = spaces >] -> [< 'tok; lexer str >]
                                                | [< ' ('A' .. 'Z' as c); tok = (Bytes.set ident_buf 0 c; ident 1) ; _ = spaces >] -> [< 'tok; lexer str >] ;;

let lex s = lexer (Stream.of_string s) ;;
