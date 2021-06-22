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
	   | IDENT of string;;


val ident_buf : bytes;;
val lexer: char Stream.t -> token Stream.t;;
