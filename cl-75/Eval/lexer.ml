let rec spaces =
  function  [< '` `|`\t`|`\n`; spaces _>]  -> ()
      |     [<>]   -> ();;
   
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
   
let int_of_digit =
  function  `0`..`9` as c   -> (int_of_char c)-(int_of_char `0`)
      |      _  ->  failwith "Not a Digit";;
let rec integer n =
  function [< ' `0`..`9` as c; (integer (10*n+int_of_digit  c)) r>]
           ->  r
      |    [<>]  -> INT n;;
let ident_max_length = 48;;
let ident_buf = make_string ident_max_length ` `;;
let constructors0 = ["Nil"]
and constructorsn = ["Cons"];;
let rec ident len =
  function [< ' `a`..`z` as c; 
              (if len >= ident_max_length
	         then ident len
		 else (set_nth_char ident_buf len c;
		      ident (succ len))) r>]
           -> r
      |  [< ' `A`..`Z` as c; 
              (if len >= ident_max_length
	         then ident len
		 else (set_nth_char ident_buf len c;
		      ident (succ len))) r>]
           -> r

      |    [<>]  -> match (sub_string ident_buf 0 len)
                    with "if" -> IF
		      |  "then" -> THEN
		      |  "else" -> ELSE
		      |  "fun"  -> FUN
		      |  "let"  -> LET
		      |  "rec"  -> REC
		      |  "in"   -> IN
		      |  "true" -> BOOL true
		      |  "false" -> BOOL false
		      |    s    -> if mem s constructors0
		                    then CONSTR0 s
				    else if mem s constructorsn
				           then  CONSTRN s
					   else  IDENT s;;
let rec lexer str =
  spaces str;
  let restsub =
    function  [<'`>`>] -> [< 'ARROW; lexer str>]
        |     [<>]   -> [< 'MINUS; lexer str>] 
  in
  match str
  with [<'`(`;spaces _>]  -> [< 'LPAR; lexer str>]
   |   [<'`)`;spaces _>]  -> [< 'RPAR; lexer str>]
   |   [<'`[`;spaces _>]  -> [< 'LBRACK; lexer str>]
   |   [<'`]`;spaces _>]  -> [< 'RBRACK; lexer str>]
   |   [<'`{`;spaces _>]  -> [< 'LBRACE; lexer str>]
   |   [<'`}`;spaces _>]  -> [< 'RBRACE; lexer str>]
   |   [<'`,`;spaces _>]  -> [< 'COMMA; lexer str>]
   |   [<'`.`;spaces _>]  -> [< 'DOT; lexer str>]
   |   [<'`;`;spaces _>]  -> [< 'SEMIC; lexer str>]
   |   [<'`:`;spaces _>]  -> [< 'COLON; lexer str>]
   |   [<'`|`;spaces _>]  -> [< 'BAR; lexer str>]
   |   [<'`+`;spaces _>]  -> [< 'PLUS; lexer str>]
   |   [<'`-`>]  ->  restsub str
   
   |   [<'`*`;spaces _>]  -> [< 'MULT; lexer str>]
   |   [<'`/`;spaces _>]  -> [< 'DIV; lexer str>]
   |   [<'`=`;spaces _>]  -> [< 'EQ; lexer str>]
   |   [<'`<`;spaces _>]  -> [< 'LT; lexer str>]
   |   [<' `0`..`9` as c;
         (integer (int_of_digit c)) tok;
	 spaces _>]        -> [<'tok; lexer str >]
   |   [< ' `a`..`z` as c;
          (set_nth_char ident_buf 0 c; ident 1) tok;
          spaces _>]       -> [<'tok; lexer str >]
   |   [< ' `A`..`Z` as c;
          (set_nth_char ident_buf 0 c; ident 1) tok;
          spaces _>]       -> [<'tok; lexer str >]
	  ;;
let lex s = lexer (stream_of_string s);;
