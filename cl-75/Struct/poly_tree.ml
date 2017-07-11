#open "prelude";;
#open "lexer";;

#infix "o";;

let int_parser  =
 let rec parser =
  function [< 'INT n >]  ->  n 
 in parser;;

let parse_int s = int_parser (lexer (stream_of_string s));;
let int_of_string = parse_int;;

let string_parser  =
 let rec parser =
  function [< 'IDENT id >]  ->  id 
 in parser;;

let parse_string s = string_parser (lexer (stream_of_string s));;
let string_of_string = parse_string;;

let string_of_stream = sos 0
where rec sos len str =
  try let (c,str') = stream_get str
      in if mem c [`(`;`,`;`)`]
          then sub_string ident_buf 0 len
          else (set_nth_char ident_buf len c; 
          match str
          with [<'c; (sos (succ len) ) r>] -> r)
  with Parse_failure  -> sub_string ident_buf 0 len;;

let tree_parser  parse =
 let rec parser =
  function [< '`(`; parser  a1;'`,`; 
           parser  a2;'`)` >]
                      -> Node(a1,a2)
       |   [< (parse o string_of_stream) n >]  ->  Leaf n
 in parser;;

let parse_tree parse = (tree_parser parse) o stream_of_string;;
let tree_of_string= parse_tree;;
