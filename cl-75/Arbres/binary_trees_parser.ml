#directory "../Util";;
#open "prelude";;
#open "lexer";;
#open "binary_trees";;

#infix "o";;

let ident_max_length = 48;;
let ident_buf = make_string ident_max_length ` `;;

let int_parser  =
 let rec parser =
  function [< 'INT n >]  ->  n 
 in parser;;

let parse_int s = int_parser (lexer (stream_of_string s));;
let int_of_string=parse_int;;

let string_parser  =
 let rec parser =
  function [< 'IDENT id >]  ->  id 
 in parser;;

let parse_string s = string_parser (lexer (stream_of_string s));;
let parse_ident= parse_string;;
let string_of_ident= parse_string;;
let ident_of_string = parse_string;;

let string_int_parser  =
 let rec parser =
  function [<'LBRACK; 'IDENT id; 'SEMIC; 'INT n; 'RBRACK >]  
     ->  (id , n)
 in parser;;

let parse_string_int s = string_int_parser (lexer (stream_of_string s));;


let string_of_stream = sos 0
where rec sos len str =
  try let (c,str') = stream_get str
      in if mem c [`(`;`,`;`)`]
          then sub_string ident_buf 0 len
          else (set_nth_char ident_buf len c; 
          match str
          with [<'c; (sos (succ len) ) r>] -> r)
  with Parse_failure  -> sub_string ident_buf 0 len;;



let btree_parser  parse =
 let rec parser =
       function  [<'`(`;'`)`>]  -> Empty
          |      [<(parse o string_of_stream) e; (rparser e) t>] -> t
     and rparser x =
        function [<'`(`; parser t1; '`,`; parser t2; '`)`>]
                             -> Bin(t1,x,t2)
          |      [<>]        -> Bin(Empty,x,Empty)

 in parser;;

let parse_btree parse  
  = (btree_parser parse) o stream_of_string;;
let btree_of_string= parse_btree;;

  
let rec print_btree (print:'a -> unit) =
 function    Empty   ->     print_string "()"
   |  Bin(t1,x,t2)
                   ->     print x;
                          if t1<>Empty or t2 <>Empty
                            then ( print_string "(";
                                  print_btree print t1;
                                  print_string ",";
                                  print_btree print t2;
                                  print_string ")");;


