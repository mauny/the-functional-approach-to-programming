#open "prelude";;
#open "lexer";;
#infix "o";;

let ident_max_length = 48;;
let ident_buf = make_string ident_max_length ` `;;

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
let parse_ident= parse_string;;
let string_of_string= parse_string;;
let string_of_ident= parse_string;;


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



let gentree_parser  parse =
 let rec parser =
       function  [<(parse o string_of_stream) e; 
                   rparser tl>] -> GenNode(e,rev tl)
     and rparser =
        function [<'`(`; (list_parser [])  l; '`)`>]
                             -> l
          |      [<>]        -> []
     and list_parser l =
        function [<parser t; (rlist_parser (t::l))  l'>]
                             -> l'
          |      [<>]        -> l
     and rlist_parser l =
        function [< '`,`; parser t; (rlist_parser (t::l))  l'>]
                             -> l'
          |      [<>]        -> l

 in parser;;

let parse_gentree parse  
  = (gentree_parser parse) o stream_of_string;;
let gentree_of_string = parse_gentree;;

  
let rec print_gentree print =
 function GenNode (x,[])  ->     print x
   |      GenNode (x,l)   ->     print x; print_string "(";
                                 print_gentree_list print l;
                                 print_string ")"
and print_gentree_list print =
 function   []  ->  ()
   |  (t::l)    ->  print_gentree print t;
                    do_list (fun t -> print_string ",";
                                      print_gentree print t)
                             l;;

let string_of_gentree= print_gentree;;

