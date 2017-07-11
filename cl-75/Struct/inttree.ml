#open "lexer";;

let inttree_parser  =
 let rec parser =
  function [< 'LPAR; parser  a1;'COMMA; 
           parser  a2;'RPAR >]
                      -> Node(a1,a2)
       |   [< 'INT n >]  ->  Leaf n
 in parser;;


let parse_inttree s = inttree_parser (lexer (stream_of_string s));;
let inttree_of_string = parse_inttree;;

(* +print_inttree+ *)
let rec print_inttree = fun
 (Leaf n)  -> print_int n
| (Node(t1,t2))
     -> print_string "(";    print_inttree t1;    print_string ",";
        print_inttree t2; print_string ")";;
(* +print_inttree+ *)
