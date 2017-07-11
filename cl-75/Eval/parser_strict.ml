#open "ml_ops";;
#open "lexer";;
#open "ml_strict";;

let ml_parser =
  let rec atom =
    function [< 'INT n >] -> Ml_int_const n
       |     [< 'BOOL b >] -> Ml_bool_const b
       |     [< 'IDENT s >]  ->  Ml_var s
       |     [<'LPAR; exp e ; 'RPAR >]  -> e
   and app =
      let rec restapp e1 =
       function [< atom e2; (restapp (match e1 with
                                         Ml_var "fst" -> Ml_unop(Ml_fst,e2)
                                       | Ml_var "snd" -> Ml_unop(Ml_snd,e2)
                                       | e1 -> Ml_app (e1,e2))) e >] -> e
          |     [<>]  -> e1
      in function [< atom e1; (restapp e1) e >] -> e
   and mult =
      let rec restmult e1 =
          function 
	        [<'MULT; app e2; 
	             (restmult (Ml_binop(Ml_mult,e1,e2))) e >]
		   -> e
	      |    [<>] -> e1
      in function [<app e1; (restmult e1) e2>] -> e2
   and add =
      let rec restadd  e1 =
          function 
	         [<'PLUS; mult e2; 
	             (restadd (Ml_binop(Ml_add,e1,e2))) e >]
		   -> e
	      |  [<'MINUS; mult e2; 
	             (restadd (Ml_binop(Ml_sub,e1,e2))) e >]
		   -> e
	      |    [<>] -> e1
      in function [<mult e1; (restadd e1) e2>] -> e2
   and comp =
      let rec restcomp  e1 =
          function 
	         [<'EQ; atom e2; 
	             (restcomp (Ml_binop(Ml_eq,e1,e2))) e >]
		   -> e
	      |   [<'LT; atom e2; 
	             (restcomp (Ml_binop(Ml_less,e1,e2))) e >]
		   -> e
	      |    [<>] -> e1
      in function [<add e1; (restcomp e1) e2>] -> e2
   and pair = 
      let rec restpair e1 =
         function [<'COMMA; comp e2>] -> Ml_pair(e1,e2)
            |     [<>]  -> e1
      in function [<comp e1;(restpair e1) e>] -> e
   and restlet =
     function [<'IDENT x; 'EQ; exp e1; 'IN; exp e2 >]
	                 ->  Ml_let(x,e1,e2)
	 |    [<'REC; 'IDENT x; 'EQ; exp e1; 'IN; exp e2 >]
	                 ->  Ml_letrec (x,e1,e2)
   and exp = 
     function [< 'IF; exp e1; 'THEN; exp e2; 'ELSE; exp e3 >]
                         ->  Ml_if(e1,e2,e3)
	 |    [<'FUN ; 'IDENT x; 'ARROW; exp e>]
	                 ->  Ml_fun(x,e)
	 |    [<'LET; restlet e>]   ->  e
	 |    [<pair e>]  -> e
in exp;;
let parse_ml_exp s = ml_parser (lexer (stream_of_string s));;
let ml_exp_of_string = parse_ml_exp;;


(* +ml_exp_printer+ *)
let string_of_unop = fun
  Ml_fst -> "fst"
| Ml_snd -> "snd";;
let string_of_binop = fun
  Ml_add -> "+"
| Ml_sub -> "-"
| Ml_mult -> "*"
| Ml_eq -> "="
| Ml_less -> "<";;
let rec print_ml_exp = function
  Ml_int_const n  ->  print_int n
| Ml_bool_const true -> print_string "true"
| Ml_bool_const false -> print_string "false"
| Ml_unop (op,e) ->      print_string (string_of_unop op);
                         print_string "(";
                         print_ml_exp e;
                         print_string ")"
| Ml_binop (op,e1,e2) -> print_ml_exp e1;
                         print_string (string_of_binop op);
                         print_ml_exp e2
| Ml_pair(e1,e2)  ->     print_string "(";
                         print_ml_exp e1;
                         print_string ",";
                         print_ml_exp e2;
                         print_string ")"
| Ml_var s    -> print_string s
| Ml_if (e1,e2,e3) -> print_string "if ";
                      print_ml_exp e1;
                      print_string " then ";
                      print_ml_exp e2;
                      print_string " else ";
                      print_ml_exp e3
| Ml_fun (x,e)  ->    print_string "(fun ";
                      print_string x;
                      print_string " -> ";
                      print_ml_exp e;
                      print_string ")"
| Ml_app (e1,e2) ->   print_ml_exp e1;
                      print_string " ";
                      print_ml_exp e2
| Ml_let (x,e1,e2) -> print_string "let ";
                      print_string x;
                      print_string " = ";
                      print_ml_exp e1;
                      print_string " in ";
                      print_ml_exp e2
| Ml_letrec (x,e1,e2) -> print_string "let rec ";
                      print_string x;
                      print_string " = ";
                      print_ml_exp e1;
                      print_string " in ";
                      print_ml_exp e2;;
(* +ml_exp_printer+ *)

