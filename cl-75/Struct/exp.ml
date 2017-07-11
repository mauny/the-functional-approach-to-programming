#open "lexer";;

let exp_parser =
  let rec atom =
    function [< 'INT n >] -> Constant n
       |     [< 'IDENT s >]  ->  Variable s
       |     [<'LPAR; exp e ; 'RPAR >]  -> e
   and mult =
      let rec restmult e1 =
          function [<'MULT; atom e2; 
	             (restmult (Multiplication(e1,e2))) e >]
		   -> e
	      |    [<>] -> e1
      in function [<atom e1; (restmult e1) e2>] -> e2
   and exp =
      let rec restexp e1 =
          function [<'PLUS; atom e2; 
	             (restexp (Addition(e1,e2))) e >]
		   -> e
	      |    [<>] -> e1
      in function [<mult e1; (restexp e1) e2>] -> e2
in exp;;


let parse_exp s = exp_parser (lexer (stream_of_string s));;
let exp_of_string = parse_exp;;
