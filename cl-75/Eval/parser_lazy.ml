#open "ml_ops";;
#open "ml_lazy";;
#open "lexer";;

let constr_of_string = fun "Nil" -> Nil 
                        | "Cons" -> Cons
			|   _    -> raise (Failure "Wrong Constructor");;


let ml_parser =
  let rec atom =
    function [< 'INT n >] -> Ml_int_const n
       |     [< 'BOOL b >] -> Ml_bool_const b
       |     [< 'IDENT s >]  ->  Ml_var s
       |     [< 'CONSTR0 s>] -> Ml_constr0 (constr_of_string s)
       |     [<'LPAR; exp e ; 'RPAR >]  -> e
   and tuple =
      let rec resttuple el =
       function [<'COMMA; exp_in_tuple e; (resttuple (e::el)) el' >]
                       ->  el'
	|	[<'RPAR>]  -> el
      in function [<'LPAR; exp_in_tuple e; (resttuple [e]) el>] -> el
   and app =
      let rec restapp e1 =
       function [< atom e2; (restapp (match e1 with
                                         Ml_var "fst" -> Ml_unop(Ml_fst,e2)
                                       | Ml_var "snd" -> Ml_unop(Ml_snd,e2)
                                       | e1 -> Ml_app (e1,e2))) e >] -> e
          |     [<>]  -> e1
      in function [< atom e1; (restapp e1) e >] -> e
             |    [< 'CONSTRN s; tuple el>]
	             -> Ml_capp (constr_of_string s, rev el)

   and mult =
      let rec restmult e1 =
          function 
	        [<'MULT; atom e2; 
	             (restmult (Ml_binop(Ml_mult,e1,e2))) e >]
		   -> e
	      |    [<>] -> e1
      in function [<app e1; (restmult e1) e2>] -> e2
   and add =
      let rec restadd  e1 =
          function 
	         [<'PLUS; atom e2; 
	             (restadd (Ml_binop(Ml_add,e1,e2))) e >]
		   -> e
	      |  [<'MINUS; atom e2; 
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
   and restfun =
     function [<'IDENT x; 'ARROW; exp e>]
	                 ->  Ml_fun(x,e)
	|     [<case c; (restcases [c]) cl>]
	                 ->  Ml_func (rev cl)
   and case =
     function [<'LPAR; 'CONSTRN c; vars vl;'RPAR;'ARROW; exp e>]
                    -> (constr_of_string c,vl,e)
	|     [<'CONSTR0 c; 'ARROW; exp e>]
                    -> (constr_of_string c,[],e)
   and restcases cl=
     function [< 'BAR; case c; (restcases (c::cl)) cl'>]
                    -> cl'
	|     [<>]  -> cl
   and vars =
     function [<'LPAR; 'IDENT v; (restvars [v]) vl>]
                    -> vl
   and restvars vl=
     function [<'COMMA; 'IDENT v; (restvars (v::vl)) vl'>]
                    -> vl'
	|     [<'RPAR>]  -> rev vl

   and exp = 
     function [< 'IF; exp e1; 'THEN; exp e2; 'ELSE; exp e3 >]
                         ->  Ml_if(e1,e2,e3)
	 |    [<'FUN ; restfun e>] -> e
	 |    [<'LET; restlet e>]   ->  e
	 |    [<pair e>]  -> e
   and exp_in_tuple  = 
     function [< 'IF; exp e1; 'THEN; exp e2; 'ELSE; exp e3 >]
                         ->  Ml_if(e1,e2,e3)
	 |    [<'FUN ; restfun e>] -> e
	 |    [<'LET; restlet e>]   ->  e
	 |    [<comp e>]  -> e
			 
in exp;;


let parse_ml_exp s = ml_parser (lexer (stream_of_string s));;
let exp_of_string = parse_ml_exp;;
