
(* Le langage considere ainsi que celui des types est simplifie *)

(*+ALL+*)
(* +new_int+ *)
let new_int, reset_new_int =
let c= ref (-1) in
 (fun () -> c:=!c+1 ; !c),
 (fun () -> c:=-1);;
(* +new_int+ *)

(* +ml_exp+ *)
type ml_exp =
    Ml_int_const of int                         (*> constante entière *)
 |  Ml_var  of string                           (*> variable *)
 |  Ml_fun  of string * ml_exp                  (*> fonction *)
 |  Ml_app  of ml_exp * ml_exp                  (*> application *)
 |  Ml_let of string * ml_exp * ml_exp          (*> déclaration *)
;;
(* +ml_exp+ *)

(* Analyse syntaxique du langage *)

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

let ml_parser =
  let rec atom =
    function [< 'INT n >] -> Ml_int_const n
       |     [< 'IDENT s >]  ->  Ml_var s
       |     [<'LPAR; exp e ; 'RPAR >]  -> e
   and app =
      let rec restapp e1 =
       function [< atom e2; (restapp (Ml_app (e1,e2))) e >] -> e
          |     [<>]  -> e1
      in function [< atom e1; (restapp e1) e >] -> e
   and exp = 
     function [<'FUN ; 'IDENT x; 'ARROW; exp e>]
	                 ->  Ml_fun(x,e)
	 |    [<'LET; 'IDENT x; 'EQ; exp e1; 'IN; exp e2 >]
	                 ->  Ml_let(x,e1,e2)
	 |    [< app e>]  -> e
in exp;;
let parse_ml_exp s = ml_parser (lexer (stream_of_string s));;
let ml_exp_of_string = parse_ml_exp;;

(* +ml_type+ *)
type ml_type =
   Unknown            (*>. Champ {\mlid Val} d'une variable de type *)
 | Int_type           
 | Var_type of vartype
 | Arrow_type of ml_type * ml_type
and vartype = {Idx: int; mutable Val: ml_type}
;;
(* +ml_type+ *)

(* Impression des types *)
let var_of_int n = "v"^(string_of_int n);;
let rec print_type = function
  Int_type -> print_string "int"
| Var_type{Idx=n; Val=Unknown} ->
    print_string (var_of_int n)
| Var_type{Idx=_; Val=t} -> print_type t
| Arrow_type(t1,t2) ->
    print_string "("; print_type t1;
    print_string " ml_type_infix_arrow "; print_type t2;
    print_string ")"
| Unknown -> failwith "print_type" (*> Ne doit jamais se produire *)
;;

let print_ml_type t =
 print_type t
;;


(* +scheme+ *)
type 'a scheme = Forall of int list * 'a;;
(* +scheme+ *)

(*+shorten+*)
let rec shorten t = match t with
     Var_type {Idx=_; Val=Unknown} -> t
   | Var_type ({Idx=_; Val = Var_type {Idx=_; Val=Unknown} as tv}) -> tv
   | Var_type ({Idx=_; Val = Var_type tv1} as tv2)
            -> (tv2.Val <- tv1.Val); shorten t
   | Var_type {Idx=_; Val=t'} -> t'
   | Unknown -> failwith "shorten"
   | t' -> t';;
(*+shorten+*)


(*+occurs+*)
let occurs {Idx=n;Val=_} =
  let rec occrec = function
        Var_type{Idx=m;Val=_} -> (n=m)
      | Int_type -> false
      | Arrow_type(t1,t2) -> (occrec t1) or (occrec t2)
      | Unknown -> failwith "occurs" (*> Ne doit jamais se produire *)
  in occrec
;;
(*+occurs+*)


(*+unify+*)
let rec unify (t1,t2) =  match (shorten t1, shorten t2) with
 (Var_type({Idx=n; Val=Unknown} as tv1),
        (Var_type {Idx=m; Val=Unknown} as t2)) (*>. variables {\mlid n} et {\mlid m} *)
       -> if n <> m then tv1.Val <- t2 else ()
     | (t1, Var_type ({Idx=_;Val=Unknown} as tv)) (* > type et variable *)
       -> if not(occurs tv t1) then tv.Val <- t1
          else failwith "unify"
     | ((Var_type {Idx=_;Val=Unknown} as t1), t2) (* > variable et type *)
       -> unify(t2,t1)
     | (Int_type, Int_type) -> ()
     | (Arrow_type(t1,t2), Arrow_type(t'1,t'2))
       -> unify(t1,t'1); unify(t2,t'2)
     | (_,_) -> failwith "unify"
;;
(*+unify+*)

(*+vars_of_type+*)
let vars_of_type tau =
 let rec vars vs = function
       Int_type -> vs
     | Var_type {Idx=n; Val=Unknown}
                 -> if mem n vs then vs else n::vs
     | Var_type {Idx=_; Val= t} -> vars vs t
     | Arrow_type(t1,t2) -> vars (vars vs t1) t2
     | Unknown -> failwith "vars_of_type" (*> Ne doit jamais se produire *)
  in vars [] tau
;;
(*+vars_of_type+*)


let flat = it_list (prefix @) [];;

(*+vars_of_ty_env+*)
let vars_of_ty_env env =
    flat (map (fun(_, Forall(gvars,t)) -> subtract (vars_of_type t) gvars) env);;
(*+vars_of_ty_env+*)


(* +unique+ *)

let rec unique = function
  [] -> []
| x::l -> if mem x l then unique l else x :: unique l;;

(* +unique+ *)

(* +generalize+ *)

let generalize tenv t =
  let genvars =
        unique (subtract (vars_of_type t)
                           (vars_of_ty_env tenv))
  in Forall(genvars, t)
;;

(* +generalize+ *)
(* +instance+ *)

let instance = function
  Forall([], t) -> t (*> Aucune variable générique *)
| Forall(gvars,t) ->
    let new_vars =
    (* On associe une nouvelle variable à chaque variable générique *)
      map (fun n -> n, Var_type({Idx=new_int(); Val=Unknown})) gvars in
    let rec inst = function
    (* et on effectue le remplacement dans le corps du schéma de type *)
      (Var_type {Idx=n; Val=Unknown} as t) ->
        (try assoc n new_vars
         with Not_found -> t)
    | Var_type {Idx=_; Val= t} -> inst t
    | Int_type -> Int_type
    | Arrow_type(t1,t2) -> Arrow_type(inst t1, inst t2)
    | Unknown -> failwith "instance" (* > Ne doit jamais se produire *)
    in inst t
;;

(* +instance+ *)

(*+type_expr+*)
let rec type_expr tenv = function
      Ml_int_const _ -> Int_type
    | Ml_var s ->
        let ts =
          try assoc s tenv
          with Not_found -> failwith "Unbound variable"
        in instance ts
    | Ml_let(s,e1,e2) ->
        let t1 = type_expr tenv e1 in
        let ts = generalize tenv t1
        in type_expr ((s,ts)::tenv) e2
    | Ml_app(e1,e2) ->
        let u = Var_type {Idx=new_int(); Val=Unknown}
        in unify(type_expr tenv e1,Arrow_type(type_expr tenv e2,u)); u
    | Ml_fun(x,e) ->
        let alpha = Var_type {Idx=new_int(); Val=Unknown} in
        let ts = Forall([], alpha)
        in Arrow_type(alpha, type_expr ((x,ts)::tenv) e)
;;
(*+type_expr+*)

(*+type_of+*)
let type_of e =
  reset_new_int ();
  type_expr [] e
;;
(*+type_of+*)

  

(*+ALL+*)

