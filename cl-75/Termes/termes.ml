#open "prelude";;
#open "lexer";;
#infix "o";;

let term_parser =
 let rec parser =
       function  [< 'IDENT x; (rparser x) t>] -> t 
     and rparser x =
        function [<'LPAR; (list_parser [])  l; 'RPAR>]
                             -> Term(x,rev l)
          |      [<>]        -> Var x
     and list_parser l =
        function [<parser t; (rlist_parser (t::l))  l'>]
                             -> l'
          |      [<>]        -> l
     and rlist_parser l =
        function [< 'COMMA; parser t; (rlist_parser (t::l))  l'>]
                             -> l'
          |      [<>]        -> l

 in parser;;

let parse_term  = term_parser o lexer o stream_of_string;;
let term_of_string = parse_term;;
  
let rec string_of_term =
 function Term (x,[])  ->     x ^  "()"
   |      Term (x,l)   ->     x ^  "(" ^ string_of_term_list  l ^  ")"
   |      Var x        ->     x
and string_of_term_list  =
 function   [t]  -> string_of_term t
   |   (t::l)    ->  string_of_term t ^ "," ^ string_of_term_list l
   |     _       -> failwith "string_of_term_list"
;;

let rec print_term =
 function Term (x,[])  ->     print_string (x ^  "()")
   |      Term (x,l)   ->     begin
                              print_string x;
                              print_string "(";
                              print_term_list  l;
                              print_string  ")"
                              end
   |      Var x        ->     print_string x
and print_term_list  =
 function   [t]  -> print_term t
   |   (t::l)    ->  begin
                     print_term t;
                     print_string ",";
                     print_term_list l
                     end
   |     _       -> failwith "print_term_list"
;;

(* +term_trav+ *)

let rec term_trav f g start v = fun
  (Term(oper,sons)) -> f(oper, list_it (g o (term_trav f g start v)) sons start)
| (Var n) -> v n;;
(* +term_trav+ *)

(* +vars+ *)
let vars t = term_trav snd union [] (fun x -> [x]) t;;
(* +vars+ *)

(* +occurs+ *)
let occurs v t = mem v (vars t);;
(* +occurs+ *)

let print_newline = io__print_newline;;

(* +print_subst+ *)

let print_subst subst =
  do_list (fun (x,t) -> print_string x;
                        print_string "  -->  ";
                        print_term t;
                        print_newline())
    subst;;
(* +print_subst+ *)

let print_newline = prelude__print_newline;;

(* +apply_subst+ *)

let rec apply_subst subst =fun
  (Term (f,tl)) -> Term(f,(map (apply_subst subst) tl))
| (Var x as v) -> try assoc x subst
                  with _ -> v;;
(* +apply_subst+ *)



(* +compsubst+ *)
let compsubst subst1 subst2 = 
   (map (fun (v,t) -> (v,apply_subst subst1 t)) subst2) 
  @(let vs= map fst subst2
    in filter (fun (x,t) -> not(mem x vs)) subst1);;
(* +compsubst+ *)

(* +compsubst2+ *)
let compsubst subst1 subst2 = 
   (map (fun (v,t) -> (v,apply_subst subst1 t)) subst2) @subst1;;
(* +compsubst2+ *)

(* +som_subst+ *)
exception Match_exc;;
let som_subst s1 s2 =
  it_list (fun subst (x,t) -> try let u = assoc x subst in
                                  if t=u then subst
                                  else raise Match_exc
                              with Not_found -> (x,t)::subst)
           s1 s2;;
(* +som_subst+ *)
(* +matching+ *)

let matching (t1,t2) =
  matchrec [] (t1,t2)
  where rec matchrec subst =fun
    (Var v,t) -> som_subst [v,t] subst
  | (t,Var v) -> raise Match_exc
  | (Term(op1,sons1),Term(op2,sons2)) ->
      if op1 = op2 then it_list matchrec subst (combine(sons1,sons2))
      else raise Match_exc;;
(* +matching+ *)

(* +unify+ *)
exception Unify_exc;;
let rec unify =fun
  (Var v,t2) -> if Var v = t2 then [] else
                if occurs v t2 then raise Unify_exc
                else [v,t2]
| (t1,Var v)-> if occurs v t1 then raise Unify_exc
               else [v,t1]
| (Term(op1,sons1),Term(op2,sons2)) ->
    if op1 = op2 then
      (it_list subst_unif [] (combine(sons1,sons2))
       where subst_unif s (t1,t2) =
         compsubst (unify (apply_subst s t1,apply_subst s t2)) s)
    else raise Unify_exc;;
(* +unify+ *)

(* +unify_list+ *)

let unify_list tl =
  it_list subst_unif [] tl
  where subst_unif s (t1,t2) =
    compsubst (unify (apply_subst s t1,apply_subst s t2)) s;;
(* +unify_list+ *)
