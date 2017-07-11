#open "lexer";;
#open "ml_ops";;
#open "ml_lazy";;
#open "parser_lazy";;

(* +select_combine+ *)
let rec select p = fun
 (a::l) -> if p a then a else select p l
| _ -> failwith "select";;

let rec combine = fun
  ([],[]) -> []
| (a::l,a'::l') -> (a,a')::combine(l,l')
| _ -> failwith "combine";;
(* +select_combine+ *)

(* +ml_eval_binop+ *)
let ml_eval_binop = fun
     Ml_add (Int_Const m) (Int_Const n) -> Int_Const (add_int m n)
  |  Ml_sub (Int_Const m) (Int_Const n) -> Int_Const (sub_int m n)
  |  Ml_mult (Int_Const m) (Int_Const n) -> Int_Const (mult_int m n)
  |  Ml_eq(Int_Const m) (Int_Const n) -> Bool_Const (eq_int m n)
  |  Ml_less (Int_Const m) (Int_Const n) -> Bool_Const (lt_int m n)
| _ _ _ -> failwith "ml_eval_binop: wrong types";;
(* +ml_eval_binop+ *)
(* +ml_eval+ *)

let rec ml_eval env = fun
  (Ml_int_const n) -> Int_Const n
| (Ml_bool_const b) -> Bool_Const b
| (Ml_pair (e1,e2)) -> Pair(Fre(env,e1), Fre(env,e2))
| (Ml_unop (op,e)) ->(match (op, ml_eval env e) with
                        (Ml_fst, Pair(v1,v2)) -> unfreeze v1
                      | (Ml_snd, Pair(v1,v2)) -> unfreeze v2
                      | _ -> failwith "ml_eval: wrong types")
| (Ml_binop (op, e1, e2)) -> let v1 = ml_eval env e1
                             and v2 = ml_eval env e2 in
                             (ml_eval_binop op) v1 v2
| (Ml_var x) -> unfreeze(assoc x env)
| (Ml_constr0 c) -> Constr0 c
| (Ml_if (c,e1,e2)) -> (match ml_eval env c with
                          (Bool_Const true) -> ml_eval env e1
                        | (Bool_Const false) -> ml_eval env e2
                        | _ -> failwith "ml_eval: wrong types")
| (Ml_fun _ as f) -> Clo(env,f)
| (Ml_func _ as f) -> Clo(env,f)
| (Ml_app (e1,e2))
  -> (match ml_eval env e1 with
        (Clo(env',Ml_fun (x,e)))
        -> ml_eval ((x,Fre(env,e2))::env') e
      | (Clo(env',Ml_func case_list))
        -> let (c,vl) = match ml_eval env e2 with
             (Constr_n(c,vl)) -> c,vl
           | (Constr0 c) -> c,[]
           | _ -> failwith "ml_eval: wrong types" in
           let (c',sl,e) =
             select (fun (c',sl,e) -> (c'=c)) case_list
           in ml_eval (combine(sl,vl)@env') e
      | _ -> failwith "ml_eval: wrong types")
| (Ml_capp (c,el)) -> Constr_n(c, map (fun e -> Fre(env,e)) el)
| (Ml_let (x,e1,e2)) -> ml_eval ((x,Fre(env,e1))::env) e2
| (Ml_letrec (f,e1,e2)) -> let rec env' = (f,Fre(env',e1))::env in
                           ml_eval env' e2
and unfreeze = fun
  (Fre(env',e)) -> ml_eval env' e
| v -> v;;
(* +ml_eval+ *)

(* +eval_examples1+ *)
ml_eval []
  (exp_of_string
    ("let rec fact = " ^
     "  fun n -> if n=0 then 1 else n*(fact(n-1)) in " ^
     "fact 10"));;
ml_eval []
  (exp_of_string
    ("let rec fact = " ^
     "  fun n -> if n=0 then 1 else n*(fact(n-1)) in " ^
     "(fun x -> 1) (fact 100000)"));;
ml_eval []
  (exp_of_string
    ("let rec fact = " ^
     "  fun n -> if n=0 then 1 else n*(fact(n-1)) in " ^
     "(fst(1,fact 100000))"));;
ml_eval []
  (exp_of_string
    ("let rec boucle =" ^
     "  fun n -> boucle (n+1) in " ^
     "(fun x -> 1) (boucle 1)"));;
(* +eval_examples1+ *)


(* +eval_examples2+ *)
let rec nfirst = fun
     0 -> (fun _ -> [])
  |  n -> (fun (Constr_n (Cons,[x;l]))
                -> (unfreeze x)::nfirst (n-1) (unfreeze l)
            | _ -> failwith "nfirst: wrong data");;
		
nfirst 20 
(ml_eval [] 
  (exp_of_string
    ("let hd = fun (Cons(x,l)) -> x in " ^
     "let tl = fun (Cons(x,l)) -> l in " ^
     "let rec makelist=" ^
     "    fun f -> fun x -> " ^
     "               Cons(x,makelist f (f x)) in " ^
     "let rec merge = " ^
     "    fun l -> fun ll ->" ^
     "          if (hd l) < (hd ll) then " ^
     "             Cons(hd l,merge(tl l) ll) else " ^
     "          if (hd l) = (hd ll) then " ^
     "             merge(tl l) ll " ^
     "          else Cons(hd ll,merge l (tl ll)) " ^
     "in merge (makelist (fun x -> x+3) 3)" ^
     "         (makelist (fun x -> x+7) 7)")));;
(* +eval_examples2+ *)
