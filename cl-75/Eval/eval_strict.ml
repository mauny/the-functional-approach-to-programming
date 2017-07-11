#open "lexer";;
#open "ml_ops";;
#open "ml_strict";;
#open "parser_strict";;

(* +ml_eval+ *)

let ml_eval_binop = fun
  Ml_add (Int_Const m) (Int_Const n) -> Int_Const (add_int m n)
| Ml_sub (Int_Const m) (Int_Const n) -> Int_Const (sub_int m n)
| Ml_mult (Int_Const m) (Int_Const n) -> Int_Const (mult_int m n)
| Ml_eq(Int_Const m) (Int_Const n) -> Bool_Const (eq_int m n)
| Ml_less (Int_Const m) (Int_Const n) -> Bool_Const (lt_int m n)
| _ _ _ -> failwith "ml_eval_binop: wrong types";;

let rec ml_eval env = fun
  (Ml_int_const n) -> Int_Const n
| (Ml_bool_const b) -> Bool_Const b
| (Ml_pair (e1,e2)) -> Pair(ml_eval env e1, ml_eval env e2)
| (Ml_unop (op,e)) -> (match (op, ml_eval env e) with
                         (Ml_fst, Pair(v1,v2)) -> v1
                       |( Ml_snd, Pair(v1,v2)) -> v2
                       | _ -> failwith "ml_eval: wrong types")
| (Ml_binop (op, e1, e2)) -> let v2 = ml_eval env e2
                             and v1 = ml_eval env e1
                             in ml_eval_binop op v1 v2
| (Ml_var x) -> (try assoc x env
                 with Not_found -> failwith "unbound variable")
| (Ml_if (c,e1,e2)) -> (match ml_eval env c with
                          (Bool_Const true) -> ml_eval env e1
                        | (Bool_Const false) -> ml_eval env e2
                        | _ -> failwith "ml_eval: wrong types")
| (Ml_fun (x,e) as f) -> Clo(env,f)
| (Ml_app (e1,e2)) -> (match (ml_eval env e1, ml_eval env e2) with
                         (Clo(env',Ml_fun (x,e)),v)
                         -> ml_eval ((x,v)::env') e
                       | _ -> failwith "ml_eval: wrong types")
| (Ml_let (x,e1,e2)) -> let v = ml_eval env e1
                        in ml_eval ((x,v)::env) e2
| (Ml_letrec (f,e1,e2))
    -> (match e1 with
        Ml_fun _ -> let rec env' = (f,Clo(env',e1))::env
                    in ml_eval env' e2
      | _ -> failwith "illegal recursive definition")
;;
(* +ml_eval+ *)

(* +ml_eval_examples+ *)
ml_eval []
  (ml_exp_of_string
    ("let double = fun f -> fun x -> f(f x) in " ^
     "let sq = fun x -> x*x in " ^
     "(double sq) 5"));;

ml_eval []
  (ml_exp_of_string
    ("let rec fact=" ^
     "  fun n -> if n=0 then 1 else n*(fact(n-1)) in " ^
     "fact 10"));;
(* +ml_eval_examples+ *)
