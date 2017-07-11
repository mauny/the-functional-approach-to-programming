#open "lexer";;
#open "ml_exp1";;
#open "ml1_parser";;
#open "code_simulator";;

(* +compile_binop_var+ *)
let compile_unop = fun
    Ml_fst -> FST
|   Ml_snd -> SND;;

let compile_binop = fun
      Ml_add -> ADD
   |  Ml_sub -> SUB
   |  Ml_mult -> MULT
   |  Ml_eq -> EGAL
   |  _ -> failwith "compile_binop: not implemented";;

exception compile_error of string;;
let rec compile_var env v =
 match env
 with [] ->  raise(compile_error "unbound variable")
   |  (x::env) -> if x=v then [FST]
                         else SND::(compile_var env v);;
(* +compile_binop_var+ *)


(* +compile+ *)
let compile e =
  let rec comp env = fun
        (Ml_int_const n) -> [LOAD (Int_Const n)]
     |  (Ml_bool_const b) -> [LOAD (Bool_Const b)]
     |  (Ml_pair(e1,e2)) -> [DUPL]@(comp env e2)@[SWAP]
                           @(comp env e1)@[CONS]
     |  (Ml_unop(op,e)) -> (comp env e)@[compile_unop op]
     |  (Ml_binop(op,e1,e2))
          -> [DUPL]@(comp env e2)@[SWAP]@(comp env e1)@
             [compile_binop op]
     |  (Ml_var  v) ->  compile_var env v
     |  (Ml_if(e1,e2,e3))
          -> [DUPL]@(comp env e1)@
	     [BRANCH(Adr(comp env e2@[RETURN]),
                     Adr(comp env e3@[RETURN]));
             CALL]
     |  (Ml_fun(x,e)) 
          -> [PUSH(Adr(comp (x::env) e @[RETURN]));SWAP;CONS]
     |  (Ml_app(e1,e2))
          -> [DUPL]@(comp env e2)@[SWAP]@(comp env e1)
	    @[SPLIT;IROT3;CONS;SWAP;CALL]
     |  (Ml_let(x,e1,e2))
          -> [DUPL]@(comp env e1)@[CONS]@(comp (x::env) e2)
     |  (Ml_letrec(f,((Ml_fun _) as e1),e2))
          -> [PUSH Nil]@(comp (f::env) e1)@[DUPL;ROT3;CONS;SETFST;FST]
	    @(comp (f::env) e2)
     |  (Ml_letrec(f,e1,e2))
          -> [DUPL;PUSH Nil;DUPL;CONS;DUPL;ROT3;CONS]
            @(comp (f::env) e1)
            @[DUPL;ROT3;FST;SETFST;SWAP;SND;SETSND;CONS]
	    @(comp (f::env) e2)
   in (comp [] e)@[STOP];;
(* +compile+ *)

(* +eval+ *)
let init_stack= [Nil];;
let eval e = exec(compile e,init_stack);;
(* +eval+ *)

(* +exemple1+ *)
let P1=
    (ml_exp_of_string
    ("let double = fun f -> fun x -> f(f x) " ^
     "in let sq = fun x -> x*x " ^
     "   in (double sq) 5"));;
(* +exemple1+ *)

(* +compileP1+ *)
compile P1;;
(* +compileP1+ *)

(* +evalP1+ *)
eval P1;;
(* +evalP1+ *)


(* +exemple2+ *)
let P2 =
  (ml_exp_of_string
    ("let rec fact=" ^
     "  fun n -> if n=0 then 1 else n*(fact(n-1)) in " ^
     "fact 10"));;
(* +exemple2+ *)

(* +compileP2+ *)
compile P2;;
(* +compileP2+ *)

(* +evalP2+ *)
eval P2;;
(* +evalP2+ *)

(* +exemple3+ *)
let P3 =
  (ml_exp_of_string
    "let rec p =  (1,(2,p)) in fst(snd(snd p))");;
(* +exemple3+ *)

(* +compileP3+ *)
compile P3;;
(* +compileP3+ *)

(* +evalP3+ *)
eval P3;;
(* +evalP3+ *)

(* +exemple_simple1+ *)
compile (ml_exp_of_string "2+3");;
(* +exemple_simple1+ *)

(* +exemple_simple2+ *)
compile (ml_exp_of_string "fun x -> x*2");;
(* +exemple_simple2+ *)

(* +exemple_simple3+ *)
compile (ml_exp_of_string "(fun x -> x*2) 3");;
(* +exemple_simple3+ *)
