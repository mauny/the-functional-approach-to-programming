(* #use "load.ml" ;; *)

open Lexxer
open Ml_exp1
open Ml1_parser
open Code_simulator

(** +compile_binop_var+ *)
let compile_unop = function
  | Ml_fst -> FST
  | Ml_snd -> SND ;;

let compile_binop = function
  | Ml_add -> ADD
  | Ml_sub -> SUB
  | Ml_mult -> MULT
  | Ml_eq -> EQ
  | _ -> failwith "compile_binop: not implemented" ;;

exception Compile_error of string ;;

let rec compile_var env v =
  match env with
  | [] -> raise (Compile_error "unbound variable")
  | (x :: env) -> if x = v then [FST]
    else SND :: (compile_var env v) ;;

(** +compile+ *)
let compile e =
  let rec comp env = function
    | (Ml_int_const n) -> [LOAD (Int_Const n)]
    | (Ml_bool_const b) -> [LOAD (Bool_Const b)]
    | (Ml_pair (e1, e2)) -> [DUPL] @ (comp env e2) @ [SWAP]
                            @ (comp env e1) @ [CONS]
    | (Ml_unop (op, e)) -> (comp env e) @ [compile_unop op]
    | (Ml_binop (op, e1, e2))
      -> [DUPL] @ (comp env e2) @ [SWAP] @ (comp env e1) @
         [compile_binop op]
    | (Ml_var v) -> compile_var env v
    | (Ml_if (e1, e2, e3))
      -> [DUPL] @ (comp env e1) @
         [BRANCH (Adr (comp env e2 @ [RETURN]),
                  Adr (comp env e3 @ [RETURN]));
          CALL]
    | (Ml_fun (x, e))
      -> [PUSH (Adr (comp (x :: env) e  @ [RETURN])); SWAP; CONS]
    | (Ml_app (e1, e2))
      -> [DUPL] @ (comp env e2) @ [SWAP] @ (comp env e1)
         @ [SPLIT; IROT3; CONS; SWAP; CALL]
    | (Ml_let (x, e1, e2))
      -> [DUPL] @ (comp env e1) @ [CONS] @ (comp (x :: env) e2)
    | (Ml_letrec (f, ((Ml_fun _) as e1), e2))
      -> [PUSH Nil] @ (comp (f :: env) e1) @ [DUPL; ROT3; CONS; SETFST; FST]
         @ (comp (f :: env) e2)
    | (Ml_letrec (f, e1, e2))
      -> [DUPL; PUSH Nil; DUPL; CONS; DUPL; ROT3; CONS]
         @ (comp (f :: env) e1)
         @ [DUPL; ROT3; FST; SETFST; SWAP; SND; SETSND; CONS]
         @ (comp (f :: env) e2)
  in (comp [] e) @ [STOP] ;;

(** +eval+ *)
let init_stack = [Nil] ;;

let eval e = exec (compile e, init_stack) ;;

(** +exemple1+ *)
let p1 =
  (ml_exp_of_string
     ("let double = fun f -> fun x -> f(f x) " ^
      "in let sq = fun x -> x*x " ^
      "   in (double sq) 5")) ;;

(** +compilep1+ *)
compile p1 ;;

(** +evalp1+ *)
eval p1 ;;

(** +exemple2+ *)
let p2 =
  (ml_exp_of_string
     ("let rec fact=" ^
      "  fun n -> if n=0 then 1 else n*(fact(n-1)) in " ^
      "fact 10")) ;;

(** +compilep2+ *)
compile p2 ;;

(** +evalp2+ *)
eval p2 ;;

(** +exemple3+ *)
let p3 =
  (ml_exp_of_string
     "let rec p =  (1,(2,p)) in fst(snd(snd p))") ;;

(** +compilep3+ *)
compile p3 ;;

(** +evalp3+ *)
eval p3 ;;

(** +exemple_simple1+ *)
compile (ml_exp_of_string "2+3") ;;

(** +exemple_simple2+ *)
compile (ml_exp_of_string "fun x -> x*2") ;;

(** +exemple_simple3+ *)
compile (ml_exp_of_string "(fun x -> x*2) 3") ;;

