(* #use "load.ml" ;; *)

(* Le langage considere ainsi que celui des types est simplifie *)

(** +new_int+ *)
let new_int, reset_new_int =
  let c = ref (-1) in
  (fun () -> c := !c + 1 ; !c),
  (fun () -> c := -1) ;;

(** +ml_exp+ *)
type ml_exp =
  | Ml_int_const of int                  (* integer constant *)
  | Ml_var of string                             (* variable *)
  | Ml_fun of string * ml_exp                    (* function *)
  | Ml_app of ml_exp * ml_exp                 (* application *)
  | Ml_let of string * ml_exp * ml_exp ;;     (* declaration *)

(** Analyse syntaxique du langage *)
let rec spaces =
  parser
| [< ' (' ' | '\t' | '\n'); _ = spaces >] -> ()
| [< >] -> () ;;

type token =
  | LPAR | RPAR | COMMA | DOT | SEMIC | COLON
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
  | IDENT of string ;;

let int_of_digit = function
  | '0'..'9' as c -> (int_of_char c) - (int_of_char '0')
  |  _ ->  failwith "Not a Digit" ;;

let rec integer n =
  parser
| [< ' ('0'..'9' as c); r = (integer (10 * n + int_of_digit c)) >] -> r
| [< >] -> INT n ;;

let ident_max_length = 48;;

let ident_buf = Bytes.make ident_max_length ' ' ;;

let constructors0 = ["Nil"] ;;
let constructorsn = ["Cons"] ;;

let rec ident len =
  parser
| [< ' ('a' .. 'z' as c);
     r = (if len >= ident_max_length then ident len
          else (Bytes.set ident_buf len c;
                ident (succ len))) >] -> r
| [< ' ('A' .. 'Z' as c);
     r = (if len >= ident_max_length then ident len
          else (Bytes.set ident_buf len c;
                ident (succ len))) >] -> r
| [< >] ->
  match (String.sub (Bytes.to_string ident_buf) 0 len) with
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "fun" -> FUN
  | "let" -> LET
  | "rec" -> REC
  | "in" -> IN
  | "true" -> BOOL true
  | "false" -> BOOL false
  | s -> if List.mem s constructors0 then CONSTR0 s
    else
    if List.mem s constructorsn then CONSTRN s
    else IDENT s ;;

let rec lexer str =
  spaces str;
  let restsub =
    parser
  | [< ''>' >] -> [< 'ARROW; lexer str >]
  | [< >] -> [< 'MINUS; lexer str>]
  in
  match str with
    parser
  | [< ''('; _ = spaces >] -> [< 'LPAR; lexer str >]
  | [< '')'; _ = spaces >] -> [< 'RPAR; lexer str >]
  | [< ''['; _ = spaces >] -> [< 'LBRACK; lexer str >]
  | [< '']'; _ = spaces >] -> [< 'RBRACK; lexer str >]
  | [< ''{'; _ = spaces >] -> [< 'LBRACE; lexer str >]
  | [< ''}'; _ = spaces >] -> [< 'RBRACE; lexer str >]
  | [< '','; _ = spaces >] -> [< 'COMMA; lexer str >]
  | [< ''.'; _ = spaces >] -> [< 'DOT; lexer str >]
  | [< '';'; _ = spaces >] -> [< 'SEMIC; lexer str >]
  | [< '':'; _ = spaces >] -> [< 'COLON; lexer str >]
  | [< ''|'; _ = spaces >] -> [< 'BAR; lexer str >]
  | [< ''+'; _ = spaces >] -> [< 'PLUS; lexer str >]
  | [< ''-'>] -> restsub str
  | [< ''*'; _ = spaces >] -> [< 'MULT; lexer str >]
  | [< ''/'; _ = spaces >] -> [< 'DIV; lexer str >]
  | [< ''='; _ = spaces >] -> [< 'EQ; lexer str >]
  | [< ''<'; _ = spaces >] -> [< 'LT; lexer str >]
  | [< ''>'; _ = spaces >] -> [< 'GT; lexer str >]
  | [< ' ('0'..'9' as c);
       tok = (integer (int_of_digit c)) ;
       _ = spaces >] -> [< 'tok; lexer str >]
  | [< ' ('a' .. 'z' as c); tok = (Bytes.set ident_buf 0 c; ident 1) ; _ = spaces >] -> [< 'tok; lexer str >]
  | [< ' ('A' .. 'Z' as c); tok = (Bytes.set ident_buf 0 c; ident 1) ; _ = spaces >] -> [< 'tok; lexer str >] ;;

let lex s = lexer (Stream.of_string s) ;;

let ml_parser =
  let rec atom =
    parser
  | [< 'INT n >] -> Ml_int_const n
  | [< 'IDENT s >] -> Ml_var s
  | [< 'LPAR; e = expr; 'RPAR >] -> e
  and app =
    let rec restapp e1 =
      parser
    | [< e2 = atom; e = (restapp (Ml_app (e1, e2))) >] -> e
    | [< >] -> e1
    in parser [< e1 = atom; e = (restapp e1) >] -> e
  and expr =
    parser
  | [< 'FUN; 'IDENT x; 'ARROW; e = expr >] -> Ml_fun (x, e)
  | [< 'LET; 'IDENT x; 'EQ; e1 = expr; 'IN; e2 = expr >] -> Ml_let (x, e1, e2)
  | [< e = app >] -> e
  in expr ;;

let parse_ml_exp s = ml_parser (lexer (Stream.of_string s)) ;;

let ml_exp_of_string = parse_ml_exp ;;

(** +ml_type+ *)
type ml_type =
  | Unknown            (* Val field of a type variable *)
  | Int_type
  | Var_type of vartype
  | Arrow_type of ml_type * ml_type
and vartype = { idx: int; mutable valu: ml_type } ;;

(** Impression des types *)
let var_of_int n = "v" ^ (string_of_int n) ;;

let rec print_type = function
  | Int_type -> print_string "int"
  | Var_type { idx = n; valu = Unknown } -> print_string (var_of_int n)
  | Var_type {idx = _; valu = t } -> print_type t
  | Arrow_type (t1, t2) ->
    print_string "("; print_type t1;
    print_string " ml_type_infix_arrow "; print_type t2;
    print_string ")"
  | Unknown -> failwith "print_type" ;; (* Ne doit jamais se produire *)

let print_ml_type t =
  print_type t ;;


(** +scheme+ *)
type 'a scheme = Forall of int list * 'a ;;

(** +shorten+ *)
let rec shorten t = match t with
  | Var_type { idx = _; valu = Unknown } -> t
  | Var_type ({ idx = _; valu = Var_type { idx = _; valu = Unknown} as tv })
    -> tv
  | Var_type ({ idx = _; valu = Var_type tv1} as tv2)
    -> (tv2.valu <- tv1.valu); shorten t
  | Var_type { idx = _; valu = t' } -> t'
  | Unknown -> failwith "shorten"
  | t' -> t' ;;

(** +occurs+ *)
let occurs { idx = n; valu = _ } =
  let rec occrec = function
    | Var_type { idx = m; valu = _ } -> (n = m)
    | Int_type -> false
    | Arrow_type (t1, t2) -> (occrec t1) || (occrec t2)
    | Unknown -> failwith "occurs" (* Should never happen *)
  in occrec ;;

(** +unify+ *)
let rec unify (t1, t2) = match (shorten t1, shorten t2) with
    (Var_type ({ idx = n; valu = Unknown } as tv1),
     (Var_type { idx = m; valu = Unknown } as t2)) (* variables n and m *)
    -> if n <> m then tv1.valu <- t2 else ()
  | (t1, Var_type ({ idx = _; valu = Unknown } as tv)) (* type and variable *)
    -> if not (occurs tv t1) then tv.valu <- t1
    else failwith "unify"
  | ((Var_type { idx = _; valu = Unknown } as t1), t2) (* variable and type *)
    -> unify (t2, t1)
  | (Int_type, Int_type) -> ()
  | (Arrow_type (t1, t2), Arrow_type (t'1, t'2))
    -> unify (t1, t'1); unify (t2, t'2)
  | (_, _) -> failwith "unify" ;;

(** +vars_of_type+ *)
let vars_of_type tau =
  let rec vars vs = function
    | Int_type -> vs
    | Var_type { idx = n; valu = Unknown }
      -> if List.mem n vs then vs else n :: vs
    | Var_type { idx = _ ; valu = t } -> vars vs t
    | Arrow_type (t1, t2) -> vars (vars vs t1) t2
    | Unknown -> failwith "vars_of_type" (* Should never happen *)
  in vars [] tau ;;
(** +vars_of_type+ *)

let flat = List.fold_left ( @ ) [] ;;

(** +vars_of_ty_env+ *)
let subtract = fun l1 l2 -> List.filter (fun x -> not (List.mem x l2)) l1 ;;

let vars_of_ty_env env =
  flat (List.map
          (fun (_, Forall (gvars, t)) -> subtract (vars_of_type t) gvars)
          env) ;;

(** +unique+ *)
let rec unique = function
  | [] -> []
  | x :: l -> if List.mem x l then unique l else x :: unique l ;;

(** +generalize+ *)
let generalize tenv t =
  let genvars =
    unique (subtract (vars_of_type t)
              (vars_of_ty_env tenv))
  in Forall (genvars, t) ;;

(** +instance+ *)
let instance = function
  | Forall ([], t) -> t (* No generic variable *)
  | Forall (gvars, t) ->
    let new_vars =
      (* Associate a new variable with each generic variable *)
      List.map (fun n -> n, Var_type ({ idx = new_int (); valu = Unknown })) gvars in
    let rec inst = function
      (* and replace generic variables in the type by these fresh ones *)
      | (Var_type { idx = n; valu = Unknown } as t) ->
        (try List.assoc n new_vars
         with Not_found -> t)
      | Var_type { idx = _; valu = t } -> inst t
      | Int_type -> Int_type
      | Arrow_type (t1, t2) -> Arrow_type (inst t1, inst t2)
      | Unknown -> failwith "instance" (* Should never happen *)
    in inst t ;;

(** +type_expr+ *)
let rec type_expr tenv = function
  | Ml_int_const _ -> Int_type
  | Ml_var s ->
    let ts =
      try List.assoc s tenv
      with Not_found -> failwith "Unbound variable"
    in instance ts
  | Ml_let (s, e1, e2) ->
    let t1 = type_expr tenv e1 in
    let ts = generalize tenv t1
    in type_expr ((s, ts) :: tenv) e2
  | Ml_app (e1, e2) ->
    let u = Var_type { idx = new_int (); valu = Unknown }
    in unify (type_expr tenv e1, Arrow_type (type_expr tenv e2, u)); u
  | Ml_fun (x, e) ->
    let alpha = Var_type { idx = new_int (); valu = Unknown } in
    let ts = Forall ([], alpha)
    in Arrow_type (alpha, type_expr ((x, ts) :: tenv) e) ;;

(** +type_of+ *)
let type_of e =
  reset_new_int ();
  type_expr [] e ;;
