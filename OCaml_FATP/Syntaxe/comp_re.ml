(* #use "load.ml" ;; *)

open Binary_trees
open Sets
open Orders

let char_for_read = Char.escaped ;;

let stream_check p = parser [< 'x when p x >] -> x ;;

let vect_indexq vect v =
  let i = ref 0 in
  while not (vect.(!i) == v) do incr i done;
  !i ;;

(* type 'a option = Some of 'a | None ;; *)

(** +regexpr+ *)
type int_set = int set ;;

type regexpr =
    Epsilon
  | Symbol of char * int
  | Star of regexpr * pos
  | Or of regexpr * regexpr * pos
  | Seq of regexpr * regexpr * pos
  | Accept of int
and pos =
  { null : bool
  ; first : int_set
  ; last : int_set
  } ;;

(** Générateur d'entiers *)
let (reset_labels, new_label) =
  let i = ref 0 in
  ((fun () -> i := 0), (fun () -> let p = !i in incr i; p)) ;;

let empty_int_set = (make_set int_comp [] : int_set) ;;

(** +first_pos+ *)
let first_pos = function
    Epsilon -> empty_int_set
  | Symbol (_, i) -> make_set int_comp [i]
  | Or (_, _, p) -> p.first
  | Star (_, p) -> p.first
  | Seq (_, _, p) -> p.first
  | Accept i -> make_set int_comp [i] ;;

(** +last_pos+ *)
let last_pos = function
    Epsilon -> empty_int_set
  | Symbol (_, i) -> make_set int_comp [i]
  | Or (_, _, p) -> p.last
  | Star (_, p) -> p.last
  | Seq (_, _, p) -> p.last
  | Accept i -> make_set int_comp [i] ;;

(** +null_pos+ *)
let null_pos = function
    Epsilon -> true
  | Symbol (_, i) -> false
  | Or (_, _, p) -> p.null
  | Star (_, p) -> p.null
  | Seq (_, _, p) -> p.null
  | Accept i -> false ;;

(** Les constructions de graphes *)
(** +construction+ *)
let symbol p = Symbol (p, new_label())
and star e =
  Star (e, { null = true; first = first_pos e; last = last_pos e })
and mkor e1 e2 =
  Or (e1, e2, { null = null_pos e1 || null_pos e2;
                first = set_union (first_pos e1) (first_pos e2);
                last = set_union (last_pos e1) (last_pos e2) })
and mkseq e1 e2 =
  let b1 = null_pos e1
  and b2 = null_pos e2 in
  Seq (e1, e2, { null = b1 && b2;
                 first = if b1 then set_union (first_pos e1) (first_pos e2)
                   else first_pos e1;
                 last = if b2 then set_union (last_pos e1) (last_pos e2)
                   else last_pos e2 }) ;;

let accept e =
  let i = new_label () in
  mkseq e (Accept i) ;;

let id e = e ;;

let postfix_op =
  parser
| [< ''*' >] -> star
| [< >] -> id ;;

let left_assoc term op =
  let rec sequence e1 =
    parser
  | [< f = op; e2 = term; e = (sequence (f e1 e2)) >] -> e
  | [< >] -> e1 in
  parser [< e1 = term; e = (sequence e1) >] -> e ;;

let left_assoc_juxt f empty term =
  let rec sequence e1 =
    parser
  | [< e2 = term; e = (sequence (f e1 e2)) >] -> e
  | [< >] -> e1 in
  parser
| [< e1 = term; e = (sequence e1) >] -> e
| [< >] -> empty ;;

let char =
  parser
| [< ''\\'; 'c >] -> c
| [< ''a'..'z' | 'A'..'Z' | '0'..'9' | ' ' | '\t' as c >] -> c ;;

let rec parse_re str =
  left_assoc disjunctelem (parser [< ''|' >] -> mkor) str
and disjunctelem str =
  left_assoc_juxt mkseq Epsilon concatelem str
and concatelem = parser
    [< e = atomic_re; f = postfix_op >] -> f e
and atomic_re =
  parser
| [< t = char >] -> symbol t
| [< ''('; p = parse_re; '')' >] -> p ;;

let parse_regexpr s =
  reset_labels ();
  let e = accept (parse_re (Stream.of_string s)) in
  (e, new_label ()) ;;

(** +compute_re+ *)
let compute_follow follow chars =
  let rec compute = function
    | Seq (e1, e2, p) ->
      compute e1; compute e2;
      let first2 = first_pos e2 in
      do_set (fun i -> follow.(i) <- set_union first2 (follow.(i))) (last_pos e1)
    | Star (e, p) ->
      compute e;
      do_set (fun i -> follow.(i) <- set_union p.first (follow.(i))) p.last
    | Or (e1, e2, p) -> compute e1; compute e2
    | Epsilon -> ()
    | Accept i -> chars.(i) <- None
    | Symbol (c, i) -> chars.(i) <- Some c
  in compute ;;

(** +regexpr_follow+ *)
let regexpr_follow s =
  let (e, n) = parse_regexpr s in
  let follow = Array.make n empty_int_set in
  let chars = Array.make n None in
  compute_follow follow chars e;
  (e, follow, chars) ;;

(** +state+ *)
type state = {
  pos : int_set
; mutable tran : transitions
}
and transitions = (char * state) list ;;

let partition (chars : char option array) =
  let rec part l1 l2 = match l1, l2 with
    (* | acc, [] -> List.map (fun (c, ref l) -> (c, l)) acc *)
    | acc, [] -> List.map (fun (c, l) -> (c, !l)) acc
    | acc, (i :: l) -> try let rl = List.assoc (chars.(i)) acc in
        rl := add_to_set (!rl) i;
        part acc l
      with Not_found ->
        part ((chars.(i), ref (make_set int_comp [i])) :: acc) l
  in fun s -> part [] (list_of_set s) ;;


(** +accessible+ *)
let accessible s follow chars =
  let part = partition chars s.pos in
  List.fold_right
    (fun s l -> match s, l with
       | (Some c, l), rest ->
         (c, List.fold_left set_union empty_int_set
            (List.map (Array.get follow) (list_of_set l))) :: rest
       | _, rest -> rest )
    part [] ;;

let rec find_state s1 s2 s3 = match s1, s2, s3 with
  | s, [], [] -> raise Not_found
  | s, (st :: sts), unmarked -> if set_equiv s st.pos then st
    else find_state s sts unmarked
  | s, [], (st :: sts) -> if set_equiv s st.pos then st
    else find_state s [] sts ;;

(** +compute_states+ *)
let rec compute_states marked unmarked follow chars =
  match unmarked with
  | [] -> Array.of_list marked
  | st :: umsts ->
    let access = accessible st follow chars in
    let marked1 = st :: marked in
    let unmarked1 =
      List.fold_right
        (fun (c, s) umsts ->
           if set_isempty s then
             umsts (* > Élimination des ensembles vides *)
           else try st.tran <- (c, find_state s marked1 umsts) :: st.tran;
               umsts
             with Not_found ->
               let state1 = { pos = s; tran = [] } in
               st.tran <- (c, state1) :: st.tran;
               state1 :: umsts)
        access umsts in
    compute_states marked1 unmarked1 follow chars ;;


(** Ca, c'est pour le printer des DFA *)

let rec do_ilist elem between = function
  | [] -> ()
  | [x] -> elem x
  | x :: xs -> elem x; between (); do_ilist elem between xs ;;

let do_iset elem between l =
  do_ilist elem between (list_of_set l) ;;

let print_trans (trans : transitions) =
  print_string "[";
  do_ilist (fun (c, st) ->
      print_string ("'" ^ (char_for_read c) ^ "'");
      print_string " mapsto ";
      print_string "[";
      do_iset print_int (fun () -> print_string ", ") st.pos;
      print_string "]")
    (fun () -> print_string " or ") trans;
  print_string "]" ;;

let print_set print_elem s =
  print_string "[";
  do_iset print_elem (fun () -> print_string ", ") s;
  print_string "]" ;;

(** Construction de l'automate *)
(** +dfa_of+ *)
let dfa_of (e, follow, chars) =
  let init_state = { pos = first_pos e; tran = [] } in
  let dfa = compute_states [] [init_state] follow chars in
  (* . Installation de l'état initial en position $0$ *)
  let idx_start = vect_indexq dfa init_state in
  dfa.(idx_start) <- dfa.(0);
  dfa.(0) <- init_state;
  dfa ;;

(** Interprétation de l'automate *)
(** +parser_or+ *)
let parser_or p pl =
  List.fold_left (fun p1 p2 -> (parser [< x = p1 >] -> x | [< y = p2 >] -> y))
    p pl ;;

(** +parser_of_char+ *)
let parser_of_char c =
  let pc = stream_check (fun x -> x = c) in
  fun str -> pc str; () ;;


(** Pour la fonction suivante, il faut remarquer que si
    pos ne contient pas l'etat final, la liste tran ne peut être vide *)

(** +interpret_dfa+ *)
let rec interpret_dfa dfa accept =
  (* . {\mlid accept} est le numéro associé au lexème {\mlid None} *)
  (* Construction du vecteur d'analyseurs *)
  let fvect = Array.make (Array.length dfa) (fun _ -> failwith "No value") in
  for i = 0 to Array.length dfa - 1 do
    let trans = dfa.(i).tran in
    let parsers =
      List.map (fun (c, st) ->
          let pc = parser_of_char c in
          let j = vect_indexq dfa st in
          parser [< _ = pc; _ = (fvect.(j)) >] -> ()) trans in
    if set_member accept (dfa.(i).pos) then
      (* fvect.(i) <- parser_or end_of_stream parsers *)
      fvect.(i) <- parser_or Stream.empty parsers
    else match parsers with
      | [] -> failwith "Impossible"
      | p :: ps -> fvect.(i) <- parser_or p ps
  done;
  fvect.(0) ;;

(** +regexpr_fun+ *)
let regexpr res =
  let ((e, follow, chars) as ast) = regexpr_follow res in
  let dfa = dfa_of ast in
  let p = interpret_dfa dfa (Array.length chars - 1) in
  fun s -> p (Stream.of_string s) ;;
