(* +all+ *)

#open "binary_trees";;

#open "sets";;

#open "orders";;

let vect_indexq vect v =
  let i = ref 0 in
  while not(vect.(!i) == v) do incr i done;
  !i
;;

type 'a option = Some of 'a | None;;

(* +regexpr+ *)

type int_set == int set;;

type regexpr =
       Epsilon
     | Symbol of char * int
     | Star of regexpr * pos
     | Or of regexpr * regexpr * pos
     | Seq of regexpr * regexpr * pos
     | Accept of int
and pos =
    { Null : bool
    ; First : int_set
    ; Last : int_set
    }
;;

(* +regexpr+ *)
(* Générateur d'entiers *)

let (reset_labels, new_label) =
  let i = ref 0 in
  ((fun () -> i:=0), (fun () -> let p = !i in incr i; p))
;;

let empty_int_set = (make_set int_comp [] : int_set);;

(* +first_pos+ *)

let first_pos = function
  Epsilon -> empty_int_set
| Symbol(_,i) -> make_set int_comp [i]
| Or(_,_,p) -> p.First
| Star(_,p) -> p.First
| Seq(_,_,p) -> p.First
| Accept i -> make_set int_comp [i]
;;

(* +first_pos+ *)

(* +last_pos+ *)

let last_pos = function
  Epsilon -> empty_int_set
| Symbol(_,i) -> make_set int_comp [i]
| Or(_,_,p) -> p.Last
| Star(_,p) -> p.Last
| Seq(_,_,p) -> p.Last
| Accept i -> make_set int_comp [i]
;;

(* +last_pos+ *)
(* +null_pos+ *)

let null_pos = function
  Epsilon -> true
| Symbol(_,i) -> false
| Or(_,_,p) -> p.Null
| Star(_,p) -> p.Null
| Seq(_,_,p) -> p.Null
| Accept i -> false
;;

(* +null_pos+ *)
(* Les constructions de graphes *)
(* +construction+ *)

let symbol p = Symbol (p, new_label())
and star e =
  Star (e, {Null = true; First = first_pos e; Last = last_pos e})
and mkor e1 e2 =
  Or(e1, e2, {Null = null_pos e1 or null_pos e2;
              First = set_union (first_pos e1) (first_pos e2);
              Last = set_union (last_pos e1) (last_pos e2)})
and mkseq e1 e2 =
  let b1 = null_pos e1
  and b2 = null_pos e2 in
  Seq(e1, e2, {Null = b1 & b2;
               First = if b1 then set_union (first_pos e1) (first_pos e2)
                       else first_pos e1;
               Last = if b2 then set_union (last_pos e1) (last_pos e2)
                      else last_pos e2})
;;

let accept e =
  let i = new_label() in
  mkseq e (Accept i)
;;

(* +construction+ *)

let id e = e;;

let postfix_op = function
  [< '`*` >] -> star
| [< >] -> id
;;

let left_assoc term op =
  let rec sequence e1 = function
    [< op f; term e2; (sequence (f e1 e2)) e >] -> e
  | [< >] -> e1 in
  function [< term e1; (sequence e1) e >] -> e
;;

let left_assoc_juxt f empty term =
  let rec sequence e1 = function
    [<term e2; (sequence (f e1 e2)) e >] -> e
  | [< >] -> e1 in
  function [< term e1; (sequence e1) e >] -> e
         | [< >] -> empty
;;

let char = function
  [< '`\\`; 'c >] -> c
| [< '(`a`..`z` | `A`..`Z` | `0`..`9` | ` ` | `\t`) as c >] -> c
;;

let rec parse_re str =
  left_assoc disjunctelem (function [< '`|` >] -> mkor) str
and disjunctelem str =
  left_assoc_juxt mkseq Epsilon concatelem str
and concatelem = function
  [< atomic_re e; postfix_op f >] -> f e
and atomic_re = function
  [< char t >] -> symbol t
| [< '`(`; parse_re p; '`)` >] -> p
;;

let parse_regexpr s =
  reset_labels();
  let e = accept(parse_re (stream_of_string s)) in
  (e, new_label())
;;

(* +compute_re+ *)

let compute_follow follow chars =
  let rec compute = function
    Seq(e1,e2,p) ->
      compute e1; compute e2;
      let first2 = first_pos e2 in
      do_set (fun i -> follow.(i) <- set_union first2 (follow.(i))) (last_pos e1)
  | Star(e,p) ->
      compute e;
      do_set (fun i -> follow.(i) <- set_union p.First (follow.(i))) p.Last
  | Or(e1,e2,p) -> compute e1; compute e2
  | Epsilon -> ()
  | Accept i -> chars.(i) <- None
  | Symbol(c,i) -> chars.(i) <- Some c
  in compute
;;

(* +compute_re+ *)
(* +regexpr_follow+ *)

let regexpr_follow s =
  let (e,n) = parse_regexpr s in
  let follow = make_vect n empty_int_set in
  let chars = make_vect n None in
  compute_follow follow chars e;
  (e, follow, chars)
;;

(* +regexpr_follow+ *)
(* +state+ *)

type state = {
       Pos : int_set
     ; mutable Tran : transitions
     }
and transitions == (char * state) list;;

(* +state+ *)

let partition (chars : char option vect) =
  let rec part = fun
    acc [] -> map (fun (c, ref l) -> (c,l)) acc
  | acc (i::l) -> try let rl = assoc (chars.(i)) acc in
                      rl := add_to_set (!rl) i;
                      part acc l
                  with Not_found ->
                         part ((chars.(i), ref (make_set int_comp [i]))::acc) l
  in fun s -> part [] (list_of_set s)
;;

(* +accessible+ *)

let accessible s follow chars =
  let part = partition chars s.Pos in
  list_it (fun (Some c, l) rest ->
                 (c, it_list set_union empty_int_set
                       (map (vect_item follow) (list_of_set l)))::rest
             | _ rest -> rest)
    part []
;;

(* +accessible+ *)

let rec find_state = fun
  s [] [] -> raise Not_found
| s (st::sts) unmarked -> if set_equiv s st.Pos then st
                          else find_state s sts unmarked
| s [] (st::sts) -> if set_equiv s st.Pos then st
                    else find_state s [] sts
;;

(* +compute_states+ *)

let rec compute_states marked unmarked follow chars =
  match unmarked with
    [] -> vect_of_list marked
  | st::umsts ->
      let access = accessible st follow chars in
      let marked1 = st::marked in
      let unmarked1 =
        list_it
          (fun (c, s) umsts ->
                 if set_isempty s then
                   umsts (* > Élimination des ensembles vides *)
                 else try st.Tran <- (c, find_state s marked1 umsts)::st.Tran;
                          umsts
                      with Not_found ->
                             let state1 = {Pos = s; Tran = []} in
                             st.Tran <- (c, state1)::st.Tran;
                             state1::umsts)
          access umsts in
      compute_states marked1 unmarked1 follow chars
;;

(* +compute_states+ *)
(* Ca, c'est pour le printer des DFA *)

let rec do_ilist elem between = function
  [] -> ()
| [x] -> elem x
| x::xs -> elem x; between (); do_ilist elem between xs
;;

let do_iset elem between l =
  do_ilist elem between (list_of_set l)
;;

#infix "mapsto";;

let print_trans (trans : transitions) =
  print_string "[";
  do_ilist (fun (c,st) ->
                  print_string ("`" ^ (char_for_read c) ^ "`");
                  print_string " mapsto ";
                  print_string "[";
                  do_iset print_int (fun () -> print_string ", ") st.Pos;
                  print_string "]")
    (fun () -> print_string " or ") trans;
  print_string "]"
;;

let print_set print_elem s =
  print_string "[";
  do_iset print_elem (fun () -> print_string ", ") s;
  print_string "]"
;;

(* new_printer "transitions" print_trans;;
   new_printer "int_set" (print_set print_int);;
 *)
(* Construction de l'automate *)
(* +dfa_of+ *)

let dfa_of (e, follow, chars) =
  let init_state = {Pos = first_pos e; Tran = []} in
  let dfa = compute_states [] [init_state] follow chars in
  (* . Installation de l'état initial en position $0$ *)
  let idx_start = vect_indexq dfa init_state in
  dfa.(idx_start) <- dfa.(0);
  dfa.(0) <- init_state;
  dfa
;;

(* +dfa_of+ *)
(* Interprétation de l'automate *)
(* +parser_or+ *)

let parser_or p pl =
  it_list (fun p1 p2 -> (function [< p1 x >] -> x | [< p2 y >] -> y))
    p pl
;;

(* +parser_or+ *)
(* +parser_of_char+ *)

let parser_of_char c =
  let pc = stream_check (prefix == c) in
  fun str -> pc str; ()
;;

(* +parser_of_char+ *)

(* Pour la fonction suivante, il faut remarquer que si
   Pos ne contient pas l'etat final, la liste Tran ne peut e^tre vide *)


(* +interpret_dfa+ *)

let rec interpret_dfa dfa accept =
  (* . {\mlid accept} est le numéro associé au lexème {\mlid None} *)
  (* Construction du vecteur d'analyseurs *)
  let fvect = make_vect (vect_length dfa) (fun _ -> failwith "No value") in
  for i=0 to vect_length dfa - 1 do
    let trans = dfa.(i).Tran in
    let parsers =
      map (fun (c, st) ->
                   let pc = parser_of_char c in
                   let j = vect_indexq dfa st in
                   function [< pc _; (fvect.(j)) _ >] -> ()) trans in
    if set_member accept (dfa.(i).Pos) then
      fvect.(i) <- parser_or end_of_stream parsers
    else match parsers with
           [] -> failwith "Impossible"
         | p::ps -> fvect.(i) <- parser_or p ps
  done;
  fvect.(0)
;;

(* +interpret_dfa+ *)
(* +regexpr_fun+ *)

let regexpr res =
  let ((e, follow, chars) as ast) = regexpr_follow res in
  let dfa = dfa_of ast in
  let p = interpret_dfa dfa (vect_length chars - 1) in
  fun s -> p (stream_of_string s)
;;

(* +regexpr_fun+ *)
(* +all+ *)


