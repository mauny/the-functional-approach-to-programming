(* #use "load.ml" ;; *)

open Prelud
open Binary_trees
open Sets

(** +No_solution+ *)
exception No_solution


(** FONCTIONS GENERALES DE RECHERCHE DE SOLUTIONS *)
(** recherche breadth-first naive d'une solution *)

(** +flat_map+ *)
let rec flat_map f = function
  | [] -> []
  | (a :: l) -> f a @ flat_map f l ;;

(** +union_map+ *)
let rec union_map f = function
  | [] -> []
  | (a :: l) -> union (f a) (union_map f l) ;;

(** +naive_solve_breadth_first+ *)
let naive_solve_breadth_first (ok, moves) start =
  find ok (loop (List.exists ok) (union_map moves) [start]) ;;

(* recherche breadth-first d'une solution
     avec memorisation des configurations
     rencontrees:
     On commence par definir une fonction "archive_map"
     qui joue le meme role que la fonction "flat_map"
     mais qui ajoute une archivation des configurations
     rencontrees.
     Alors que "flat_map"  est de type
     ('a -> 'b list) -> 'a list -> 'b list,
     "archive_map" est essentiellement de type
     ('a -> 'b list) -> 'c * 'a list -> 'c * 'b list
     où 'c est le type de l'archive.
*)

(** +archive1+ *)
type 'a archive = Arch of 'a set ;;

let add_to_archive = fun
  (Arch s) x -> Arch (add_to_set s x) ;;

let add_list_to_archive = fun
  (Arch s) l -> Arch (add_list_to_set s l) ;;

let make_archive comp l = Arch (make_set comp l) ;;

let archive_member = fun
  x (Arch s) -> set_member x s ;;

(** +archive_map+ *)
let archive_map arch_part f (arch,l) =
  let rec arch_map arch ll = function
    | [] -> (arch, ll)
    | (c :: cl) ->
      let ll' = select
          (fun c -> not (archive_member (arch_part c) arch)) (f c) in
      arch_map (add_list_to_archive arch (List.map arch_part ll')) (ll' @ ll) cl
  in
  arch_map arch [] l ;;

(** +solve_breadth_first+ *)
let solve_breadth_first (ok, pos_moves, comp) arch_part start =
  (find ok % snd)
    (loop
       (List.exists ok % snd)
       (archive_map arch_part pos_moves)
       (make_archive comp (List.map arch_part start), start)) ;;

let solve_breadth_first_trace (ok, pos_moves, comp) arch_part start =
  (find ok % snd)
    ((n := 0; loopn)
       (List.exists ok % snd)
       (archive_map arch_part pos_moves)
       (make_archive comp (List.map arch_part start), start)) ;;

(* recherche breadth-first de toutes les solutions :
       on utilise une fonction "add_memo" qui permet
       d'ajouter à la fonction utilisée dans l'itération
       une mémorisation des solutions rencontrées
       "add_memo" a pour type:
       ('a -> bool) -> ('b * 'c -> 'd * 'a list)
         -> 'a list * ('b * 'c) -> 'a list * ('d * 'a list)
*)

let add_memo ok f (m,(a,l))=
  let (a', l') = f (a, l) in
  let (l1, l2) = partition ok l' in (l2 @ m, (a', l1)) ;;

let find_all_solutions trace
    (ok, pos_moves, comp)
    arch_part start =
  (if trace then (n := 0; loopn) else loop)
    (fun (_sols, (_a, l)) -> l = [])
    (add_memo ok
       (archive_map arch_part pos_moves) )
    ([], (make_archive comp (List.map arch_part start), start)) ;;


(** Exploration complete du graphe *)

(** +explore_breadth_first+ *)
let _explore_breadth_first (pos_moves, comp) arch_map start =
  fst (loop
         (fun (_s, l) -> l = [])
         (archive_map arch_map pos_moves)
         (make_archive comp (List.map arch_map start), start)) ;;

let explore_breadth_first_trace (pos_moves, comp) arch_map start =
  fst ((n := 0; loopn)
         (fun (_s, l) -> l = [])
         (archive_map arch_map pos_moves)
         (make_archive comp (List.map arch_map start), start)) ;;

(**   Recherche depth_first de solutions *)

(** +naive_solve_depth_first+ *)
let naive_solve_depth_first (ok, pos_moves) c =
  let rec solve_rec = function
    | [] -> raise No_solution
    | (c :: cl as _cl') ->
      if ok c then c
      else solve_rec (pos_moves c @ cl)
  in
  solve_rec [c] ;;

(** +solve_depth_first+ *)
let solve_depth_first (ok, pos_moves, comp) arch_part c =
  let rec solve_rec a = function
    | [] -> raise No_solution
    | (c :: cl) ->
      if ok c then c else
      if archive_member (arch_part c) a then solve_rec a cl
      else solve_rec (add_to_archive a (arch_part c))
          (pos_moves c @ cl)
  in
  solve_rec (make_archive comp []) [c] ;;

let solve_depth_first_trace (ok, pos_moves, comp) arch_part c =
  let rec solve_rec a cl n p =
    match cl with
    | [] -> raise No_solution
    | (c :: cl') ->
      if ok c then (a, c, n, p) else
      if archive_member (arch_part c) a then solve_rec a cl' n (p + 1)
      else
        ((if n land 1023 = 0
          then (print_int n; print_string " ";
                print_int p; print_string " ";
                print_int (List.length cl); print_newline ())) ;
         solve_rec (add_to_archive a (arch_part c))
           (pos_moves c @ cl') (n + 1) (p + 1))
  in
  solve_rec (make_archive comp []) [c] 0 0 ;;

let solve_all_depth_first output (ok, pos_moves)
    (member, add, empty_arch, arch_part) c =
  let rec solve_rec a cl n p =
    match cl with
    | [] -> ()
    | (c :: cl') ->
      (if ok c then output (a, c, n, p)) ;
      if member (arch_part c) a then solve_rec a cl' n (p + 1)
      else let cs = pos_moves c in
        (if n land 16383 = 0
         then (print_int n; print_string " " ;
               print_int p; print_string " " ;
               print_int (List.length cl); print_newline () ;
               output (a, c, n, p); print_newline ()) ;
         solve_rec (add a (arch_part c)) (cs @ cl')(n + 1)(p + 1))
  in
  solve_rec empty_arch [c] 0 0 ;;

(** +explore_depth_first+ *)
let explore_depth_first (pos_moves,comp) arch_part c =
  let rec solve_rec a = function
    | [] -> a
    | (c :: cl) ->
      if archive_member (arch_part c) a then solve_rec a cl
      else solve_rec (add_to_archive a (arch_part c) )
          (pos_moves c @ cl)
  in
  solve_rec (make_archive comp []) [c] ;;

(** +archive2+ *)
let add_to_archive = add_to_set ;;

let add_list_to_archive = add_list_to_set ;;

let make_archive = make_set ;;

let archive_member = set_member ;;

let archive_map arch_part f (arch, l) =
  let rec arch_map arch ll = function
    | [] -> (arch, ll)
    | (c :: cl) ->
      let ll' = select (fun c ->
          not (archive_member
                 (arch_part c) arch)) (f c) in
      arch_map (add_list_to_archive arch (List.map arch_part ll'))
        (ll' @ ll) cl
  in
  arch_map arch [] l ;;

let explore_breadth_first (pos_moves, comp) arch_map start =
  fst (loop
         (fun (_s, l) -> l = [])
         (archive_map arch_map pos_moves)
         (make_archive comp (List.map arch_map start), start)) ;;

(** +findcc+ *)
let rec findcc (pos_moves, comp) cset =
  if set_isempty cset then []
  else let c = set_random_element cset in
    let cc = explore_breadth_first (pos_moves, comp) (fun x -> x) [c]
    in cc :: findcc (pos_moves, comp) (set_diff cset cc) ;;
