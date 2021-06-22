(* #use "load.ml" ;; *)

open Prelud
open Orders
open Binary_trees
open Sets
open Games


(**   JEU DU SOLITAIRE *)
(* On part d'une représentation naive sous forme de vecteurs *)
(* d'entiers 0 ou 1 représentant des booleens *)
(* mais on va travailler sur une  représentation compacte *)
(* Malheureusement, les entiers caml light ne sont pas assez *)
(* pour contenir 33 bits.  On séparera donc les 3 premiers bits *)
(* des autres pour aboutir a une représentation sous la forme *)
(* d'une paire d'entiers *)

(** +compact+ *)
let compact l = match l with
  | n0 :: n1 :: n2 :: l ->
    let rec save n i l =
      if i < 3 then n
      else save ((n lsl 1) lor (List.hd l)) (i - 1) (List.tl l)
    in
    (4 * n2 + 2 * n1 + n0, save 0 32 (List.rev l))
  | _ -> failwith "compact: list too short" ;;

let uncompact  (m, n) =
  let rec unsave n i =
    if i > 29 then [] else (n mod 2) :: unsave (n lsr 1) (i + 1)
  in
  (m mod 2) :: ((m lsr 1) mod 2) :: ((m lsr 2) mod 2) :: unsave n 0 ;;

let compactv v =
  let rec save n i =
    if i < 3 then n
    else save ((n lsl 1) lor v.(i)) (i - 1)
  in
  (4 * v.(2) + 2 * v.(1) + v.(0), save 0 32) ;;

(** +nth+ *)
let nth n (p1, p2) =
  if n <= 2 then (p1 lsr n) mod 2
  else (p2 lsr (n - 3)) mod 2 ;;

let set_nth n b (p1, p2) =
  let set_nth' n b p =
    if b then p + (1 lsl n)
    else p - (1 lsl n) in
  if n <= 2 then (set_nth' n b p1, p2)
  else (p1, set_nth'(n - 3) b p2) ;;

(* Attention: "set_nth" fait l'hypothese que l'on inverse le
   bit considéré   *)

(** +conv+ *)
let conv n=
  if n <= 12 then failwith "conv: wrong arg" else
  if n <= 15 then n - 13 else
  if n <= 25 then n - 20 else
  if n <= 37 then n - 25 else
  if n <= 47 then n - 28 else
  if n <= 57 then n - 31 else
  if n <= 65 then n - 36 else
  if n <= 75 then n - 43 else failwith "conv: wrong arg" ;;

let iconv n =  if n < 0 || n > 32
  then failwith ("iconv failed: " ^ string_of_int n)
  else if n <=2  then 13 + n
  else if n <=5  then 20 + n
  else if n <=12 then 25 + n
  else if n <=19 then 28 + n
  else if n <=26 then 31 + n
  else if n <=29 then 36 + n
  else 43 + n ;;

(** +direction+ *)
type direction = Left | Up | Right | Down ;;

(** +opdir+ *)
let op_dir=
  function Left -> Right | Right -> Left | Up -> Down | Down -> Up ;;

(** +right+ *)
let right_ =
  [|          1; 2;-1;
              4; 5;-1;
              7; 8; 9;10;11;12;-1;
              14;15;16;17;18;19;-1;
              21;22;23;24;25;26;-1;
              28;29;-1;
              31;32;-1      |] ;;


(** +autres_deplacements+ *)
let right =
  [|1;2;-1;4;5;-1;7;8;9;10;11;12;-1;14;15;16;17;
    18;19;-1;21;22;23;24;25;26;-1;28;29;-1;31;32;-1   |] ;;

let right2 =
  [|2;-1;-1;5;-1;-1;8;9;10;11;12;-1;-1;15;16;17;18;19;-1;-1;
    22;23;24;25;26;-1;-1;29;-1;-1;32;-1;-1 |] ;;

let left =
  [|-1;0;1;-1;3;4;-1;6;7;8;9;10;11;-1;13;14;15;16;17;18;
    -1;20;21;22;23;24;25;-1;27;28;-1;30;31  |] ;;

let left2 =
  [|-1;-1;0;-1;-1;3;-1;-1;6;7;8;9;10;-1;-1;13;14;15;16;17;
    -1;-1;20;21;22;23;24;-1;-1;27;-1;-1;30 |] ;;

let down =
  [|3;4;5;8;9;10;13;14;15;16;17;18;19;20;21;22;23;24;25;26;
    -1;-1;27;28;29;-1;-1;30;31;32;-1;-1;-1 |] ;;

let down2 =
  [|8;9;10;15;16;17;20;21;22;23;24;25;26;-1;-1;27;28;29;-1;-1;
    -1;-1;30;31;32;-1;-1;-1;-1;-1;-1;-1;-1 |] ;;

let up =
  [|-1;-1;-1;0;1;2;-1;-1;3;4;5;-1;-1;6;7;8;9;10;11;12;
    13;14;15;16;17;18;19;22;23;24;27;28;29 |] ;;

let up2 =
  [|-1;-1;-1;-1;-1;-1;-1;-1;0;1;2;-1;-1;-1;-1;3;4;5;-1;-1;
    6;7;8;9;10;11;12;15;16;17;22;23;24 |] ;;

(** +table+ *)
let table d n = match (d, n) with
  | Left, 1 -> left
  | Left, 2 -> left2
  | Right, 1 -> right
  | Right, 2 -> right2
  | Up, 1 -> up
  | Up, 2 -> up2
  | Down, 1 -> down
  | Down, 2 -> down2
  | _ -> failwith "Deplacement inconnu" ;;

(** +config+ *)
type config = { size: int; moves: (int * direction) list ;
                board: int * int } ;;

(** +correct_move+ *)
let correct_move i dir { size = k; moves = ml; board = p } =
  let dir' = op_dir dir in
  let tab = table dir' 1 and tab2 = table dir' 2 in
  let i1 = tab.(i) and i2 = tab2.(i) in
  if nth i p  = 0 && i1 <> -1 && i2 <> -1 &&
     nth i1 p = 1 && nth i2 p = 1
  then [{size = pred k; moves = (i, dir) :: ml ;
         board = set_nth i true
             (set_nth i1 false (set_nth i2 false p))}]
  else [] ;;

(** +solit_moves+ *)
let solit_moves config =
  let rec pm n mvs =
    if n < 0 then mvs
    else pm (n - 1)
        (flat_map (fun d -> correct_move n d config )
           [Left; Right; Up; Down] @ mvs)
  in
  pm 32 [] ;;

(*
type direction = Left | Up | Right | Down | Erase of int list ;;

let big_move { size = k; moves = ml; board = p}  n ns =
  [{ size = k - list_length ns; moves = (n, Erase ns) :: ml;
    board = List.fold_left (fun p n -> set_nth n false p) p ns }] ;;


let check b l p =
  for_all (fun x -> x <> -1 && nth x p = b) l ;;

let Tmoves i c=
  let p = c.board in
  let checkT i d1 d2 =
    if nth i p = 0 then [] else
    let d11 = table d1 1 and d12 = table d2 2
    and d21 = table d2 1 and d22 = table d2 2 in
    let p1= d11.(i) and p2=d12.(i) in
    if p1 = -1 || p2 = -1 then [] else
    let q1 = d21.(p1) and q2=d22.(p1) in
    if check 0 [p2] p && check 1 [p1; q1; q2] p
    then big_move c i [p1; q1; q2]
    else [] in
  (checkT i Right Up @
   checkT i Left Up @
   checkT i Left Down @
   checkT i Right Down @
   checkT i Down Right @
   checkT i Down Left @
   checkT i Up Left @
   checkT i Up Right) ;;


let Lmoves i c =
  let p = c.board in
  let checkL i d1 d2 d3 d4 =
    if nth i p = 0 then [] else
    let d11 = table d1 1 and d12 = table d1 2
    and d21 = table d2 1 and d22 = table d2 2
    and d3 = table d3 1
    and d41 = table d4 1 and d42 = table d4 2 in
    let p1 = d11.(i) and p2 = d12.(i) in
    if p1 = -1 then [] else
    let q1 = d21.(p1) and q2 = d22.(p1)
    and r = d3.(p1) in
    if q1 = -1 || q2 = -1 || r = -1 then [] else
    let s1 = d41.(q2) and s2 = d42.(q2) in
    if check 0 [p2] p && check 1 [r; p1; q1; q2; s1; s2] p
    then big_move c i [r; p1; q1; q2; s1; s2]
    else [] in
  (checkL i Right Up Down Right @
   checkL i Right Up Down Left @
   checkL i Left Up Down Left @
   checkL i Left Up Down Right @
   checkL i Left Down Up Left @
   checkL i Left Down Up Right @
   checkL i Right Down Up Right @
   checkL i Right Down Up Left @
   checkL i Down Right Left Down @
   checkL i Down Right Left Up @
   checkL i Down Left Right Down @
   checkL i Down Left Right Up @
   checkL i Up Left Right Up @
   checkL i Up Left Right Down @
   checkL i Up Right Left Up @
   checkL i Up Right Left Down) ;;

let S1moves i c =
  let p = c.board in
  let checkS1 i d1 d2 =
    if nth i p = 0 then [] else
    let d11 = table d1 1 and d12 = table d1 2
    and d21 = table d2 1 and d22 = table d2 2 in
    let p1 = d11.(i) and p2 = d12.(i) in
    if p1 = -1 || p2 = -1 then [] else
    let q1 = d21.(p1) and q2 = d22.(p1) in
    if q1 = -1 || q2 = -1 then [] else
    let s1 = d11.(q1) and t1 = d12.(q1)
    and s2 = d11.(q2) and t2 = d12.(q2) in
    if check 0 [p1; p2] p && check 1 [q1; q2; s1; s2; t1; t2] p
    then big_move c i [q1; q2; s1; s2; t1; t2]
    else [] in
  (checkS1 i Right Up @
   checkS1 i Left Up @
   checkS1 i Left Down @
   checkS1 i Right Down @
   checkS1 i Down Right @
   checkS1 i Down Left @
   checkS1 i Up Left @
   checkS1 i Up Right) ;;

let S2moves i c =
  let p = c.board in
  let checkS2 i d1 d2 =
    if nth i p = 0 then [] else
    let d3 = op_dir d1 in
    let d11 = table d1 1 and d12 = table d1 2
    and d21 = table d2 1 and d22 = table d2 2
    and d31 = table d3 1 and d32 = table d3 2 in
    let p1 = d11.(i) and p2 = d12.(i) in
    if p1 = -1 || p2 = -1 then [] else
    let q1 = d21.(p1) and q2 = d22.(p1) in
    if q1 = -1 || q2 = -1 then [] else
    let s1 = d31.(q1) and t1 = d32.(q1)
    and s2 = d31.(q2) and t2 = d32.(q2) in
    if check 0 [p1; p2] p && check 1 [q1; q2; s1; s2; t1; t2] p
    then big_move c i [q1; q2; s1; s2; t1; t2]
    else [] in
  (checkS2 i Right Up @
   checkS2 i Left Up @
   checkS2 i Left Down @
   checkS2 i Right Down @
   checkS2 i Down Right @
   checkS2 i Down Left @
   checkS2 i Up Left @
   checkS2 i Up Right) ;;

let S3moves i c =
  let p = c.board in
  let checkS3 i d1 d2 =
    if nth i p = 0 then [] else
    let d11 = table d1 1 and d12 = table d1 2
    and d21 = table d2 1 and d22 = table d2 2 in
    let r = d11.(i) in
    if r = -1 then [] else
    let q1 = d11.(r) and q2 = d12.(r) in
    if q1 = -1 || q2 = -1 then [] else
    let s1 = d21.(q1) and t1 = d22.(q1)
    and s2 = d21.(q2) and t2 = d22.(q2) in
    if check 0 [r] p && check 1 [q1; q2; s1; s2; t1; t2] p
    then big_move c i [q1; q2; s1; s2; t1; t2]
    else [] in
  (checkS3 i Right Up @
   checkS3 i Left Up @
   checkS3 i Left Down @
   checkS3 i Right Down @
   checkS3 i Down Right @
   checkS3 i Down Left @
   checkS3 i Up Left @
   checkS3 i Up Right) ;;

let S4moves i c =
  let p = c.board in
  let checkS4 i d1 d2=
    if nth i p = 0 then [] else
    let d3 = op_dir d1 in
    let d11 = table d1 1 and d12 = table d1 2
    and d21 = table d2 1 and d22 = table d2 2
    and d31 = table d3 1 in
    let r = d31.(i)
    and q1 = d11.(i) and q2 = d12.(i) in
    if r = -1 || q1 = -1 || q2 = -1 then [] else
    let s1 = d21.(q1) and t1 = d22.(q1)
    and s2 = d21.(q2) and t2 = d22.(q2) in
    if not (check 0 [r] p) || not (check 1
                                     [q1; q2; s1; s2; t1; t2] p)
    then []
    else big_move c i [q1; q2; s1; s2; t1; t2] in
  (checkS4 i Right Up @
   checkS4 i Left Up @
   checkS4 i Left Down @
   checkS4 i Right Down @
   checkS4 i Down Right @
   checkS4 i Down Left @
   checkS4 i Up Left @
   checkS4 i Up Right) ;;

let solit_bigmoves c =
  let rec sm i=
    if i > 32 then []
    else (Tmoves i c @ Lmoves i c @ S1moves i c @ S2moves i c
          @ S3moves i c @ S4moves i c) @ sm(i + 1)
  in
  sm 0 @ solit_moves c ;;
*)


(** Prise en compte des symetries  *)
let rot =
  [|        12;19;26;
            11;18;25;
            2; 5;10;17;24;29;32;
            1; 4; 9;16;23;28;31;
            0; 3; 8;15;22;27;30;
            7;14;21;
            6;13;20     |] ;;

let rot' =
  [|        20;13; 6;
            21;14; 7;
            30;27;22;15; 8; 3; 0;
            31;28;23;16; 9; 4; 1;
            32;29;24;17;10; 5; 2;
            25;18;11;
            26;19;12     |] ;;

let sym_center =
  [|        32;31;30;
            29;28;27;
            26;25;24;23;22;21;20;
            19;18;17;16;15;14;13;
            12;11;10; 9; 8; 7; 6;
            5; 4; 3;
            2; 1; 0     |] ;;


let sym_diag1 =
  [|        26;19;12;
            25;18;11;
            32;29;24;17;10; 5; 2;
            31;28;23;16; 9; 4; 1;
            30;27;22;15; 8; 3; 0;
            21;14; 7;
            20;13; 6     |] ;;

let sym_diag2 =
  [|         6;13;20;
             7;14;21;
             0; 3; 8;15;22;27;30;
             1; 4; 9;16;23;28;31;
             2; 5;10;17;24;29;32;
             11;16;25;
             12;19;26     |] ;;

let sym_horiz =
  [|        30;31;32;
            27;28;29;
            20;21;22;23;24;25;26;
            13;14;15;16;17;18;19;
            6; 7; 8; 9;10;11;12;
            3; 4; 5;
            0; 1; 2     |] ;;

let sym_vert =
  [|         2; 1; 0;
             5; 4; 3;
             12;11;10; 9; 8; 7; 6;
             19;18;17;16;15;14;13;
             26;25;24;23;22;21;20;
             29;28;27;
             32;31;30     |] ;;

let apply_trans t v v' =
  let rec app i =
    if i > 32 then v'
    else  ( v'.(t.(i)) <- v.(i); app (succ i))
  in
  app 0 ;;

(** +output_result+ *)
let output_int ch n =
  output_string ch (string_of_int n); output_string ch " " ;;

let strofd = function
  | Left -> "Left"
  | Right -> "Right"
  | Up -> "Up"
  | Down -> "Down" ;;

let output_move ch m=
  output_int ch (iconv (fst m)); output_string ch (strofd  (snd m));
  output_string ch " | " ;;

let rec nfirst l n =
  if n = 0 then [] else (List.hd l) :: nfirst (List.tl l) (n - 1) ;;

let rec sub_list l m n =
  if m = 0 then nfirst l n
  else sub_list (List.tl l) (m - 1) n ;;

let output_config ch l=
  output_string ch "    ";
  List.iter (output_int ch) (sub_list l 0 3);
  output_string ch "\n";
  output_string ch "    ";
  List.iter (output_int ch) (sub_list l 3 3);
  output_string ch "\n";
  List.iter (output_int ch) (sub_list l 6 7);
  output_string ch "\n";
  List.iter (output_int ch) (sub_list l 13 7);
  output_string ch "\n";
  List.iter (output_int ch) (sub_list l 20 7);
  output_string ch "\n";
  output_string ch "    ";
  List.iter (output_int ch) (sub_list l 27 3);
  output_string ch "\n";
  output_string ch "    ";
  List.iter (output_int ch) (sub_list l 30 3) ;;

let output_result ch { size = k; moves = ml; board = p } =
  output_string ch "Moves: \n";
  List.iter (output_move ch) (List.rev ml);
  output_string ch "\n \n";
  output_string ch "Final Configuration: \n";
  output_config ch (uncompact  p);
  print_newline () ;;

let output_result = output_result stdout ;;

let int_comp i1 i2 =
  if i1 < i2 then Smaller else
  if i1 > i2 then Greater
  else Equiv ;;

(** +type_archive+ *)
type 'a archive = Arch of 'a set array ;;

(** +archive+ *)
let add_to_archive (Arch arch) (n1, n2) =
  (arch.(n1) <- add_to_set arch.(n1) n2); Arch arch ;;

let add_list_to_archive = List.fold_left add_to_archive ;;

let make_archive  comp =
  add_list_to_archive (Arch (Array.make 8 (make_set comp []))) ;;

let archive_member (n1, n2) (Arch arch) = set_member n2 (arch.(n1)) ;;

(*
let buff = Array.make 33 0 ;;
let buff' = Array.make 33 0 ;;

let uncompactv buff (m, n) =
  let rec unsave n i =
  if i<3 then () else (buff.(i) <- n mod 2; unsave (n lsr 1) (i-1))
  in
  (buff.(2) <- (m lsr 2) mod 2 ;
   buff.(1) <- (m lsr 1) mod 2 ;
   buff.(0) <-  m mod 2 ;
   unsave n 32 ;
   buff) ;;

let archive_member (n1,n2) (Arch arch) = set_member n2 (arch.(n1))
   || (uncompactv buff (n1,n2);
       ((apply_trans rot buff buff';
         let (n1,n2) = compactv buff' in set_member n2 (arch.(n1)))
        ||
        (apply_trans rot' buff buff';
         let (n1,n2) = compactv buff' in set_member n2 (arch.(n1)))
        ||
        (apply_trans sym_center buff buff';
         let (n1,n2) = compactv buff' in set_member n2 (arch.(n1)))
        ||
        (apply_trans sym_diag1 buff buff';
         let (n1,n2) = compactv buff' in set_member n2 (arch.(n1)))
        ||
        (apply_trans sym_diag2  buff buff';
         let (n1,n2) = compactv buff' in set_member n2 (arch.(n1)))
        ||
        (apply_trans sym_horiz buff buff';
         let (n1,n2) = compactv buff' in set_member n2 (arch.(n1)))
        ||
        (apply_trans sym_vert buff buff';
         let (n1,n2) = compactv buff' in set_member n2 (arch.(n1))))) ;;
*)


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
  let rec solve_rec a cl n p = match cl with
    | [] -> raise No_solution
    | (c :: cl') ->
      if ok c then (a, c, n, p) else
      if archive_member (arch_part c) a then solve_rec a cl' n (p + 1)
      else ((if n land 127 = 0
             then (print_int n; print_string " " ;
                   print_int p; print_string " " ;
                   print_int (List.length cl); print_newline ())) ;
            solve_rec (add_to_archive a (arch_part c))
              (pos_moves c @ cl') (n + 1) (p + 1))
  in
  solve_rec (make_archive comp []) [c] 0 0 ;;

(** +solve_depth_first_solit_game+ *)
let solve_depth_first_solit_game ok =
  solve_depth_first (ok, solit_moves, int_comp) (fun c -> c.board) ;;

(** +ok_solit+ *)
let ok_solit cgoal c = (c.board = cgoal) ;;

let b1  = compact [        0;0;0;
                           0;0;0;
                           1;1;0;0;0;0;0;
                           1;1;1;0;0;0;0;
                           1;1;0;0;0;0;0;
                           0;0;0;
                           0;0;0     ] ;;

let b2  = compact [        0;0;0;
                           0;0;0;
                           0;0;0;0;0;0;0;
                           0;0;1;0;0;0;0;
                           0;0;0;0;0;0;0;
                           0;0;0;
                           0;0;0     ] ;;
(*
output_result
   (solve_depth_first_solit_game
       (ok_solit b2) { size = 7; moves = []; board = b1 }) ;;
*)

let b3  = compact [        1;1;1;
                           1;1;1;
                           1;1;0;0;1;0;0;
                           1;1;1;1;1;1;0;
                           1;1;0;0;1;0;0;
                           1;1;1;
                           0;0;0     ] ;;

let b4  = compact [        0;0;0;
                           0;0;0;
                           0;0;0;0;0;0;0;
                           0;0;0;1;0;0;0;
                           0;0;0;0;0;0;0;
                           0;0;0;
                           0;0;0     ] ;;
(*
b3 : int * int = 7, 119995551
b4 : int * int = 0, 8192
*)
(*
output_result
   (solve_depth_first_solit_game
       (ok_solit b4) { size = 21; moves = []; board = b3 }) ;;
*)
(*
Moves:
33 Right | 31 Up | 32 Up | 34 Right | 33 Left | 34 Down | 32 Left | 33
Right | 35 Down | 25 Up | 53 Down | 33 Down | 43 Up | 53 Down | 63 Left |
43 Up | 45 Right | 35 Up | 45 Down | 44 Left |

Final Configuration:
    0 0 0
    0 0 0
0 0 0 0 0 0 0
0 0 0 1 0 0 0
0 0 0 0 0 0 0
    0 0 0
    0 0 0


- : unit = ()
*)


let start =   compact [        1;1;1;
                               1;1;1;
                               1;1;1;1;1;1;1;
                               1;1;1;0;1;1;1;
                               1;1;1;1;1;1;1;
                               1;1;1;
                               1;1;1      ] ;;

let cross   = compact [        1;1;1;
                               0;1;0;
                               1;0;0;1;0;0;1;
                               1;1;1;0;1;1;1;
                               1;0;0;1;0;0;1;
                               0;1;0;
                               1;1;1      ] ;;


let solve_depth_first (ok, pos_moves, comp) cond arch_part c =
  let rec solve_rec a cl n p = match cl with
    | [] -> raise No_solution
    | (c :: cl') ->
      if ok c then (a, c, n, p) else
      if cond c || archive_member (arch_part c) a
      then solve_rec a cl' n (p + 1)
      else ((if n land 127 = 0
             then (print_int n; print_string " " ;
                   print_int p; print_string " " ;
                   print_int (List.length cl); print_newline ())) ;
            solve_rec (add_to_archive a (arch_part c))
              (pos_moves c @ cl') (n + 1) (p + 1))
  in
  solve_rec (make_archive comp []) [c] 0 0 ;;


let solve_depth_first_solit_game ok cond =
  solve_depth_first (ok, solit_moves, int_comp) cond (fun c -> c.board) ;;


let ok_solit c = (c.board = cross) ;;

(* A VOIR
   output_result
   (solve_depth_first_solit_game
      ok_solit  (fun c -> c.size < 18) { size = 32; moves = []; board = start }) ;;
*)


(** Nouvelle version ou on archive aussi les configurations symetriques *)

let buff = Array.make 33 0 ;;
let buff' = Array.make 33 0 ;;

let uncompactv buff (m, n) =
  let rec unsave n i =
    if i < 3 then () else (buff.(i) <- n mod 2; unsave (n lsr 1) (i - 1))
  in
  (buff.(2) <- (m lsr 2) mod 2 ;
   buff.(1) <- (m lsr 1) mod 2 ;
   buff.(0) <-  m mod 2 ;
   unsave n 32 ;
   buff) ;;

let add_to_archive (Arch arch) (n1, n2) =
  ignore (uncompactv buff (n1, n2) ) ;
  arch.(n1) <- add_to_set arch.(n1) n2 ;
  List.iter (fun t -> ignore (apply_trans t buff buff') ;
              let (n1, n2) = compactv buff' in
              arch.(n1) <- add_to_set arch.(n1) n2)
    [rot; rot'; sym_center; sym_diag1; sym_diag2 ;
     sym_horiz; sym_vert ] ;
  Arch arch ;;

let solve_depth_first (ok, pos_moves, comp) cond arch_part c =
  let rec solve_rec a cl n p = match cl with
    | [] -> raise No_solution
    | (c :: cl') ->
      if ok c then (a, c, n, p) else
      if cond c || archive_member (arch_part c) a
      then solve_rec a cl' n (p + 1)
      else ((if n land 127 = 0
             then (print_int n; print_string " " ;
                   print_int p; print_string " " ;
                   print_int (List.length cl); print_newline ())) ;
            solve_rec (add_to_archive a (arch_part c))
              (pos_moves c @ cl') (n + 1) (p + 1))
  in
  solve_rec (make_archive comp []) [c] 0 0 ;;

let solve_depth_first_solit_game ok cond =
  solve_depth_first (ok, solit_moves, int_comp) cond (fun c -> c.board) ;;


(* A VOIR
   output_result
   (solve_depth_first_solit_game
       ok_solit (fun c -> c.size < 18) { size = 32; moves = []; board = start }) ;;
*)

(* A VOIR
   let archive_member (n1, n2) (Arch arch) = set_member n2 (arch.(n1))
   || ( ignore (uncompactv buff (n1, n2)) ;
       ((ignore (apply_trans rot buff buff') ;
         let (n1, n2) = compactv buff' in set_member n2 (arch.(n1)))
        ||
        (apply_trans rot' buff buff' ;
         let (n1, n2) = compactv buff' in set_member n2 (arch.(n1)))
        ||
        (apply_trans sym_center buff buff' ;
         let (n1, n2) = compactv buff' in set_member n2 (arch.(n1)))
        ||
        (apply_trans sym_diag1 buff buff' ;
         let (n1, n2) = compactv buff' in set_member n2 (arch.(n1)))
        ||
        (apply_trans sym_diag2  buff buff' ;
         let (n1, n2) = compactv buff' in set_member n2 (arch.(n1)))
        ||
        (apply_trans sym_horiz buff buff' ;
         let (n1, n2) = compactv buff' in set_member n2 (arch.(n1)))
        ||
        (apply_trans sym_vert buff buff' ;
         let (n1, n2) = compactv buff' in set_member n2 (arch.(n1))))) ;;
*)
