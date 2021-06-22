(*************************************************************************)
(*                                                                       *)
(*                     Projet      Formel                                *)
(*                                                                       *)
(*                  Objective CAML: MLgraph library                      *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            LIENS                                      *)
(*                        45 rue d'Ulm                                   *)
(*                         75005 PARIS                                   *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)

(* $Id: compatibility.mlp,v 1.3 1997/08/14 14:38:52 emmanuel Exp $ *)
(*  compatibility  with Objective Caml                                   *)
(*                    Emmanuel Chailloux & Guy Cousineau                 *)
(*                    Fri Feb 16  1996                                   *)






let lt_int x y = (x<y);;
let lt_float x y = (x<y);;
let le_float x y = (x<=y);;
let gt_int x y = (x>y);;
let gt_float x y = (x>y);;
let mult_float x y = x*.y;;
let add_int x y = x+y;;

let int_of_char = Char.code;;
let char_of_int = Char.chr;;
let space_char = ' ';;
let lf_char = '\010';;
let char_0 = '0';;
let comma_char = ',';;
let open_par_char = '(';;
let close_par_char = ')';;
let ascii_0 = int_of_char '0'
and ascii_9 = int_of_char '9'
and ascii_a = int_of_char 'a'
and ascii_f = int_of_char 'f'
and ascii_A = int_of_char 'A'
and ascii_F = int_of_char 'F'
;;

let eq_string (a:string) (b:string) = a = b;;
let string_length = String.length;;
let sub_string = String.sub;;
let map=List.map;;
let map2=List.map2;;
let nth_char = String.get;;
let set_nth_char = Bytes.set;;
let create_string = Bytes.create;;
let make_string = String.make;;
let it_list = List.fold_left;;
let list_it = List.fold_right;;
let append l1 l2 = l1 @ l2;;
let list_length = List.length;;
let int_of_float = truncate;;
let float_of_int = float;;
let rev = List.rev;;
let do_list = List.iter;;
let do_list2 = List.iter2;;
type 'a vect = 'a array;;
let mem = List.mem;;
let vect_of_list = Array.of_list;;
let make_vect = Array.make;;
let rec except l a =
match l with
   []  -> []
| (x::l') -> if x=a then except l' a
                else x::except l' a;;
let rec subtract l1 l2 =
match l2 with
  []  -> l1
| (a::l) -> subtract (except l1 a) l;;
let assoc=List.assoc;;
let index x l =
let rec ind  l n =
match l with
 []   ->   failwith "index: not found"
| (a::l) -> if a=x then n else 1+ind l (n+1)
in ind l 0;;
let replace_string dest src start =
 String.blit src 0 dest start (string_length src);;
let format_float  = Printf.sprintf ;;
let std_out = stdout;;
let assq = List.assq;;
let combine=List.combine;;
let hd=List.hd;;
let tl=List.tl;;

let vect_length = Array.length;;
