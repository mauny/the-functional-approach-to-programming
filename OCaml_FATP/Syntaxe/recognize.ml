open Astexpr

(** +remaining+ *)
type 'a remaining =
  | Remains of 'a list
  | Fails ;;

(** +recognizer+ *)
type 'a recognizer = 'a list -> 'a remaining ;;

(** +empty+ *)
let (empty : 'a recognizer) = fun toks -> Remains toks ;;

(** +token+ *)
let token (test: 'a -> bool) : 'a recognizer = function
  |  (t :: ts) -> if test t then Remains ts else Fails
  | _ -> Fails ;;

(** +any+ *)
let any x =
  token (fun _ -> true) x ;;

(** +orelse+ *)
let (orelse : 'a recognizer -> 'a recognizer -> 'a recognizer) =
  fun p1 p2 toks -> match p1 toks with
    | Fails -> p2 toks
    | res -> res ;;

let ( |$ ) = orelse ;;

(** +optional+ *)
let optional p =
  p |$ empty ;;

(** +andalso+ *)
let (andalso : 'a recognizer -> 'a recognizer -> 'a recognizer) =
  fun  p1 p2 toks ->
  match p1 toks with
  | Remains toks1 -> p2 toks1
  | _ -> Fails ;;

let ( &$ ) = andalso ;;

(** +zero_or_more+ *)
let rec zero_or_more p =
  (fun toks -> ((p &$ (zero_or_more p))
                |$ empty) toks : 'a recognizer) ;;

(** +one_or_more+ *)
let one_or_more p =
  p &$ (zero_or_more p) ;;

(** +or_list+ *)
let or_list p pl = List.fold_left ( |$ ) p pl ;;

(** +and_list+ *)
let and_list pl =
  List.fold_right ( &$ ) pl empty ;;

(** +iterate+ *)
let rec iterate p n = match n with
  | 0 -> empty
  | n -> p &$ (iterate p (n-1)) ;;
