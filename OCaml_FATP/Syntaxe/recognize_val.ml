(** +parsed+ *)
type ('a, 'b) parsed =
    Returns of 'b * ('a list)
  | AnalyzeFails ;;

(** +parse+ *)
type ('a, 'b) parse = 'a list -> ('a, 'b) parsed ;;

(** +gives+ *)
let (gives : ('a, 'b) parse -> ('b -> 'c) -> ('a, 'c) parse) =
  fun p f toks ->  match p toks with
    | Returns (r1, toks1) -> Returns (f r1, toks1)
    | AnalyzeFails -> AnalyzeFails ;;

let ( %$ ) = gives ;;

(** +empty+ *)
let (empty : 'a -> ('b, 'a) parse) = fun v toks -> Returns (v, toks) ;;

(** +token+ *)
let token (test: 'a -> 'b option) : ('a, 'b) parse = function
  | (t :: ts) -> (match test t with
      | Some r -> Returns (r, ts)
      | None -> AnalyzeFails)
  | _ -> AnalyzeFails ;;

(** +orelse+ *)
let (orelse : ('a, 'b) parse -> ('a, 'b) parse -> ('a, 'b) parse) =
  fun p1 p2 toks ->
  match p1 toks with
  | AnalyzeFails -> p2 toks
  | res -> res ;;

let ( |$ ) = orelse ;;

(** +andalso+ *)
let (andalso : ('a, 'b) parse -> ('a, 'c) parse -> ('a, ('b * 'c)) parse) =
  fun p1 p2 toks ->
  (match p1 toks with
   | Returns (r1, toks1) -> (match p2 toks1 with
       | Returns (r2, toks2) ->
         Returns ((r1, r2), toks2)
       | _ -> AnalyzeFails)
   | _ -> AnalyzeFails) ;;

let ( &$ ) = andalso ;;

(** +zero_or_more+ *)
let rec zero_or_more p =
  ((fun toks -> (((p &$ (zero_or_more p)) %$ (fun (x, xs) -> x :: xs))
                 |$ (empty [])) toks) : ('a, 'b list) parse) ;;

(** +any+ *)
let any v = token (fun _ -> Some v) ;;

(** +optional+ *)
let optional p v = p |$ (empty v) ;;

(** +one_or_more+ *)
let one_or_more p = p &$ (zero_or_more p) ;;

(** +or_list+ *)
let or_list p pl = List.fold_left ( |$ ) p pl ;;

(** +and_list+ *)
let and_list pl =
  List.fold_right (fun p1 p2 -> (p1 &$ p2) %$ (fun (x, xs) -> x :: xs))
    pl (empty []) ;;

(** +iterate+ *)
let rec iterate p n = match n with
  | 0 -> empty []
  | n -> (p &$ (iterate p (n-1))) %$ (fun (x, xs) -> x :: xs) ;;
