#open "astexpr";;

(* +remaining+ *)

type 'a remaining =
       Remains of 'a list
     | Fails
;;

(* +remaining+ *)
(* +recognizer+ *)

type 'a recognizer == 'a list -> 'a remaining;;

(* +recognizer+ *)
(* +empty+ *)

let (empty : 'a recognizer) = fun toks -> Remains toks
;;

(* +empty+ *)
(* +token+ *)

let (token : ('a -> bool) -> 'a recognizer) = fun
  test (t :: ts) -> if test t then Remains ts else Fails
| _ _ -> Fails
;;

(* +token+ *)
(* +any+ *)

let any x =
  token (fun _ -> true) x
;;

(* +any+ *)
(* +orelse+ *)

let (orelse : 'a recognizer -> 'a recognizer -> 'a recognizer) =
  fun p1 p2 toks -> match p1 toks with
                      Fails -> p2 toks
                    | res -> res
;;

#infix "orelse";;

(* +orelse+ *)
(* +optional+ *)

let optional p =
  p orelse empty
;;

(* +optional+ *)
(* +andalso+ *)

let (andalso : 'a recognizer -> 'a recognizer -> 'a recognizer) =
  fun p1 p2 toks ->
        match p1 toks with
          Remains toks1 -> p2 toks1
        | _ -> Fails
;;

#infix "andalso";;

(* +andalso+ *)
(* +zero_or_more+ *)

let rec zero_or_more p =
  (fun toks -> ((p andalso (zero_or_more p))
                orelse empty) toks : 'a recognizer)
;;

(* +zero_or_more+ *)
(* +one_or_more+ *)

let one_or_more p =
  p andalso (zero_or_more p)
;;

(* +one_or_more+ *)
(* +or_list+ *)

let or_list p pl = it_list (prefix orelse) p pl
;;

(* +or_list+ *)
(* +and_list+ *)

let and_list pl =
  list_it (prefix andalso) pl empty
;;

(* +and_list+ *)
(* +iterate+ *)

let rec iterate p n = match n with
  0 -> empty
| n -> p andalso (iterate p (n-1))
;;

(* +iterate+ *)


