(* +parsed+ *)

type ('a, 'b) parsed =
       Returns of 'b * ('a list)
     | AnalyzeFails
;;

(* +parsed+ *)
(* +parser+ *)

type ('a, 'b) parser == 'a list -> ('a,'b) parsed;;

(* +parser+ *)
(* +option+ *)

type 'a option =
       Some of 'a
     | None
;;

(* +option+ *)

#uninfix "gives";;

(* +gives+ *)

let (gives : ('a, 'b) parser -> ('b -> 'c) -> ('a, 'c) parser) =
  fun p f toks ->
        match p toks with
          Returns(r1, toks1) -> Returns(f r1, toks1)
        | AnalyzeFails -> AnalyzeFails
;;

#infix "gives";;

(* +gives+ *)
(* +empty+ *)

let (empty : 'a -> ('b,'a) parser) = fun v toks -> Returns (v, toks)
;;

(* +empty+ *)
(* +token+ *)

let (token : ('a -> 'b option) -> ('a, 'b) parser) = fun
  test (t :: ts) -> (match test t with
                       Some r -> Returns (r, ts)
                     | None -> AnalyzeFails)
| _ _ -> AnalyzeFails
;;

(* +token+ *)
(* +orelse+ *)

let (orelse : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser) =
  fun p1 p2 toks ->
        match p1 toks with
          AnalyzeFails -> p2 toks
        | res -> res
;;

#infix "orelse";;

(* +orelse+ *)

#uninfix "andalso";;

(* +andalso+ *)

let (andalso : ('a, 'b) parser -> ('a,'c) parser -> ('a, ('b * 'c)) parser) =
  fun p1 p2 toks ->
        (match p1 toks with
           Returns(r1, toks1) -> (match p2 toks1 with
                                    Returns(r2, toks2) ->
                                      Returns((r1, r2), toks2)
                                  | _ -> AnalyzeFails)
         | _ -> AnalyzeFails)
;;

#infix "andalso";;

(* +andalso+ *)
(* +zero_or_more+ *)

let rec zero_or_more p =
  ((fun toks -> (((p andalso (zero_or_more p)) gives (fun (x,xs) -> x::xs))
                 orelse (empty [])) toks) : ('a, 'b list) parser)
;;

(* +zero_or_more+ *)
(* +any+ *)

let any v = token (fun _ -> Some v)
;;

(* +any+ *)
(* +optional+ *)

let optional p v = p orelse (empty v)
;;

(* +optional+ *)
(* +one_or_more+ *)

let one_or_more p = p andalso (zero_or_more p)
;;

(* +one_or_more+ *)
(* +or_list+ *)

let or_list p pl = it_list (prefix orelse) p pl
;;

(* +or_list+ *)
(* +and_list+ *)

let and_list pl =
  list_it (fun p1 p2 -> (p1 andalso p2) gives (fun (x,xs) -> x::xs))
     pl (empty [])
;;

(* +and_list+ *)
(* +iterate+ *)

let rec iterate p n = match n with
  0 -> empty []
| n -> (p andalso (iterate p (n-1))) gives (fun (x,xs) -> x::xs)
;;

(* +iterate+ *)


