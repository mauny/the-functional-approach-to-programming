#open "prelude";;

#infix "o";;

(* +iter1+ *)

let rec iter n f = if n=0 then id else f o (iter (n-1) f);;

(* +iter1+ *)
(* +iter2+ *)

let rec iter n f x = if n=0 then x else f(iter (n-1) f x);;

(* +iter2+ *)
(* +loop+ *)

let rec loop p f x = if (p x) then x else loop p f (f x);;

(* +loop+ *)
(* +dicho+ *)

let dicho (f,a,b,epsilon) =
  loop is_ok do_better (a,b)
  where is_ok(a,b) = abs_float(b-.a) <. epsilon
  and do_better(a,b) =
    let c = (a +. b) /. 2.0 in
    if f(a) *. f(c) >. 0.0 then (c,b)
    else (a,c)
;;

(* +dicho+ *)
(* +deriv+ *)

let deriv (f,dx) x = (f(x+.dx)-.f(x))/.dx;;

(* +deriv+ *)
(* +newton1+ *)

let newton(f,start,dx,epsilon) =
  loop is_ok do_better start
  where is_ok x = abs_float (f x) <. epsilon
  and do_better x =
    let f' = deriv (f,dx) in
    (x -. f x /. f' x)
;;

(* +newton1+ *)
(* +newton2+ *)

let newton(f,start,dx,epsilon) =
  let f' = deriv (f,dx) in
  let is_ok x = abs_float (f x) <. epsilon
  and do_better x = x -. f x /. f' x in
  loop is_ok do_better start;;

(* +newton2+ *)
(* +sigma+ *)

let rec sigma f (a,b) =
  if a > b then 0
  else (f a) + sigma f (a+1,b);;

(* +sigma+ *)
(* +summation+ *)

let rec summation (incr,test) (op,e) f a =
  if test a then e
  else op (f a) (summation (incr,test) (op,e) f (incr a)) ;;

(* +summation+ *)
(* +sum+ *)

let sum (op,e) f a b dx =
  summation ((fun x -> x +. dx), (fun x -> x >. b)) (op,e) f a;;

(* +sum+ *)
(* +integrate+ *)

let integrate f a b dx =
  sum (prefix +., 0.) (fun x -> f(x) *. dx) a b dx;;

(* +integrate+ *)
(* +summation_int+ *)

let summation_int (op,e) f a b =
  summation ((fun x -> x+1), (fun x -> x>b)) (op,e) f a;;

(* +summation_int+ *)
