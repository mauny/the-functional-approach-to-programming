(** +cpermut+ *)
let cpermut v v'=
  let n = Array.length v and n' = Array.length v' in
  if n <> n' then failwith "cpermut: non compatible args"
  else let w = Array.make n 0 in
    (try let rec cp = function
         | 0 -> (w.(0) <- v'.(v.(0)); w)
         | i -> (w.(i) <- v'.(v.(i)); cp (i-1))
        in
        cp (n-1)
     with _ -> failwith "cpermut: wrong arg") ;;

(** +id_permut+ *)
let id_permut k =
  let w = Array.make k 0
  in
  let rec ic = function
    | 0 -> (w.(0) <- 0; w)
    | i -> (w.(i) <- i; ic (i-1))
  in ic (k-1) ;;
