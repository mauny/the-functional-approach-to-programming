
(* +cpermut+ *)
let cpermut v v'=
  let n = vect_length v and n' = vect_length v' in
  if n <> n' then failwith "cpermut: non compatible args"
  else let w = make_vect n 0 in
       (try  cp (n-1)
        with _ -> failwith "cpermut: wrong arg")
                where rec cp=
                 fun 0 -> (w.(0)<-v'.(v.(0));w)
                  |  i ->  (w.(i)<-v'.(v.(i)); cp(i-1));;

(* +cpermut+ *)
(* +id_permut+ *)

let id_permut k =
  let w = make_vect k 0
  in ic (k-1)
     where rec ic = fun
       0 -> (w.(0)<-0; w)
     | i -> (w.(i)<-i; ic(i-1));;

(* +id_permut+ *)
