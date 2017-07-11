(* +mk_circular_list+ *)
let mk_circular_list e=
  let rec x={info=e; next=x} in x;;
(* +mk_circular_list+ *)


(* +last_first+ *)
let last ln= ln.info;;
let first ln = (ln.next).info;;
(* +last_first+ *)


(* +insert_head_tail+ *)
let insert_head e l=
  let x={info=e; next=l.next}
  in l.next<-x;l;;

let insert_tail e l=
  let x={info=e; next=l.next}
  in l.next<-x;x;;
(* +insert_head_tail+ *)


(* +elim_head+ *)
let elim_head l=
  l.next<-(l.next).next;l;;
(* +elim_head+ *)
