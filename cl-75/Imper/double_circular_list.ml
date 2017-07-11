
(* +mk_dbl_circular_list+ *)
let mk_dbl_circular_list  e =
  let rec x = {info=e; prev=x; next=x} in x;;
(* +mk_dbl_circular_list+ *)



(* +insert_before_after+ *)
let insert_before e l =
  let lprev = l.prev in
  let x = {info=e; prev=lprev; next=l} in
  lprev.next<-x;
  l.prev<-x;;

let insert_after e l =
  let lnext = l.next in
  let x = {info=e; prev=l; next=lnext} in
  lnext.prev<-x;
  l.next<-x;;
(* +insert_before_after+ *)


(* +elim+ *)
let elim l =
  let lprev = l.prev
  and lnext = l.next
  in lprev.next<-lnext;
     lnext.prev<-lprev; lprev;;
(* +elim+ *)

