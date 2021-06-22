(* #use "load.ml" ;; *)

open Circular_list

(** +type_queue+ *)
type 'a queue = Emptyqueue | Queue of 'a lnode ;;

(** +enqueue+ *)
let enqueue x = function
  | Emptyqueue -> Queue (mk_circular_list x)
  | (Queue ln) -> Queue (insert_tail x ln) ;;

(** +dequeue+ *)
let dequeue  = function
  | Emptyqueue -> failwith "dequeue: queue is empty"
  | (Queue ln) -> if ln.next == ln  then (Emptyqueue, ln.info)
    else let x = first ln in
      (Queue (elim_head ln), x) ;;

(** +list_of_queue+ *)
let list_of_queue = function
  | Emptyqueue   ->   []
  | (Queue ln) -> let ln1 = ln.next in
    let rec loq ln =
      if ln == ln1 then []
      else ln.info :: loq  ln.next
    in
    ln1.info :: loq ln1.next ;;
