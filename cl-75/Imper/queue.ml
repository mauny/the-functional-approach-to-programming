#open "circular_list";;

(* +enqueue+ *)
let enqueue x =  fun
         Emptyqueue -> Queue (mk_circular_list x)
   |    (Queue ln) -> Queue (insert_tail x ln);;
(* +enqueue+ *)


(* +dequeue+ *)
let dequeue  = fun
       Emptyqueue -> failwith "dequeue: queue is empty"
   |   (Queue ln) -> if ln.next == ln  then (Emptyqueue,ln.info)
                     else let x = first ln in
                           (Queue (elim_head ln),x);;
(* +dequeue+ *)


(* +list_of_queue+ *)
let list_of_queue = fun
       Emptyqueue   ->   []
   |   (Queue ln)  
          -> let ln1 = ln.next in
             ln1.info :: loq  ln1.next
               where rec loq ln =
                   if ln == ln1 then []
                      else ln.info :: loq  ln.next;;
(* +list_of_queue+ *)




