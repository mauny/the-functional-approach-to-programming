#open "circular_list";;

(* +type_queue+ *)
type 'a queue = Emptyqueue | Queue of 'a lnode;;
(* +type_queue+ *)


value enqueue : 'a -> 'a queue -> 'a queue;;
value dequeue : 'a queue -> 'a queue * 'a;;
value list_of_queue : 'a queue -> 'a list;;
