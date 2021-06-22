(** +type_lnode+ *)
type 'a lnode = { info: 'a; mutable next: 'a lnode } ;;

(** +mk_circular_list+ *)
let mk_circular_list e =
  let rec x = { info = e; next = x } in x ;;

(** +last_first+ *)
let last ln = ln.info ;;
let first ln = (ln.next).info ;;

(** +insert_head_tail+ *)
let insert_head e l =
  let x = { info = e; next = l.next }
  in l.next <- x; l ;;

let insert_tail e l =
  let x = { info = e; next = l.next }
  in l.next <- x; x ;;

(** +elim_head+ *)
let elim_head l =
  l.next <- (l.next).next; l ;;
