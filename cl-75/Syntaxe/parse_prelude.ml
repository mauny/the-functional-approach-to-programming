let explode s =
  let res = ref [] in
  for i=0 to string_length s - 1 do
    res := nth_char s i :: ! res
  done;
  rev(!res)
;;

let implode l =
  let n = list_length l in
  let res = make_string n ` ` in
  let rl = ref l in
  for i=0 to n-1 do
    set_nth_char res i (hd !rl);
    rl := tl !rl
  done;
  res
;;
