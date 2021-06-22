let explode s =
  let res = ref [] in
  for i = 0 to String.length s - 1 do
    res := String.get s i :: !res
  done;
  List.rev (!res) ;;

let implode l =
  let n = List.length l in
  let res = Bytes.make n ' ' in
  let rl = ref l in
  for i = 0 to n-1 do
    Bytes.set res i (List.hd !rl);
    rl := List.tl !rl
  done;
  Bytes.to_string res ;;
