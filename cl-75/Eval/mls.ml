#directory "../Util";;

#open "lexer";;

#open "parser_strict";;

#open "eval_strict";;

#open "ml_strict";;

let rec print_val = function
  Int_Const n -> print_int n
| Bool_Const true -> print_string "true"
| Bool_Const false -> print_string "false"
| Pair(v1,v2) ->
      print_string "("; print_val v1;
      print_string ", "; print_val v2;
      print_string ")"
| Clo _ -> print_string "<funval>"
;;

let strict_eval () =
  print_string "Enter a phrase, and terminate with ^D on a line by itself\n";
  flush stdout;
  try let e = ml_parser (lexer (stream_of_channel stdin)) in
      print_string "=> "; flush stdout;
      let v = ml_eval [] e in
      print_val v;
      print_newline();
      exit 0
  with Failure msg -> prerr_string msg; prerr_string "\n"; exit 1
     | _ -> prerr_string "Error\n"; exit 1
;;

strict_eval ();;
