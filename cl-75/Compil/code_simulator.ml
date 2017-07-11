(* +type_instruction+ *)
type instruction =
    STOP
  | LOAD of val
  | PUSH of val
  | DUPL  | SWAP  | ROT3 | IROT3
  | FST | SND | SETFST | SETSND 
  | CONS | SPLIT
  | ADD | SUB | MULT | EGAL
  | CALL | RETURN 
  | BRANCH of val * val

and val =
     Int_Const of int
  |  Bool_Const of bool
  |  Clo  of val ref * instruction list     (*> fermeture *)
  |  Nil 
  |  Pair of val ref * val ref
  |  Adr of instruction list;;
(* +type_instruction+ *)


(* +exec+ *)
exception Exec_error;;
let rec exec = fun
   ([STOP],[v]) -> v
 | ((LOAD v)::code,v'::stack) -> exec (code,v::stack)
 | ((PUSH v)::code,stack) -> exec (code,v::stack)
 | (DUPL::code,v::stack) -> exec (code,v::v::stack)
 | (SWAP::code,v::v'::stack) -> exec(code,v'::v::stack)
 | (ROT3::code,v1::v2::v3::stack) -> exec(code,v2::v3::v1::stack)
 | (IROT3::code,v1::v2::v3::stack) -> exec(code,v3::v1::v2::stack)
 | (FST::code,(Pair(ref v1,_))::stack)
     ->  exec(code,v1::stack)
 | (SND::code,(Pair(_,ref v2))::stack)
     ->  exec(code, v2::stack)
 | (SETFST::code, v::(Pair(v1,v2) as p)::stack)
     ->  v1:=v;exec (code,p::stack)
 | (SETSND::code, v::(Pair(v1,v2)as p)::stack)
     ->  v2:=v;exec (code,p::stack)
 | (CONS::code, v1:: v2::stack)
     -> exec(code,(Pair(ref v1,ref v2))::stack)
 | (SPLIT::code, (Pair(ref v1,ref v2))::stack)
     ->  exec(code, v1::v2::stack)
 | (ADD::code,(Int_Const v1)::(Int_Const v2)::stack)
     -> exec(code, (Int_Const(v1+v2))::stack)
 | (SUB::code,(Int_Const v1)::(Int_Const v2)::stack)
     -> exec(code,(Int_Const(v1-v2))::stack)
 | (MULT::code,(Int_Const v1)::(Int_Const v2)::stack)
     -> exec(code, (Int_Const(v1*v2))::stack)
 | (EGAL::code,(Int_Const v1)::(Int_Const v2)::stack)
     -> exec(code, (Bool_Const(v1=v2))::stack)
 | (CALL::code,(Adr code')::v::stack)
     -> exec(code',  v::(Adr code)::stack)
 | (RETURN::code,v::(Adr code')::stack)
     -> exec(code',v::stack)
 | (BRANCH(adr1,adr2)::code,(Bool_Const b)::stack)
     -> if b then exec(code,adr1::stack)
             else exec(code,adr2::stack)
 | _  -> raise Exec_error;;
(* +exec+ *)
