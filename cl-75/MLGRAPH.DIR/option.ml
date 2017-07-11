(*                                                                       *)
(*                     Projet      Formel                                *)
(*                                                                       *)
(* CAML-light: MLgraph library *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            LIENS                                      *)
(*                        45 rue d'Ulm                                   *)
(*                         75005 PARIS                                   *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)

(* $Id: option.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* option.ml       Option handling for mlPicTex                          *)
(*                 Ascander Suarez                                       *)
(*                 Mon Jun 21 1993                                       *)





#open  "MLgraph";;

#open "compatibility";;
#open "prelude";;

#open "pictures";;




let string name s = name,SOption s;;
let bool name b = name,BOption b;;
let int name i = name,IOption i;;
let float name i = name,FOption i;;
let option name = name,POption;;
let color name c = name,COption c;;
let dashPattern l = "dashPattern",DOption l;;
let join j = "lineJoin",JOption j;;
let cap c = "lineCap",CapOption c;;
let fillStyle f = "fillStyle",FillOption f;;
let lineLabel name pos = "lineLabel",LOption(name,pos);;
let font f = "font",FontOption f;;


exception OptionError;;
let theString ol name default =
   try match assoc name ol with SOption b -> b | _ -> raise OptionError
   with Not_found -> default;;

let theInt ol name default =
   try match assoc name ol with IOption b -> b | _ -> raise OptionError
   with Not_found -> default;;

let theFloat ol name default =
   try match assoc name ol with FOption b -> b | _ -> raise OptionError
   with Not_found -> default;;

let theBool ol name default =
   try match assoc name ol with BOption b -> b | _ -> raise OptionError
   with Not_found -> default;;
   
let theOption ol name =
   try assoc name ol;true
   with Not_found -> false;;

let theColor ol name default =
   try match assoc name ol with COption b -> b | _ -> raise OptionError
   with Not_found -> default;;

let theDashPattern ol default =
   try match assoc "dashPattern" ol with DOption b -> b | _ -> raise OptionError
   with Not_found -> default;;

let theLineJoin ol  default =
   try match assoc "lineJoin" ol with JOption b -> b | _ -> raise OptionError
   with Not_found -> default;;

let theLineCap ol default =
   try match assoc "lineCap" ol with CapOption b -> b | _ -> raise OptionError
   with Not_found -> default;;

let theFillStyle ol default =
   try match assoc "fillStyle" ol with FillOption b -> b | _ -> raise OptionError
   with Not_found -> default;;

let theLineLabel ol =
   try match assoc "lineLabel"  ol with LOption (a,b) as c -> c | _ -> raise OptionError
   with Not_found -> raise OptionError;;

let theFont ol default =
   try match assoc "font" ol with FontOption b -> b | _ -> raise OptionError
   with Not_found -> default;;

let findOption ol name = assoc name ol;;

(*    END    *)
