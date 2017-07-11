




















#open  "MLgraph";;
#open "compatibility";;
#open "prelude";;
#open "pictures";;


value string : 'a -> string -> 'a * option;;
value bool : 'a -> bool -> 'a * option;;
value int : 'a -> int -> 'a * option;;
value float : 'a -> float -> 'a * option;;
value option : 'a -> 'a * option;;
value color : 'a -> color -> 'a * option;;
value dashPattern : int list -> string * option;;
value join : linejoin -> string * option;;
value cap : linecap -> string * option;;
value fillStyle : fillstyle -> string * option;;
value lineLabel : string -> float -> string * option;;
value font : font_style -> string * option;;
exception OptionError
;;

value theString : ('a * option) list -> 'a -> string -> string;;
value theInt : ('a * option) list -> 'a -> int -> int;;
value theFloat : ('a * option) list -> 'a -> float -> float;;
value theBool : ('a * option) list -> 'a -> bool -> bool;;
value theOption : ('a * 'b) list -> 'a -> bool;;
value theColor : ('a * option) list -> 'a -> color -> color;;
value theDashPattern : (string * option) list -> int list -> int list;;
value theLineJoin : (string * option) list -> linejoin -> linejoin;;
value theLineCap : (string * option) list -> linecap -> linecap;;
value theFillStyle : (string * option) list -> fillstyle -> fillstyle;;
value theLineLabel : (string * option) list -> option;;
value theFont : (string * option) list -> font_style -> font_style;;
value findOption : ('a * 'b) list -> 'a -> 'b;;

