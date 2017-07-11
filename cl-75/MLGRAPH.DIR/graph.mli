




















#open  "MLgraph";;
#open "compatibility";;
#open "prelude";;
#open "sketches";;
#open "pictures";;


value fold : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list;;
value try_find : ('a -> 'b) -> 'a list -> 'b;;
value nth : int -> 'a list -> 'a;;
value substituteNth : int -> (graph * pos -> graph * pos) -> (graph * pos) list -> (graph * pos) list;;
value sqr : float -> float;;
value pSub : point -> point -> point;;
value pAdd : point -> point -> point;;
value pi : float;;

value circlePoint : float -> float -> point;;
value pMult : point -> float -> point;;
value lengthOfLine : point * point -> float;;
value slopeOfLine : point * point -> float;;
value sketchGen : (string * option) list -> float -> sketch -> picture;;
value sketch : float -> sketch -> picture;;
value curvePos : point * point * point * point -> float -> point * float;;
value arrowFormGen : (string * option) list -> point * float -> string -> geom_element list list;;
value textGen : (string * option) list -> string -> picture;;
value text : string -> picture;;
value diagOfFrame : frame -> float;;
value blankSketch : (string * option) list -> sketch -> picture;;
value rectOfFrame : (string * option) list -> frame -> picture;;
value rectangleGen : (string * option) list -> picture -> picture;;
value rectangle : picture -> picture;;
value circOfFrame : (string * option) list -> frame -> picture;;
value circleGen : (string * option) list -> picture -> picture;;
value circle : picture -> picture;;
value ovalOfFrame : (string * option) list -> frame -> picture;;
value ovalGen : (string * option) list -> picture -> picture;;
value oval : picture -> picture;;
value linesOfPoints : (string * option) list -> point list -> ((string * option) list * geom_element list list) list;;
value curveOfPoints : (string * option) list -> (point * float) list -> ((string * option) list * geom_element list list) list;;
value symmetricCurvesOfPoints : (string * option) list -> point * (float * point) list -> ((string * option) list * geom_element list list) list;;
value hullOfPoints : (string * option) list -> point list -> ((string * option) list * geom_element list list) list;;
value degOfDir : dir -> float;;
value transformGraph : transformation -> graph -> graph;;
value graphPoint : string -> graph -> point;;
value graphLineLabel : float * float -> graph -> string -> point;;
value nodeGraph : string -> graph;;
value addLines : graph -> line list -> graph;;
value addPoints : (string * option) list -> graph -> (string * (string * float)) list -> graph;;
value polyGraph : string -> string list -> line list -> graph;;
value tabularGraph : string -> tabStyle -> tabPos list list -> line list -> graph;;
value linkGraphs : graph * string -> graph * string -> line list -> graph;;
value composeGraphs : graph * string * string -> graph * string * string -> line list -> graph;;
value insLineGen : (graph * pos) list -> (string * option) list * string * dir * string -> (graph * pos) list * line;;
value assembleGraphs : graph list -> string list -> ((string * option) list * string * dir * string) list -> graph;;
value pictOfLines : transformation -> float -> (string * option) list -> ((string * option) list * geom_element list list) list -> picture list;;
value skeletonOfGraphGen : (string * option) list -> transformation * float -> graph -> picture list;;
value graphGen : (string * option) list -> graph -> (string * picture) list -> picture;;
value graph : graph -> (string * picture) list -> picture;;

