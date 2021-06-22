open Compatibility
open Prelude
open Geometry
open Frames
open Paint
open Fonts
open Texts
open Sketches
open Pictures
open Graphics_defaults
open Options

type graph =
    Graph of string
  | PGraph of graph*(string*point)list
  | LGraph of graph*((string * option) list * geom_element list list) list
  | TGraph of transformation*graph
  | CGraph of graph*graph;;
type dir = Dn | Ds | De | Dw | Dne | Dnw | Dse | Dsw | Deg of float;;
type line =
    GLine  of (string*option) list*string list
  | GCurve of (string*option) list*(string*dir) list
  | GSCurve of (string*option) list*string*(float*string)list
  | GHull of (string*option) list*string list;;

type tabPos = V of float | L of string;;
type tabStyle = Center|Left|Right;;

type pos = AnyPos | RelPos of int;;


val fold : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list;;
val try_find : ('a -> 'b) -> 'a list -> 'b;;
val nth : int -> 'a list -> 'a;;
val substituteNth : int -> (graph * pos -> graph * pos) -> (graph * pos) list -> (graph * pos) list;;
val sqr : float -> float;;
val pSub : point -> point -> point;;
val pAdd : point -> point -> point;;
val pi : float;;

val circlePoint : float -> float -> point;;
val pMult : point -> float -> point;;
val lengthOfLine : point * point -> float;;
val slopeOfLine : point * point -> float;;
val sketchGen : (string * option) list -> float -> sketch -> picture;;
val sketch : float -> sketch -> picture;;
val curvePos : point * point * point * point -> float -> point * float;;
val arrowFormGen : (string * option) list -> point * float -> string -> geom_element list list;;
val textGen : (string * option) list -> string -> picture;;
val text : string -> picture;;
val diagOfFrame : frame -> float;;
val blankSketch : (string * option) list -> sketch -> picture;;
val rectOfFrame : (string * option) list -> frame -> picture;;
val rectangleGen : (string * option) list -> picture -> picture;;
val rectangle : picture -> picture;;
val circOfFrame : (string * option) list -> frame -> picture;;
val circleGen : (string * option) list -> picture -> picture;;
val circle : picture -> picture;;
val ovalOfFrame : (string * option) list -> frame -> picture;;
val ovalGen : (string * option) list -> picture -> picture;;
val oval : picture -> picture;;
val linesOfPoints : (string * option) list -> point list -> ((string * option) list * geom_element list list) list;;
val curveOfPoints : (string * option) list -> (point * float) list -> ((string * option) list * geom_element list list) list;;
val symmetricCurvesOfPoints : (string * option) list -> point * (float * point) list -> ((string * option) list * geom_element list list) list;;
val hullOfPoints : (string * option) list -> point list -> ((string * option) list * geom_element list list) list;;
val degOfDir : dir -> float;;
val transformGraph : transformation -> graph -> graph;;
val graphPoint : string -> graph -> point;;
val graphLineLabel : float * float -> graph -> string -> point;;
val nodeGraph : string -> graph;;
val addLines : graph -> line list -> graph;;
val addPoints : (string * option) list -> graph -> (string * (string * float)) list -> graph;;
val polyGraph : string -> string list -> line list -> graph;;
val tabularGraph : string -> tabStyle -> tabPos list list -> line list -> graph;;
val linkGraphs : graph * string -> graph * string -> line list -> graph;;
val composeGraphs : graph * string * string -> graph * string * string -> line list -> graph;;
val insLineGen : (graph * pos) list -> (string * option) list * string * dir * string -> (graph * pos) list * line;;
val assembleGraphs : graph list -> string list -> ((string * option) list * string * dir * string) list -> graph;;
val pictOfLines : transformation -> float -> (string * option) list -> ((string * option) list * geom_element list list) list -> picture list;;
val skeletonOfGraphGen : (string * option) list -> transformation * float -> graph -> picture list;;
val graphGen : (string * option) list -> graph -> (string * picture) list -> picture;;
val graph : graph -> (string * picture) list -> picture;;
