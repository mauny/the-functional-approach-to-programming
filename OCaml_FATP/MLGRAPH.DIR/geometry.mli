(* $Id: geometry.mlip,v 1.2 1997/08/14 14:14:51 emmanuel Exp $ *)

type point = {xc:float;yc:float};;

type geom_element =
  Seg of point list
| Arc of point * float * float * float   
        (* center,radius,start_angle,end_angle*)
| Curve of point * point * point * point;;
        (* start,control1,control2,end *)
type transformation = {m11:float;m12:float;m13:float;
                       m21:float;m22:float;m23:float};;


val make_point : float * float -> point;;
val origin : point;;
val make_transformation : float * float * float * float * float * float -> transformation;;
val id_trans : transformation;;
val transform_point : transformation -> point -> point;;
val compose_transformation : transformation -> transformation -> transformation;;
val compose_transformations : transformation list -> transformation;;
val ctrans : transformation -> transformation -> transformation;;
val inverse_transformation : transformation -> transformation;;
val handle_transform : point * point -> point * point -> transformation;;
val translation : float * float -> transformation;;
val origin_rotation : float -> transformation;;
val rotation : point -> float -> transformation;;
val scaling : float * float -> transformation;;
val symmetry : float * float -> transformation;;
val vsymmetry : float -> transformation;;
val hsymmetry : float -> transformation;;
val line_symmetry : point * point -> transformation;;
val point_symmetry : point -> transformation;;
