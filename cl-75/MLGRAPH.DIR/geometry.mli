
#open "MLgraph";;

value make_point : float * float -> point;;
value origin : point;;
value make_transformation : float * float * float * float * float * float -> transformation;;
value id_trans : transformation;;
value transform_point : transformation -> point -> point;;
value compose_transformation : transformation -> transformation -> transformation;;
value compose_transformations : transformation list -> transformation;;
value ctrans : transformation -> transformation -> transformation;;
value inverse_transformation : transformation -> transformation;;
value handle_transform : point * point -> point * point -> transformation;;
value translation : float * float -> transformation;;
value origin_rotation : float -> transformation;;
value rotation : point -> float -> transformation;;
value scaling : float * float -> transformation;;
value symmetry : float * float -> transformation;;
value vsymmetry : float -> transformation;;
value hsymmetry : float -> transformation;;
value line_symmetry : point * point -> transformation;;
value point_symmetry : point -> transformation;;
