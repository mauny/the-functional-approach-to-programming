#open "prelude";;
#open "lexer";;

(*+term+*)
type ('a,'b) term = Term of 'a * ('a,'b) term list
                  | Var of 'b;;
(*+term+*)

value term_of_string :  string -> (string, string) term;;
value print_term : (string, string) term -> unit;;

value term_trav : ('a * 'b -> 'c) -> ('c -> 'b -> 'b) -> 'b -> 
                            ('d -> 'c) -> ('a, 'd) term -> 'c;;
value vars : ('a, 'b) term -> 'b list;;
value occurs : 'a -> ('b, 'a) term -> bool;;
value print_subst : (string * (string, string) term) list -> unit;;
value apply_subst : ('a * ('b, 'a) term) list -> ('b, 'a) term 
                            -> ('b, 'a) term;;
value compsubst : ('a * ('b, 'a) term) list -> ('a * ('b, 'a) term) list 
                      -> ('a * ('b, 'a) term) list;;
value som_subst : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list;;
value matching : ('a, 'b) term * ('a, 'c) term -> ('b * ('a, 'c) term) list;;
value unify : ('a, 'b) term * ('a, 'b) term -> ('b * ('a, 'b) term) list;;
value unify_list : (('a, 'b) term * ('a, 'b) term) list -> ('b * ('a, 'b) term) list;;

