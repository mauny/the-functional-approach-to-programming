#directory "../MLGRAPH.DIR";;
#open "MLgraph";;

(* +type_complex+ *)
type complex= {re_part:float; im_part:float};;
(* +type_complex+ *)
(* +complex+ *)
let mk_cx r i= {re_part=r; im_part=i};;
let cx_1 = mk_cx 1.0 0.0
and cx_0 = mk_cx 0.0 0.0;;
let cx_of_pol rho theta= {re_part=rho*.cosinus theta; 
                          im_part=rho*.sinus theta};;

(* +complex+ *)



(* +operations_complexes+ *)
let conjugate {re_part=r; im_part=i} = {re_part=r; im_part= -.i};;
let module {re_part=r; im_part=i}= sqrt(r*.r +. i*.i);;
let add_cx {re_part=r1; im_part=i1} {re_part=r2; im_part=i2}=
  {re_part=r1+.r2; im_part=i1+.i2};;
let sub_cx {re_part=r1; im_part=i1} {re_part=r2; im_part=i2}=
  {re_part=r1-.r2; im_part=i1-.i2};;
let uminus_cx {re_part=r; im_part=i}= {re_part= -.r; im_part= -.i};;
let mult_cx {re_part=r1; im_part=i1} {re_part=r2; im_part=i2}=
  {re_part=r1*.r2-.i1*.i2; im_part=i1*.r2+.i2*.r1};;
let div_cx {re_part=r1; im_part=i1} {re_part=r2; im_part=i2}=
   let rho= r2*.r2 +. i2*.i2 in
     if rho=0.0 then failwith "cdiv: division by zero"
       else {re_part=(r1*.r2+.i1*.i2)/.rho;
             im_part=(i1*.r2-.r1*.i2)/.rho};;

(* +operations_complexes+ *)



(* +type_isometry+ *)
type hyp_isometry= {iso_m:complex; iso_a:complex};;
(* +type_isometry+ *)
(* +iso_id+ *)
let hyp_identity = {iso_m=cx_1;iso_a=cx_0};;
(* +iso_id+ *)



(* +apply_hyp_iso+ *)
let apply_hyp_iso {iso_m=mu; iso_a=a} z=
    mult_cx mu (div_cx (add_cx z a) 
                       (add_cx cx_1 (mult_cx (conjugate a) z)));;

(* +apply_hyp_iso+ *)


(* +compose_hyp_iso+ *)
let compose_hyp_iso {iso_m=mu1; iso_a=a1} {iso_m=mu2; iso_a=a2}=
  {iso_m= div_cx  (mult_cx mu2 (add_cx mu1 (mult_cx a2 (conjugate a1))))
                   (add_cx cx_1 (mult_cx mu1 (mult_cx (conjugate a2) a1))); 
   iso_a= div_cx (add_cx a2 (mult_cx mu1 a1)) 
                 (add_cx mu1 (mult_cx a2 (conjugate a1)))};;

(* +compose_hyp_iso+ *)

(*
let compose_hyp_iso {iso_m=mu1; iso_a=a1} {iso_m=mu2; iso_a=a2}=
  {iso_m= div_cx  (mult_cx mu1 (add_cx mu2 (mult_cx a1 (conjugate a2))))
                   (add_cx cx_1 (mult_cx mu2 (mult_cx (conjugate a1) a2))); 
   iso_a= div_cx (add_cx a1 (mult_cx mu2 a2)) 
                 (add_cx mu2 (mult_cx a1 (conjugate a2)))};;
*)



(* +hyp_rotation+ *)
let hyp_rotation center angle=
  compose_hyp_iso 
     {iso_m=cx_1;iso_a=uminus_cx center}
     (compose_hyp_iso 
           {iso_m=mk_cx (cosinus angle) (sinus angle); 
            iso_a=cx_0}
           {iso_m=cx_1; iso_a=center});;

(* +hyp_rotation+ *)

(*
let hyp_rotation center angle=
  compose_hyp_iso 
     (compose_hyp_iso {iso_m=cx_1; iso_a=center}
           {iso_m=mk_cx (cosinus angle) (sinus angle); 
            iso_a=cx_0})
     {iso_m=cx_1;iso_a=uminus_cx center};;
*)





(* ++ *)
(* ++ *)
(* ++ *)
(* ++ *)
