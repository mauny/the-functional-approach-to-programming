(*                                                                       *)
(*                     projet      Formel                                *)
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

(* $Id: geometry.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* geometry.ml     basic geometric objects                               *)
(*                 Emmanuel Chailloux & Guy Cousineau                    *)
(*                 Wed 7 February 1996                                   *)




#open "MLgraph";;

#open "compatibility";;
#open "prelude";;




let make_point(x,y) = {xc=x ; yc =y};;
let origin = make_point(0.0,0.0);;

let make_transformation (a,b,c,d,e,f) =
           {m11=a; m12=b; m13=c; m21=d; m22=e; m23=f};;

let id_trans =  {m11=1.0;m12=0.0;m13=0.0;m21=0.0;m22=1.0;m23=0.0};;

let transform_point {m11=a;m12=b;m13=c;m21=d;m22=e;m23=f} {xc=x;yc=y} =
      {xc = a*.x+.b*.y+.c ; yc = d*.x+.e*.y+.f};;

let compose_transformation {m11=a11;m12=a12;m13=a13;m21=a21;m22=a22;m23=a23} 
                           {m11=b11;m12=b12;m13=b13;m21=b21;m22=b22;m23=b23} 
=

{m11=a11*.b11+.a12*.b21 ; m12=a11*.b12 +. a12*.b22 ; m13=a11*.b13+.a12*.b23+.a13;
 m21=a21*.b11+.a22*.b21 ; m22=a21*.b12+.a22*.b22 ; m23 = a21*.b13+.a22*.b23+.a23};;

let compose_transformations = it_list compose_transformation id_trans;;

let ctrans = compose_transformation;;


let inverse_transformation {m11=a;m12=b;m13=c;m21=d;m22=e;m23=f} =
  let det = a*.e -. d*.b 
  in
     if det =0.0 then failwith "inverse_transformation: non inversible matrix"
               else  {m11=e/.det ; m12 = -.b/.det ; m13=(f*.b-.c*.e)/.det ;
                      m21 = -.d/.det ; m22 = a/.det ; m23=(d*.c-.a*.f)/.det};;


let handle_transform ({xc=x1;yc=y1},{xc=x2;yc=y2}) 
                     ({xc=x3;yc=y3},{xc=x4;yc=y4})
=

 let a'=x2-.x1
 and b'=y2-.y1
 and a''=x4-.x3
 and b''=y4-.y3
in
 let det = a''*.a'' +. b''*.b'' 
in

 if det = 0.0 
   then failwith "Wrong handle"
   else
     let c = (a'*.a''+.b'*.b'')/.det
     and d = (b'*.a''-.a'*.b'')/.det 
      in
        {m11=c;m12= -.d;m13= -.c*.x3+.d*.y3+.x1;
         m21=d;m22=c;m23= -.d*.x3-.c*.y3+.y1};;



let translation (a,b) = {m11=1.0 ; m12=0.0 ; m13=a ; m21=0.0 ; m22=1.0 ; m23 = b};;

let origin_rotation alpha = 
  {m11=cosinus alpha ; m12= -. (sinus alpha); m13=0.0 ; 
   m21=sinus alpha ; m22=cosinus alpha ; m23 = 0.0};;


let rotation {xc=x;yc=y} alpha =
  compose_transformations
       [translation(x,y); origin_rotation alpha; translation(-.x,-.y)];;

let scaling (a,b) = {m11=a ; m12=0.0 ; m13=0.0 ; m21=0.0 ; m22=b ; m23 = 0.0};;

let symmetry (a,b) =        (* symetrie par rapport a la droite y-.ax+.b  *)
      if a=0.0 then failwith "a=0: use hsymmetry"
             else let d = a*.a+. 1.0 in
   {m11= -.((a*.a-.1.0)/.d); m12=2.0*.a/.d; m13= -.(2.0*.a*.b/.d);
    m21=2.0*.a/.d; m22=(a*.a-.1.0)/.d;  m23=2.0*.b/.d};;  

let vsymmetry a =      (* symetrie par rapport a la droite x=a   *)
   {m11= -.1.0; m12=0.0; m13=2.0*.a; m21=0.0; m22=1.0; m23=0.0};;

let hsymmetry a =      (* symetrie par rapport a la droite y=a   *)  
   {m11=1.0; m12=0.0; m13=0.0 ; m21=0.0; m22= -.1.0; m23=2.0*.a};;
  

let line_symmetry (({xc=x1;yc=y1} as pt1),({xc=x2;yc=y2} as pt2)) =
  if pt1=pt2 
    then failwith "Cannot define a line symmetry with two equal points"
    else if y1=y2
           then let a = y1 in hsymmetry a
	   else if x1=x2
	          then let a=x1 in vsymmetry a
		  else let a = (y2-.y1)/.(x2-.x1)
		       in let b= y1-.a*.x1
		          in symmetry(a,b);;
			  
let point_symmetry {xc=x;yc=y} =
        {m11= -.1.0; m12=0.0;  m13=2.0*.x;
         m21=0.0;  m22= -.1.0; m23=2.0*.y};;  
