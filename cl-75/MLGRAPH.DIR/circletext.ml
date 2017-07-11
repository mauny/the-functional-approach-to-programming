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


(* $Id: circletext.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)
(* circletext.ml caml graphic system interfaces with postscript          *)
(*               Emmanuel Chailloux & Guy Cousineau                      *)
(*               Tue Jan 21  1992                                        *)





#open  "MLgraph";;

#open "compatibility";;
#open "prelude";;
#open "geometry";;
#open "paint";;
#open "texts";;
#open "pictures";;




  

let circletexttop fnt  str ce radius =
  let l= text_width fnt str
  in
    if le_float (2.0*.pi*.radius) l
       then failwith "text is longer than circumference"
       else let start_angle = (pi/.2.0) +. l/.(2.0*.radius)
            and ll= explode str in
            let lls = map (make_string 1) ll
             in
               let  al = start_angle:: (rev (tl (snd
                        (it_list (fun (a,l) w -> let aa = a-.w/.radius
                                                 in  (aa, aa::l))
                               (start_angle,[]) 
                               (map (text_width fnt) lls)))))
              and mkp  (s,a) =
                let t1 = rotation ce ((a-.pi/.2.0)*.180.0/.pi)
                and t2 = translation (ce.xc,ce.yc+.radius)
                 in transform_picture (ctrans t1 t2)
                   (make_text_picture  fnt (Gra 0.0) s)
                      
              in
                 group_pictures
                    (map mkp (combine lls al));;

let circletextbottom fnt  str ce radius =
  let l= text_width fnt str
  in
    if le_float (2.0*.pi*.radius) l
       then failwith "text is longer than circumference"
       else let start_angle = (-.pi/.2.0) -. l/.(2.0*.radius)
            and ll= explode str in
            let lls = map (make_string 1) ll
             in
               let  al = start_angle::(rev (tl (snd
                        (it_list (fun (a,l) w -> let aa = a+.w/.radius
                                                 in  (aa, aa::l))
                               (start_angle,[]) 
                               (map (text_width fnt) lls)))))
              and mkp  (s,a) =
                let t1 = rotation ce ((a+.pi/.2.0)*.180.0/.pi)
                and t2 = translation (ce.xc,ce.yc-.radius)
                 in transform_picture (ctrans t1 t2)
                   (make_text_picture  fnt (Gra 0.0) s)
                      
              in
                 group_pictures
                    (map mkp (combine lls al));;


