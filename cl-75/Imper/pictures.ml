#directory "../MLGRAPH.DIR";;
#open "MLgraph";;
#open "prelude";;
#open "circular_list";;



let list_of_clist ln =
  let ln1= ln.next
  in ln1.info :: loq  ln1.next
      where rec loq ln =
           if ln==ln1 then []
             else ln.info :: loq  ln.next;;

let draw_string_node r a =
  let s = center_picture
            (make_text_picture (make_font Helvetica r) black a)
            origin
  and f = make_fill_picture (Nzfill,white)
            (make_sketch [Arc(origin,r,0.0,360.0)])
  and c = make_draw_picture ({linewidth= r*.0.1;linecap=Buttcap;
                              linejoin=Miterjoin;dashpattern=[]},
                             black)
            (make_sketch [Arc(origin, r, 0.0, 360.0)])
  in group_pictures [f;c;s];;

let draw_int_node r n = draw_string_node r (string_of_int n);;



let LS = {linewidth= 1.0;linecap=Buttcap;
          linejoin=Beveljoin;dashpattern=[]};;
let FONT= make_font Helvetica 8.0;;
let make_arrowhead_picture l w  color=
  make_fill_picture
             (Nzfill,color)
             (make_sketch [Seg[{xc= -.l/.2.0;yc=0.0};
                               {xc= -.l/.2.0;yc=w/.2.0};{xc=l/.2.0;yc=0.0};
                               {xc= -.l/.2.0;yc= -.w/.2.0};
                               {xc= -.l/.2.0;yc=0.0}]])
;;
let make_arrow_picture l l' w w' color=
  make_fill_picture
             (Nzfill,color)
             (make_sketch [Seg[origin;{xc=0.;yc=w};
                               {xc=l;yc=w};{xc=l;yc=w'};
                               {xc=l+.l';yc=0.};{xc=l;yc= -.w'};
                               {xc=l;yc= -.w};{xc=0.;yc= -.w};
                               origin]])
;;

let radius= 50.0;;
let draw_circular_list ln=
  let start_arrow = translate_picture (-.radius*.2.0, 0.0)
                      (make_arrow_picture (radius/.2.0) (radius/.16.0)
                            (radius/.100.0) (radius/.32.0) black)
  and l= list_of_clist ln 
  in let n= list_length l
     in let circ= make_draw_picture (LS,black)
                    (make_sketch [Arc(origin,radius,0.0,360.0)])
        and nodes= 
             let points= 
                let angle= 360.0/.(float_of_int n)
                and first_point={xc= -.radius;yc=0.0}
                in let angles = map (fun n -> -.angle*.(float_of_int n))
                                    (interval 0 (n-1))
                   in map (fun a -> transform_point (rotation origin a)
                                                    first_point)
                           angles
             in group_pictures
                 (map2 (fun n pt -> center_picture
                                      (draw_int_node (radius/.4.0) n) pt)
                       l points)
        and arrowheads= 
                let angle= 360.0/.(float_of_int n)
                and arrow= make_arrowhead_picture
                               (radius/.8.0)  (radius/.16.0) black
                in let angles = map (fun n -> -.angle*.(float_of_int n)
                                               -.angle/.2.0)
                                    (interval 0 (n-1))
                    in map (fun a -> transform_picture 
                                         (rotation origin a)
                                         (translate_picture (-.radius,0.0)
                                            (transform_picture
                                               (rotation origin 90.0)
                                               arrow)))
                            angles
        in group_pictures ([start_arrow;circ;nodes]@arrowheads);;
let p= draw_circular_list 
         (let l = mk_circular_list 1
          in list_it insert_tail [5;4;3;2] l);;

eps_file p "../../PS/liste_circulaire";;

