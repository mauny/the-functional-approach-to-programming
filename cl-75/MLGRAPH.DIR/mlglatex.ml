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


(* $Id: mlglatex.mlp,v 1.4 1997/08/27 15:54:25 emmanuel Exp $ *)
(* mlgLatex.ml     Interfate between MLgraph and Latex                   *)
(*                 Ascander Suarez                                       *)
(*                 Mon Mar 29 1993                                       *)




#open  "MLgraph";;

#open "compatibility";;
#open "prelude";;
#open "geometry";;
#open "frames";;
#open "paint";;
#open "fonts";;
#open "texts";;
#open "sketches";;
#open "pictures";;
#open "display";;
#open "option";;





let oRotation = origin_rotation;;
let arrowSketch  = 
   make_sketch[Seg[{xc=20.0;yc=0.0};{xc=20.0;yc=20.0};{xc=40.0;yc=20.0};
    {xc=40.0;yc=30.0};{xc=50.0;yc=10.0};{xc=40.0;yc= -.10.0};
    {xc=40.0;yc=0.0};{xc=20.0;yc=0.0}]];;
let arrowPict = make_fill_picture(Nzfill,Gra 0.6) arrowSketch;;
let picDir = ref "";;


let  transOfPict p =
  let lp = subpictures p in
    subpicture_transformations (hd lp)
;;


let colorOfPict p =
  let lp = subpictures p in
    subpicture_colors (hd (tl lp))
;;

let clipOfPict p =
  let lp = subpictures p in
    subpicture_clips (hd lp)
;;

let cvw = float_of_string;;
let fEq f g = let h = f-.g in le_float (-0.00001) h & le_float h 0.00001;;
let stringTexOfFloat f = if fEq f 0. then "0" else string_of_float f;; 

let blankLatexBox = make_blank_picture (0.0,0.0);;


let colorPict wd ht dp col = make_fill_picture (Eofill,col) 
  (make_sketch 
    [Seg[{xc=0.0;yc=dp};{xc=wd;yc=dp};{xc=wd;yc=ht};
         {xc=0.0;yc=ht};{xc=0.0;yc=dp}]]);;

let colorPict wd ht dp colb colf =
 group_pictures
  [make_fill_picture (Eofill,colb)
  (make_sketch
    [Seg[{xc=0.0;yc=dp};{xc=wd;yc=dp};{xc=wd;yc=ht};
         {xc=0.0;yc=ht};{xc=0.0;yc=dp}]]);
   change_color_picture colf (make_blank_picture (0.0,0.0))];;

let opaque_white = (Gra 0.0);;


let latexPictureGen opts (name,boxNumber,wd,ht,dp) = 
   let p = (colorPict (cvw wd) (cvw ht) (-.cvw dp)
            (theColor opts "background" (Gra 1.0))
            (theColor opts "foreground" opaque_white)) in
   name,(p,boxNumber);;

let latexPicture nbwhd = latexPictureGen [] nbwhd;;

let inverseTransAngle =
 let sqr x = x*.x in
 let slopeOfLine (p1,p2) = 
  let dx = p2.xc-.p1.xc and dy = p2.yc-.p1.yc in
  let long = sqrt(sqr dx +. sqr dy) in
   if le_float 0. dx then 180.*.acos (dy/.long)/.pi
    else     360. -. 180.*.acos (dy/.long)/.pi
 in fun t -> 
  let ang = slopeOfLine (transform_point t origin,transform_point t {xc=0.;yc=1.}) in
  (* print_string("% "^string_of_float ang^" => "^string_of_float  (ang*.2.)^"\n");*)
  compose_transformation (rotation origin (ang*.2.)) t;;

let printBox  oc boxNumber box  t (c,pa) =
  let print_string s = output_string oc s in 
    let base = transform_point t origin in
    let t2 = inverseTransAngle t in
    match (t2,c) with
     {m11=1.;m12=0.;m13=_;m21=0.;m22=1.;m23=_},(Gra 1.0) ->
       print_string
        ("\\put("^stringTexOfFloat base.xc^","
         ^stringTexOfFloat base.yc^"){\\"^box^string_of_int boxNumber^"}\n")
    | _,(Gra 1.0) ->
             print_string
              ("\\mlPut{("^stringTexOfFloat base.xc^","
               ^stringTexOfFloat base.yc^")}{"^
               list_it (fun f s -> stringTexOfFloat f ^" "^s)
                  [t2.m11;t2.m21;t2.m12;t2.m22] "}{\\"^box ^
               string_of_int boxNumber^"}\n")
          | _,(Gra x) ->
             print_string
              ("\\mlPutg{("^stringTexOfFloat base.xc^","
               ^stringTexOfFloat base.yc^")}{"^
               list_it (fun f s -> stringTexOfFloat f ^" "^s)
                  [t2.m11;t2.m21;t2.m12;t2.m22] "}{"^stringTexOfFloat x^"}{\\"^box ^
               string_of_int boxNumber^"}\n")
          | _,(Rgb (r,g,b)) ->
             print_string
              ("\\mlPutrgb{("^stringTexOfFloat base.xc
               ^","^stringTexOfFloat base.yc^")}{"^
               list_it (fun f s -> stringTexOfFloat f ^" "^s)
                  [t2.m11;t2.m21;t2.m12;t2.m22] "}{");
              flush std_out;
(*              special_display_path pa " clip newpath ";*)
              print_string  (" "^ (list_it (fun f s -> stringTexOfFloat f^" "
                          ^s) [r;g;b] "}{\\"^box ^
                          string_of_int boxNumber^"}\n"))
          | _,(Hsb (h,s,b)) ->
             print_string
              ("\\mlPuthsb{("^stringTexOfFloat base.xc^","
               ^stringTexOfFloat base.yc^")}{"^
               list_it (fun f s -> stringTexOfFloat f ^" "^s)
                  [t2.m11;t2.m21;t2.m12;t2.m22] "}{"^
                  (list_it (fun f s -> stringTexOfFloat f^" "^s) 
                       [h;s;b] "}{\\"^box ^
                  string_of_int boxNumber^"}\n"))
;;

let rec printBoxList f1 f2  ls = 
  match  ls with 
    ([],[],[]) -> ()
  | ([t],[c],[p]) -> f2 t (c,p)
  | ((t::tl),(c::cl),(p::pl)) -> f1 t (c,p); printBoxList f1 f2 (tl,cl,pl)
  |  (l1,l2,l3) ->  failwith ("trans "^string_of_int (list_length l1)^" col"^
              string_of_int (list_length l2)^" clip"^
              string_of_int (list_length l3))
;;


let makeLatexPicture p' exts name =  
  let oc = open_out (!picDir^(!directory_concat_string)^name^".tex") in 
  let print_string s = output_string oc s in 
  let p = begin let f = picture_frame p' in 
                  translate(-.f.xmin,-.f.ymin) p' 
          end in
    eps_file p (!picDir^(!directory_concat_string)^name);
  let f = picture_frame p in 
    print_string ("%----------- begin of "^name^".tex -----------\n");
    print_string ("\\begin{picture}("^string_of_float (f.xmax-.f.xmin)^","
      (* modif -f.xmin a la place de -f.ymin *)
     ^string_of_float (f.ymax-.f.ymin)^")(0,0)\n");
    print_string ("\\put(0,0){\\special{psfile="^ !picDir^
      (!directory_concat_string)^name^".ps}}\n");
   (*print_string ("\\put(0,0){\\makebox(0,0){$\\times$}}\n");*)
   do_list (fun (_,(e,boxNumber)) -> flush oc;
    let new_printBox = printBox oc boxNumber in    
         let l1 = transOfPict e p in
         let l2 = colorOfPict e p in 
         let l3 = clipOfPict e p in
            printBoxList (new_printBox "copy") (new_printBox "box") (l1,l2,l3))     exts;
   print_string "\\end{picture}\n";
   print_string ("%------------ end of "^name^".tex ------------\n");
   close_out oc;;

let latexBoxTable=ref ([]:(string * (picture *  int)) list );;
let add_latexBox lb = latexBoxTable:=(latexPicture lb)::!latexBoxTable;;
let empty_latexBoxTable () = latexBoxTable:=[];;
let latexBox s = 
  try fst (assoc s !latexBoxTable)
  with Not_found -> failwith ("latexBox : "^s^" latexBox unknown");;


let latexBoxGen opts s =
try 
  let p = fst(assoc s !latexBoxTable) in
  let lp = subpictures p in
  match lp with
    [fill;blank] ->
     group_pictures
       [change_color_picture (theColor opts "background" (Gra 1.0)) fill;
        change_color_picture (theColor opts "foreground" (Gra 1.0))
        (force_picture_in_frame {xmin=0.;xmax=0.;ymin=0.;ymax=0.} blank)]
   | _ -> p
with Not_found -> failwith ("latexBoxGen : "^s^" latexBox unknown")
;;

let is_latex_mode = ref false;;
let latex_mode () = !is_latex_mode;;
let set_latex_mode () = is_latex_mode:=true;;

(* Examples 

latexPicture("latex",26,"23.79582pt","6.14998pt")::
 []) in 
 let latexBox s = fst(assoc s latexBoxes) in 
 let mlGraphPicture = 
 rotate 180.0 (latexBox"latex") 
in makeLatexPicture mlGraphPicture latexBoxes "f1";;

let latexBoxes = (
latexPicture("o",26,"4.62497pt","4.0486pt")::
latexPicture("n",27,"9.34991pt","5.49998pt")::
latexPicture("s",28,"9.34991pt","5.49998pt")::
latexPicture("e",29,"9.34991pt","5.49998pt")::
latexPicture("w",30,"9.34991pt","5.49998pt")::
 []) in 
 let latexBox s = fst(assoc s latexBoxes) in 
 let mlGraphPicture = 
  circPictGen (rotate 135. (scale(4.,4.) (insPict "o" (latexBox "o");insPict 
"n" (latexBox "n");insPict "s" (latexBox "s"); insPict "e" (latexBox "e");insPict
 "w" (latexBox "w"); insLine "o" Dn "n"; insLine "o" Ds "s"; insLine "o" De "e"
; insLine "o" Dw "w"; assemblePict()))) ["Sep",FOption (-.80.)]  
in makeLatexPicture mlGraphPicture latexBoxes "p";;

let latexBoxes = (
latexPicture("e",26,"16.46863pt","5.5357pt")::
latexPicture("0",27,"25.35014pt","6.75pt")::
latexPicture("1",28,"16.41066pt","6.25pt")::
latexPicture("10",29,"31.23796pt","7.68994pt")::
latexPicture("11",30,"21.56335pt","7.68994pt")::
latexPicture("12",31,"22.15958pt","7.68994pt")::
latexPicture("120",32,"31.23796pt","7.68994pt")::
latexPicture("121",33,"22.15958pt","7.68994pt")::
 []) in 
 let latexBox s = fst(assoc s latexBoxes) in 
 let mlGraphPicture = 
 assemblePict( insPict "e" (latexBox "e"); insPict "0" (latexBox "0"); insPict "1" 
(latexBox "1"); insPict "10" (latexBox "10"); insPict "11" (latexBox "11"); 
insPict "12" (latexBox "12"); insPict "120" (latexBox "120"); insPict "121" (
latexBox "121"); insLine "e" Dsw "0"; insLine "e" Dse "1"; insLine "1" Dsw "10"; 
insLine "1" Ds "11"; insLine "1" Dse "12"; insLine "12" Dsw "120"; insLine "12" 
Dse "121") 
in makeLatexPicture mlGraphPicture latexBoxes "tree";;


*)


