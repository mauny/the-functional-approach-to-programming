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

(* $Id: bitmaps.mlp,v 1.1 1997/08/14 11:34:25 emmanuel Exp $ *)  
(* bitmaps.ml                                                            *)
(*              Guy Cousineau  & Emmanuel Chailloux                      *)  
(*              Mon Jan 20  1992                                         *)





#open "MLgraph";;

#open "compatibility";;
#open "prelude";;
#open "frames";;
#open "geometry";;






(* Functions to access components of a bitmap                            *)

let bitmap_width (b:bitmap) = b.b_width;;
let bitmap_height (b:bitmap) = b.b_height;;
let bitmap_depth (b:bitmap) = b.b_depth;;

let create_bitmap w h d =
  if (not (mem d [1;2;4;8;16]))
    then failwith
           ("Bitmap: create_bitmap: bad depth ("^
             (string_of_int d)^") for bitmap, only 1,2,4,8 or 16 are ok")
  else if not ((w*d) mod 8=0)
     then failwith ("create_bitmap: width*depth should be a multiple of 8")
    else
     let hexaw=2*((w*d+7) /  8)
     and sl= ref ([]:string list)
     in 
       for i=0 to h-1 do
        sl:= (make_string hexaw char_0) :: !sl
       done;
       let v = (vect_of_list !sl)
       in
         {b_width=w; b_height=h; b_depth=d; b_bits=v}
;;


let conv_four_bits c =
     if ascii_0 <= c & c <= ascii_9
        then c-ascii_0
        else if ascii_a <= c & c <= ascii_f
                then c-ascii_a+10
                else if ascii_A <= c & c <= ascii_F
                        then  c-ascii_A+10
                        else  failwith "conv_four_bits"
;;

let iconv_four_bits n =
      if 0<=n & n<=9
         then ascii_0 + n
         else if 10<=n & n<=15
                 then ascii_a + n - 10
                 else  failwith "iconv_four_bits"
;;


let nth_conv (n,s) = conv_four_bits (nth_ascii(n,s));;
let set_nth (n,s,c) = set_nth_ascii (n,s,iconv_four_bits c);;

let char_map_bitmap f b =
  let b' = create_bitmap b.b_width b.b_height b.b_depth
  in let p = b.b_bits
     and p'= b'.b_bits
     in
      let w = string_length p.(0)
      and h = b.b_height
      in
       for i=0 to h-1
        do
         for j=0 to w-1
          do
           set_nth (j,p'.(i),f (nth_conv(j,p.(i))))
          done
        done;
       b'   
;;


let sub_bitmap b (x1,y1) (ww,hh) =
 let w,h,d,bits = b.b_width,b.b_height,b.b_depth,b.b_bits
 in
  if not (0<hh & 0<ww) 
     then failwith ("sub_bitmap: width and height should be positive")
  else if not (0<=y1 & y1+hh<=h & 0<=x1 & x1+ww<=w)
     then failwith "sub_bitmap: some coordinate is out of the bitmap"
  else if not ((ww*d) mod 8 = 0) 
     then failwith ("sub_bitmap: width*depth should be a multiple of 8")
  else if not ((x1*d) mod 8=0)
     then failwith ("sub_bitmap: the x coordinate of the bottom left corner"
                    ^ "should be such that x*width is a a multiple of 8")
   else
      let sl= ref ([]:string list)
      in 
      for i=h-1-y1-hh+1 to h-1-y1
       do
       sl:= sub_string bits.(i) (x1*d/4) (ww*d/4) :: !sl
       done;
      {b_width=ww; b_height=hh;b_depth=d;
       b_bits=(vect_of_list (rev !sl))};;

let copy_bitmap b = sub_bitmap b (0,0) (b.b_width,b.b_height);;


let mask0001 = 1;;
let mask0010 = 2;;
let mask0100 = 4;;
let mask1000 = 8;;
let mask0011 = 3;;
let mask1100 = 10;;
let mask1110 = 14;;
let mask1101 = 13;;
let mask1011 = 11;;
let mask0111 = 7;;
let mask1111 = 15;;



let invert_bitmap = char_map_bitmap 
                      (fun x -> (lnot x) land mask1111);;


let lshift (a,b) =if b<0 then  a lsr (-b) else  a lsl b;;


let val1 s char_pos bit_pos =
    let v= nth_conv(char_pos,s)
    in match bit_pos
       with 0 -> (mask0001) land (lshift(v,-3))
        |   1 -> (mask0001) land (lshift(v,-2))
        |   2 -> (mask0001) land (lshift(v,-1))
        |   3 -> (mask0001) land (v)
        |   n -> failwith ("bitmaps__val1 : "^(string_of_int n));;

let val2 s char_pos bit_pos =
    let v= nth_conv(char_pos,s)
    in match bit_pos
       with 0 -> (mask0011) land (lshift(v,-2))
        |   1 -> (mask0011) land (v)
        |   n -> failwith ("bitmaps__val2 : "^(string_of_int n));;

let val4 s char_pos  =
   nth_conv(char_pos,s);;

let val8 s char_pos =
   (lshift(nth_conv(char_pos,s),4)) lor (nth_conv(char_pos+1,s));;

let val16 s char_pos =
      (lshift((lshift((lshift(nth_conv(char_pos,s),4))
                          lor
                          (nth_conv(char_pos+1,s)),
                       4))
                  lor 
                 (nth_conv(char_pos+2,s)),
              4))
        lor
       (nth_conv(char_pos+3,s))
;;




let change_val1 s char_pos bit_pos bit_val =
  if not (bit_val=0 or bit_val=1)
    then failwith ("Attempt to assign value "
                    ^ string_of_int bit_val
                    ^ " to a binary bitmap")
    else
    let v= nth_conv(char_pos,s)
    in set_nth
           (char_pos,s,
               match bit_pos
               with 0 -> (lshift(bit_val,3)) lor (v land mask0111)
                |   1 -> (lshift(bit_val,2)) lor (v land mask1011)
                |   2 -> (lshift(bit_val,1)) lor (v land mask1101)
                |   3 -> (bit_val) lor (v land mask1110)
                |   n -> failwith ("bitmaps__change_val1 : "^(string_of_int n)))
;;

let change_val2 s char_pos bit_pos pix_val =
  if not (pix_val>=0 & pix_val<=3)
    then failwith ("Attempt to assign value "
                    ^ string_of_int pix_val
                    ^ " to a bitmap with depth 2")
    else
    let v= nth_conv(char_pos,s)
    in set_nth
           (char_pos,s,
               match bit_pos
               with 0 -> (lshift(pix_val,2)) lor (v land mask0011)
                |   1 -> (pix_val) lor (v land mask1100)
                |   n -> failwith ("bitmaps__change_val2 : "^(string_of_int n)))
;;

let change_val4 s char_pos  pix_val =
  if not (pix_val>=0 & pix_val<=15)
    then failwith ("Attempt to assign value "
                    ^ string_of_int pix_val
                    ^ " to a bitmap with depth 4")
    else
        set_nth (char_pos,s,pix_val);;

let change_val8 s char_pos  pix_val =
  if not (pix_val>=0 & pix_val<=255)
    then failwith ("Attempt to assign value "
                    ^ string_of_int pix_val
                    ^ " to a bitmap with depth 8")
    else
       set_nth(char_pos,s,lshift(pix_val,-4));
       set_nth(char_pos+1,s,(pix_val) land (mask1111))
;;

let change_val16 s char_pos  pix_val =
       set_nth(char_pos,s,lshift(pix_val,-12));
       set_nth(char_pos+1,s,(lshift(pix_val,-8)) land (mask1111));
       set_nth(char_pos+2,s,(lshift(pix_val,-4)) land (mask1111));
       set_nth(char_pos+3,s,(pix_val) land (mask1111))
;;

(* To set a pixel in a bitmap *)



let set_pixel1 b x y v =
      let m,n = (x*b.b_depth) / 4 , (x*b.b_depth) mod 4 
      and p = b.b_height-1-y
      in  change_val1 b.b_bits.(p) m n v
;;

let set_pixel2 b x y v =
      let m,n = (x*b.b_depth) / 4 , (x*b.b_depth) mod 4 
      and p = b.b_height-1-y
      in  change_val2 b.b_bits.(p) m n v
;;

let set_pixel4 b x y v =
      let m = x
      and p = b.b_height-1-y
      in    change_val4 b.b_bits.(p) m v
;;

let set_pixel8 b x y v =
      let m = 2*x
      and p = b.b_height-1-y
      in    change_val8 b.b_bits.(p) m v
;;

let set_pixel16 b x y v =
      let m = 4*x
      and p = b.b_height-1-y
      in    change_val16 b.b_bits.(p) m v
;;

let set_pixel b x y v =
 if x<0 or x>b.b_width-1 or y<0 or y>b.b_height-1
    then failwith "set_pixel: wrong bitmap coordinates"
    else
    begin 
      (match b.b_depth
       with  1  ->  set_pixel1 b x y v
         |   2  ->  set_pixel2 b x y v
         |   4  ->  set_pixel4 b x y v
         |   8  ->  set_pixel8 b x y v
         |   16  -> set_pixel16 b x y v
         |   n -> failwith ("bitmaps__set_pixel : "^(string_of_int n)));
      ()
    end
  
;;


(* To get a pixel value in a bitmap *)


let get_pixel1 b x y =
      let m,n = (x*b.b_depth) / 4 , (x*b.b_depth) mod 4 
      and p = b.b_height-1-y
      in  val1 b.b_bits.(p) m n
;;

let get_pixel2 b x y =
      let m,n = (x*b.b_depth) / 4 , (x*b.b_depth) mod 4 
      and p = b.b_height-1-y
      in  val2 b.b_bits.(p) m n
;;

let get_pixel4 b x y =
      let m = x
      and p = b.b_height-1-y
      in    val4 b.b_bits.(p) m
;;

let get_pixel8 b x y =
      let m = 2*x
      and p = b.b_height-1-y
      in    val8 b.b_bits.(p) m
;;

let get_pixel16 b x y =
      let m = 4*x
      and p = b.b_height-1-y
      in    val16 b.b_bits.(p) m
;;

let get_pixel b x y =
 if x<0 or x>b.b_width-1 or y<0 or y>b.b_height-1
    then failwith "get_pixel: wrong bitmap coordinates"
    else
        match b.b_depth
         with  1  ->  get_pixel1 b x y
           |   2  ->  get_pixel2 b x y
           |   4  ->  get_pixel4 b x y
           |   8  ->  get_pixel8 b x y
           |   16  -> get_pixel16 b x y 
           |   n -> failwith ("bitmaps__get_pixel : "^(string_of_int n))
;;


let map_hexabyte1 f hex = 
   let bit1 = f (lshift(hex,-3))
   and bit2 = f ((lshift(hex,-2)) land (mask0001))
   and bit3 = f ((lshift(hex,-1)) land (mask0001))
   and bit4 = f ((hex) land (mask0001))
   in
       (lshift((lshift(lshift(bit1,1) lor bit2,1))
                lor bit3
               ,1))
        lor bit4;;

let map_hexabyte2 f hex = 
   let b1 = f (lshift(hex,-2))
   and b2 = f (hex land mask0001)
   in
       lshift(b1,2) lor b2;;

let map_bitmap8 f b = 
    let w = b.b_width
    and h = b.b_height
    in
      let b' = create_bitmap w h 8
      in 
       for i=0 to h-1
        do
         let j = ref 0 in
         while !j<=2 do 
           (
           let v = f ((lshift(nth_conv(!j,b.b_bits.(i)),4)) 
                       lor
                      (nth_conv(!j+1,b.b_bits.(i))))

           in set_nth(!j,b'.b_bits.(i),(lshift(v,-4)));
              set_nth(!j+1,b'.b_bits.(i),(v land mask1111));
           j:=!j+w/8-1)
          done
        done;
       b'
;;

let convert_bitmap (d,f) b =
    let w = b.b_width
    and h = b.b_height
    and (get,set) = match (b.b_depth , d)
                    with  (1,1)  ->  (get_pixel1,set_pixel1)
                      |   (1,2)  ->  (get_pixel1,set_pixel2)
                      |   (1,4)  ->  (get_pixel1,set_pixel4)
                      |   (1,8)  ->  (get_pixel1,set_pixel8)
                      |   (1,16) ->  (get_pixel1,set_pixel16)
                      |   (2,1)  ->  (get_pixel2,set_pixel1)
                      |   (2,2)  ->  (get_pixel2,set_pixel2)
                      |   (2,4)  ->  (get_pixel2,set_pixel4)
                      |   (2,8)  ->  (get_pixel2,set_pixel8)
                      |   (2,16) ->  (get_pixel2,set_pixel16)
                      |   (4,1)  ->  (get_pixel4,set_pixel1)
                      |   (4,2)  ->  (get_pixel4,set_pixel2)
                      |   (4,4)  ->  (get_pixel4,set_pixel4)
                      |   (4,8)  ->  (get_pixel4,set_pixel8)
                      |   (4,16) ->  (get_pixel4,set_pixel16)
                      |   (8,1)  ->  (get_pixel8,set_pixel1)
                      |   (8,2)  ->  (get_pixel8,set_pixel2)
                      |   (8,4)  ->  (get_pixel8,set_pixel4)
                      |   (8,8)  ->  (get_pixel8,set_pixel8)
                      |   (8,16) ->  (get_pixel8,set_pixel16)
                      |   (16,1)  ->  (get_pixel16,set_pixel1)
                      |   (16,2)  ->  (get_pixel16,set_pixel2)
                      |   (16,4)  ->  (get_pixel16,set_pixel4)
                      |   (16,8)  ->  (get_pixel16,set_pixel8)
                      |   (16,16) ->  (get_pixel16,set_pixel16)
                      |   (n,p) -> failwith ("bitmaps__convert_bitmap : "^
                                     (string_of_int n)^" "^(string_of_int p))

     in
       let b' = create_bitmap w h d
         in
           for i=0 to w-1
            do
             for j=0 to h-1
              do
               set b' i j (f (get b i j))
              done
            done;
           b'
;;

let map_bitmap f  b =
    match b.b_depth
    with  1  ->  char_map_bitmap (map_hexabyte1 f) b
      |   2  ->  char_map_bitmap (map_hexabyte2 f) b
      |   4  ->  char_map_bitmap f b
      |   8  ->  map_bitmap8 f b
      |   16 ->  convert_bitmap(b.b_depth,f) b
      |   n -> failwith ("bitmaps__map_bitmap : "^(string_of_int n))
;;




let read_bitmap depth filename=
 let channel = open_in filename
 in let line= input_line channel
    in let w= string_length line
       and ll= ref [line]
       in
         (try 
           while true do 
             let l' = input_line channel 
              in if string_length l' <> w 
                 then 
                 begin
                   close_in channel; 
                   failwith ("read_bitmap : bad bitmap file "^filename)
                 end
                 else ll:=l'::!ll 
           done
         with End_of_file -> ());
         close_in channel;
         {b_width=w*4/depth; b_height=list_length !ll; b_depth=depth;
          b_bits=(vect_of_list (rev !ll))}
;;

let write_bitmap b filename =
  let channel = open_out filename in
  let out = output_line channel
  in
    for i=0 to b.b_height-1
     do out b.b_bits.(i)
     done;
    close_out channel
;;


let bitmap_frame {b_width=w; b_height=h} =
   {xmin=0.0;xmax=float_of_int w;ymin=0.0;ymax=float_of_int h};;


let bitmap_hull  {b_width=w; b_height=h} =
  let w=float_of_int w and h=float_of_int h in
  let ptA= {xc=0.;yc=0.} and ptB= {xc=w;yc=0.} and ptC={xc=w;yc=h} and ptD={xc=0.;yc=h}
  in [ptA;ptB;ptC;ptD];;
