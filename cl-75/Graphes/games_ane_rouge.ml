#directory "../Util";;
#directory "../Arbres";;

#open "prelude";;
#open "orders";;

#open "binary_trees";;

#open "sets";;

#open "games";;

#infix "o";;

(*   JEUX    DE L'ANE ROUGE  *)

type cell== int;;

(* +up_left_down_right+ *)

let up n = n-10 and left n = n-1
and down n = n+10 and right n = n+1;;

(* +up_left_down_right+ *)

let up2 n = n-20;;

let left2 n = n-2;;

let down2 n = n+20;;

let right2 n = n+2;;

(* +piece+ *)

type piece = Donkey | Square | Horiz | Vertic | None;;

(* +piece+ *)
(* +board+ *)

type board =
     {donkey: int;
      squares: int list;
      horiz: int;
      vertics: int list};;

(* +board+ *)
(* +direction_move+ *)

type direction = Left | Up | Right | Down;;

type move == int * direction;;

(* +direction_move+ *)
(* +get+ *)

let get cell board =
  if board.donkey=cell then Donkey else
  if board.horiz = cell then Horiz else
  if mem cell board.squares then Square else
  if mem cell board.vertics then Vertic
  else None;;

(* +get+ *)
(* +moves1+ *)

let moves1 b board =
  (match get (right b) board with
     (Square|Horiz) -> [b,Left] | _ -> []) @
  (match get (down b) board with
     (Square|Vertic) -> [(b,Up)] | _ -> []) @
  (match (get (left b) board, get (left(left b)) board) with
     ((Square,_)|(_,Horiz)) -> [(b,Right)] | _ -> []) @
  (match (get (up b) board, get (up(up b)) board) with
     ((Square,_)|(_,Vertic)) -> [(b,Down)] | _ -> []);;

(* +moves1+ *)
(* +adjacent+ *)

let h_adjacent (n1,n2) = n2=n1+1;;

let v_adjacent (n1,n2) = n2=n1+10;;

(* +adjacent+ *)
(* +moves2+ *)

let moves2 (b,b') board =
  if not(h_adjacent (b,b')) & not(v_adjacent (b,b')) then [] else
  if h_adjacent (b,b') then
    (match (get (up b) board, get (up(up b)) board) with
       ((_,Donkey)|(Horiz,_)) -> [(b,Down)] | _ -> []) @
    (match get (down b) board with
       (Donkey|Horiz) -> [(b,Up)] | _ -> [])
  else
    (match (get (left b) board, get (left(left b)) board) with
       ((_,Donkey)|(Vertic,_)) -> [(b,Right)] | _ -> []) @
    (match get (right b) board with
       (Donkey |Vertic) -> [b,Left] | _ -> []);;

(* +moves2+ *)

(* +ane_comp+ *)
let cell_comp = int_comp;;

let rec cell_list_comp cl1 cl2= 
match (cl1,cl2) with
  ([],[]) -> Equiv
| (c1::cl1', c2::cl2') ->
    match cell_comp c1 c2 with
      Smaller -> Smaller
    | Greater -> Greater
    | Equiv -> cell_list_comp cl1' cl2'
    | _ -> failwith "cell_list_comp";;

let ane_comp c1 c2 =
  match cell_comp c1.donkey c2.donkey with
    Smaller -> Smaller
  | Greater -> Greater
  | Equiv ->
      match cell_comp c1.horiz c2.horiz with
        Smaller -> Smaller
      | Greater -> Greater
      | Equiv ->
          match cell_list_comp c1.squares c2.squares with
            Smaller -> Smaller
          | Greater -> Greater
          | Equiv ->
              match cell_list_comp c1.vertics c2.vertics with
                Smaller -> Smaller
              | Greater -> Greater
              | Equiv -> Equiv;;
(* +ane_comp+ *)



(* +app_move+ *)

let rec update x y = fun
  [] -> []
| (a::l) -> if x=a then y::l
            else a::update x y l;;


let rec insert comp x = fun
  [] -> [x]
| ((a::l) as l') ->
    match comp x a with
      (Smaller|Equiv) -> x::l'
    | Greater -> a :: insert comp x l;;

let sort comp l = list_it (insert comp) l [];;

let order_pair ((n1,n2) as p) =
  match cell_comp n1 n2 with
    (Smaller | Equiv) -> p
  | Greater -> (n2,n1);;

let update_pair x y (l,r)=
  order_pair (if x=l then (y,r) else (l,y));;

let update_donkey c x =
  {donkey=x; squares=c.squares;
   horiz=c.horiz; vertics=c.vertics};;

let update_square c x =
  {donkey=c.donkey; squares=x;
   horiz=c.horiz; vertics=c.vertics};;

let update_horiz c x =
  {donkey=c.donkey; squares=c.squares;
   horiz=x; vertics=c.vertics};;

let update_vertic c x =
  {donkey=c.donkey; squares=c.squares;
   horiz=c.horiz; vertics=x};;

let app_move ((b1,b2) as p,c) (b,d) = match d with
  Left -> (match (get (right b) c) with
             Square -> (update_pair b (right b) p,
                        update_square c
                          (sort cell_comp
                             (update (right b) b c.squares)))
           | Horiz -> (update_pair b (right (right b)) p,
                       update_horiz c b)
           | Donkey -> ((right (right b1), right (right b2)),
                        update_donkey c b)
           | Vertic -> ((right b1, right b2),
                        update_vertic c
                          (sort cell_comp
                             (update (right b) b c.vertics)))
           | None -> failwith "wrong move")
| Right -> (match (get (left b) c, get (left (left b)) c) with
              (Square, _) -> (update_pair b (left b) p,
                              update_square c
                                (sort cell_comp
                                   (update (left b) b c.squares)))
            | (_, Horiz) -> (update_pair b (left (left b)) p,
                             update_horiz c (left b))
            | (_, Donkey) -> ((left (left b1), left (left b2)),
                              update_donkey c (left b))
            | (Vertic, _) -> ((left b1, left b2),
                              update_vertic c
                                (sort cell_comp
                                   (update (left b) b c.vertics)))
            | _ -> failwith "wrong move")
| Up -> (match (get (down b) c) with
           Square -> (update_pair b (down b) p,
                      update_square c
                        (sort cell_comp
                           (update (down b) b c.squares)))
         | Horiz -> ((down b1, down b2),
                     update_horiz c b)
         | Donkey -> ((down (down b1), down (down b2)),
                      update_donkey c b)
         | Vertic -> (update_pair b (down (down b)) p,
                      update_vertic c
                        (sort cell_comp
                           (update (down b) b c.vertics)))
         | None -> failwith "wrong move")
| Down -> (match (get (up b) c, get (up (up b)) c) with
             (Square, _) -> (update_pair b (up b) p,
                             update_square c
                               (sort cell_comp
                                  (update (up b) b c.squares)))
           | (Horiz, _) -> ((up b1, up b2),
                            update_horiz c b)
           | (_, Donkey) -> ((up (up b1), up (up b2)),
                             update_donkey c (up b))
           | (_, Vertic) -> (update_pair b (up (up b)) p,
                             update_vertic c
                               (sort cell_comp
                                  (update (up (up b)) (up b)
                                     c.vertics)))
           | _ -> failwith "wrong move");;

(* +app_move+ *)
(* +app_moves+ *)

let app_moves c = it_list app_move c;;

(* +app_moves+ *)

let thai_moves (((b1,b2),c) as c') =
  let moves = moves1 b1 c @ moves1 b2 c @ moves2 (b1,b2) c in
  map (fun m -> (app_move c' m)) moves;;

(* +next_configs+ *)

let next_configs (ml,(((b1,b2),c) as c')) =
  let moves = moves1 b1 c @ moves1 b2 c @ moves2 (b1,b2) c in
  map (fun m -> (m::ml, app_move c' m)) moves;;

(* +next_configs+ *)
(* n'utilise pas le compteur *)
(* +solve_ane_rouge+ *)

let solve_ane_rouge start ok =
  solve_breadth_first
    (ok, next_configs, ane_comp)
    (snd o snd)
    [([], start)];;

(* +solve_ane_rouge+ *)

let solve_ane_rouge_trace start ok=
  solve_breadth_first_trace
    (ok, next_configs, ane_comp)
    (snd o snd)
    [([], start)];;

(* +ok_config+ *)

let ok_config c = (snd(snd c)).donkey / 10 = 4;;

(* +ok_config+ *)

let output_int ch n =
  output_string ch (string_of_int n);;

let output_cell = output_int;;

let strofd = fun
  Left -> " Left"
| Right -> " Right"
| Up -> " Up"
| Down -> " Down";;

let output_move ch m =
  output_cell ch (fst m); output_string ch (strofd (snd m));
  output_string ch " | ";;

let output_solution ch sol=
  output_string ch "Solution en ";
  output_int ch (list_length (fst sol));
  output_string ch " mouvements\n\n";
  do_list (output_move ch) (rev (fst sol));
  output_string ch "\n\n\n";;

let start =
  ((31,34), {donkey=12;
           squares=[11;14;21;24];
           horiz=32;
           vertics= [41;42;43;44]});;

let start2 =
  ((31,34), {donkey=12;
            squares=[42;43;52;53];
            horiz=32;
            vertics= [11;14;41;44]});;

(* 
     output_solution std_out
       (solve_ane_rouge_trace start ok_config ) ;;
 *)

let find_all_solutions_for_thai_game ok start =
  find_all_solutions true
    (ok, next_configs, ane_comp)
    (snd o snd)
    [([], start)];;

let output_solutions ch sols =
  output_string ch "Nombre de Solutions: ";
  output_int ch (list_length sols);
  output_string ch "\n\n";
  output_string ch "Listes des Solutions: \n\n";
  do_list (output_solution ch) sols;;

let output_results ch (sols,(arch,l)) =
  output_solutions ch sols;
  output_string ch "Nombre de configurations explore'es: ";
  output_int ch (cardinal arch);
  output_string ch "\n";;

(* 
     output_results std_out
       (find_all_solutions_for_thai_game ok_config start) ;;
 *)

let explore_breadth_first_ane_rouge start =
  explore_breadth_first_trace (thai_moves,ane_comp) snd [start];;

(* 
     explore_breadth_first_ane_rouge start ;;
     Nombre de configurations atteintes: 25955
     Longueur du plus long chemin: 132
 *)
(* 

     let N=ref 0;;
     let V1= [|0;0;0;0;0;0;0|];;
     let V2= [|0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|];;

     let set_donkey i = (V1.(1)<-i;V2.(i)<-1; V2.(i+1)<-1;
                       V2.(i+4)<-1; V2.(i+5)<-1);;
     let unset_donkey i = (V1.(1)<-0;V2.(i)<-0; V2.(i+1)<-0;
                       V2.(i+4)<-0; V2.(i+5)<-0);;

     let place_donkey()=
     if V1.(1)=0 then (set_donkey 1;true)
                 else let i= V1.(1)
                      in unset_donkey i;
                         if i<15 then if i mod 4 <=2
                                        then (set_donkey(i+1);true)
                                        else (set_donkey(i+2);true)
                                 else false;;

     let set_horiz i = (V1.(2)<-i;V2.(i)<-1; V2.(i+1)<-1);;
     let unset_horiz i = (V1.(2)<-0;V2.(i)<-0; V2.(i+1)<-0);;
     let ok_for_horiz i = (i mod 4 <= 3) & V2.(i)=0 & V2.(i+1)=0;;

     let place_horiz()=
      let p = V1.(2)
      in let i= if p=0 then 1
                       else (unset_horiz p;p+1)
         in ph i
            where rec ph n= if n>19 then false
                              else if ok_for_horiz n
                                     then (set_horiz n;true)
                                     else ph (n+1);;

     let set_vertic k i = (V1.(k+2)<-i;V2.(i)<-1; V2.(i+4)<-1);;
     let unset_vertic k i = (V1.(k+2)<-0;V2.(i)<-0; V2.(i+4)<-0);;
     let ok_for_vertic i = (i  <= 16) & V2.(i)=0 & V2.(i+4)=0;;

     let place_vertic k =
      let p = V1.(k+2)
      in let i= if p=0 then if k=1 then 1
                                   else V1.(k+1)+1
                       else (unset_vertic k p;p+1)
         in ph i
            where rec ph n= if n>16 then false
                              else if ok_for_vertic n
                                     then (set_vertic k n;true)
                                     else ph (n+1);;


     while place_donkey()
     do while place_horiz()
        do while place_vertic 1
              do while place_vertic 2
                 do while place_vertic 3
                    do while place_vertic 4
                       do (N:=!N+1;
                           if !N land 1023=0
                               then (print_string (string_of_int !N);
                                     print_newline()))
                       done
                    done
                 done
              done
        done
     done;;

 *)

let N=ref 0;;

let V1= [|0;0;0;0;0;0;0;0;0;0;0|];;

let V2= [|0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|];;

let conv n = 11+ 10*((n-1)/4)+(n-1) mod 4;;

let inv_conv n =
  let n1= n/10-1 and n2=n mod 10 in 4*n1+n2;;

let set_donkey V1 V2 i =
  (V1.(1)<-i;V2.(i)<-1; V2.(i+1)<-1;
   V2.(i+4)<-1; V2.(i+5)<-1);;

let unset_donkey V1 V2 i =
  (V1.(1)<-0;V2.(i)<-0; V2.(i+1)<-0;
   V2.(i+4)<-0; V2.(i+5)<-0);;

let place_donkey V1 V2 ()=
  if V1.(1)=0 then (set_donkey V1 V2 1;true)
  else let i= V1.(1) in
       unset_donkey V1 V2 i;
       if i<15 then if conv i mod 10 <= 2
                    then (set_donkey V1 V2 (i+1);true)
                    else (set_donkey V1 V2 (i+2);true)
       else false;;

let set_horiz V1 V2 i = (V1.(2)<-i;V2.(i)<-1; V2.(i+1)<-1);;

let unset_horiz V1 V2 i = (V1.(2)<-0;V2.(i)<-0; V2.(i+1)<-0);;

let ok_for_horiz V1 V2 i = (conv i mod 10 <= 3) & V2.(i)=0 & V2.(i+1)=0;;

let place_horiz V1 V2 ()=
  let p = V1.(2) in
  let i = if p=0 then 1 else (unset_horiz V1 V2 p;p+1) in
  ph i
  where rec ph n =
    if n>19 then false else
    if ok_for_horiz V1 V2 n then (set_horiz V1 V2 n;true)
    else ph (n+1);;

let set_vertic V1 V2 k i = (V1.(k+2)<-i;V2.(i)<-1; V2.(i+4)<-1);;

let unset_vertic V1 V2 k i = (V1.(k+2)<-0;V2.(i)<-0; V2.(i+4)<-0);;

let ok_for_vertic V1 V2 i = (i <= 16) & V2.(i)=0 & V2.(i+4)=0;;

let place_vertic V1 V2 k =
  let p = V1.(k+2) in
  let i = if p=0 then if k=1 then 1 else V1.(k+1)+1
          else (unset_vertic V1 V2 k p;p+1) in
  ph i
  where rec ph n =
    if n>16 then false else
    if ok_for_vertic V1 V2 n then (set_vertic V1 V2 k n;true)
    else ph (n+1);;

let set_square V1 V2 k i = (V1.(k+6)<-i;V2.(i)<-1);;

let unset_square V1 V2 k i = (V1.(k+6)<-0;V2.(i)<-0);;

let ok_for_square V1 V2 i = (i <= 20) & V2.(i)=0;;

let place_square V1 V2 k =
  let p = V1.(k+6) in
  let i = if p=0 then if k=1 then 1 else V1.(k+5)+1
          else (unset_square V1 V2 k p;p+1) in
  ph i
  where rec ph n =
    if n>20 then false else
    if ok_for_square V1 V2 n then (set_square V1 V2 k n;true)
    else ph (n+1);;

let S = ref (make_set ane_comp []);;

let make_config c = match c with
  {donkey=k;
    squares=[k1; k2; k3; k4];
    horiz=h;
    vertics=[v1; v2; v3; v4]} ->
    let vect1 = [|0;0;0;0;0;0;0;0;0;0;0|]
    and vect2 = [|0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|]
    and i =ref 1 in
    set_donkey vect1 vect2 (inv_conv k);
    set_horiz vect1 vect2 (inv_conv h);
    set_vertic vect1 vect2 1 (inv_conv v1);
    set_vertic vect1 vect2 2 (inv_conv v2);
    set_vertic vect1 vect2 3 (inv_conv v3);
    set_vertic vect1 vect2 4 (inv_conv v4);
    set_square vect1 vect2 1 (inv_conv k1);
    set_square vect1 vect2 2 (inv_conv k2);
    set_square vect1 vect2 3 (inv_conv k3);
    set_square vect1 vect2 4 (inv_conv k4);
    (let n1 =(while vect2.(!i) <> 0 do i:= !i+1 done;!i)
     and n2 =( i:=!i+1;while vect2.(!i) <> 0 do i:= !i+1 done;!i)
     in (conv n1, conv n2), c)
| _ -> failwith "make_config"
;;


let conv_config vect1 vect2 () =
  {donkey= conv(vect1.(1));
   squares=[conv(vect1.(7));conv(vect1.(8));conv(vect1.(9));conv(vect1.(10))];
   horiz=conv(vect1.(2));
   vertics= [conv(vect1.(3));conv(vect1.(4));conv(vect1.(5));conv(vect1.(6))]};;

(* 
     while place_donkey V1 V2 ()
     do while place_horiz V1 V2 ()
        do while place_vertic V1 V2  1
              do while place_vertic V1 V2  2
                 do while place_vertic V1 V2  3
                    do while place_vertic V1 V2  4
                       do while place_square V1 V2  1
                         do while place_square V1 V2  2
                            do while place_square V1 V2  3
                               do while place_square V1 V2  4
                                  do S:= add_to_set !S (conv_config V1 V2 ()) ;
                                     N:=!N+1;
                           if !N land 1023=0
                               then (print_string (string_of_int !N);
                                     print_newline())

                                  done
                               done
                            done
                         done
                       done
                    done
                 done
              done
        done
     done;;

     let THAI_GRAPHE= !S;;
     cardinal THAI_GRAPHE;;
     - : int = 65880
     let TG= open_out "THAI_GRAPHE"
     in output_value TG THAI_GRAPHE.set_elements;;

     let los = list_of_set;;
     let ll= los THAI_GRAPHE;;



     let rec findcc  cset  =
      if set_isempty cset then []
       else let c= make_config(set_random_element cset)
            in let CC= explore_breadth_first (thai_moves,ane_comp) snd [c]
               in CC::findcc  (set_diff cset CC);;



     let lcc= findcc (thai_moves,ane_comp) snd THAI_GRAPHE;;

     let start =(31,34),
           {donkey=12;
            squares=[11;14;21;24];
            horiz=32;
            vertics= [41;42;43;44]};;


     let start2 =(31,34),
           {donkey=12;
            squares=[42;43;52;53];
            horiz=32;
            vertics= [11;14;41;44]};;



     let CC1= fst ( snd (find_number_of_configs_for_thai_game ok_config start));;
     cardinal CC1;;
     - : int = 25955

     let CC2= fst ( snd (find_number_of_configs_for_thai_game ok_config start2));;
     cardinal CC2;;
     - : int = 25955

     let start3 =(31,34),
           {donkey=42;
            squares=[41;44;51;54];
            horiz=32;
            vertics= [11;12;13;14]};;


     let start4 =(31,34),
           {donkey=42;
            squares=[12;13;22;23];
            horiz=32;
            vertics= [11;14;41;44]};;



 *)

let small_cc =
  ((11, 42), {donkey=21; squares=[12; 34; 44; 54]; horiz=52;
              vertics=[13; 14; 33; 41]});;

(* 
     Il y a 898 composantes connexes
     Seules CC1 et CC2 sont de grosse taille:  25955
     Les suivantes se partagent 13 970 elements

     nombre de composantes connexes de taille  entre 100 et 1000 : 16
     ex: 248, ((11, 12), {donkey=21; squares=[14; 51; 52; 53]; horiz=41;
                         vertics=[13; 24; 33; 44]})
     nombre de composantes connexes de taille < 100  : 880
     ex: 92, ((11, 21), {donkey=32; squares=[31; 34; 44; 54]; horiz=52;
                        vertics=[12; 13; 14; 41]})
     nombre de composantes connexes de taille < 10  :  544
     ex: 6, ((11, 13), {donkey=32; squares=[23; 34; 41; 53]; horiz=51;
                       vertics=[12; 14; 21; 44]})
     nombre de composantes connexes de taille  2  : 36
     ex: 2,  ((11, 42), {donkey=21; squares=[12; 34; 44; 54]; horiz=52;
                        vertics=[13; 14; 33; 41]})


     let rec rem = function
     (s,[]) -> (s,[])
     | (s,(x::l)) -> try rem (remove_from_set s x,l)
                   with _ -> (s,[x]);;

     let check_config {donkey=k;
                      squares=[k1; k2; k3; k4];
                      horiz=h;
                      vertics=[v1; v2; v3; v4]} =
     k1<k2 & k2<k3 & k3<k4 & v1<v2 & v2<v3 & v3<v4
     & (;;



     find (fun c -> not (check_config c)) ll1;;


     let start3= (53, 54), {donkey=42; squares=[12; 31; 41; 44]; horiz=33; vertics=[11; 13; 14; 22]};;

     let CC3= fst ( snd (find_number_of_configs_for_thai_game ok_config start3));;



     all (fun c -> let [k1;k2;k3;k4]=c.squares in k1>=k2 or k2>=k3 or k3>=k4) ll;;


 *)
(* +solve_ane_rouge_depth_first+ *)

let solve_ane_rouge_depth_first start ok=
  solve_depth_first
    (ok,next_configs,ane_comp)
    (snd o snd)
    ([],start);;

(* +solve_ane_rouge_depth_first+ *)
(* 
     let ml,c= solve_ane_rouge_depth_first start ok_config
     in list_length ml,c;;
 *)


