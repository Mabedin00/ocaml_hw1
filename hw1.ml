
let rec pow x = function 
    | 0 -> 1
    | n -> x * pow x (n-1);;
         
pow 2 2;;
pow 1 0 ;;
pow 2 20;;

 let rec float_pow x = function
   | 0 -> 1.0
   | n -> float_pow x (n - 1) *. x;;

float_pow 1.5 2;;
float_pow 2.0 2;;
float_pow 3.6 26;;

 let rec compress_h list curr_h = 
   match list with
   | [] -> []
   | h::t -> if h = curr_h 
       then compress_h t h 
       else h :: compress_h t h;;
  
 let compress list = compress_h list "";;

compress ["a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e"];;
compress ["e";"e";"e"];;
compress [];;

let rec remove_if list checker = 
  match list with
  | [] -> []
  | h::t -> if checker h
      then remove_if t checker
      else h::remove_if  t checker;; 

remove_if [1;2;3;4;5] (fun x -> x mod 2 = 1);;
remove_if [] (fun x -> x mod 2 = 1);;

  let rec slice_h list s f i = 
    match list with
    | [] -> []
    | h::t -> if i >= s && i < f
        then h::slice_h t s f (i+1)
        else slice_h t s f (i+1) ;;

  let slice list s f = slice_h list s f 0;;


slice ["a";"b";"c";"d";"e";"f";"g";"h"] 2 6;;
slice ["a";"b";"c";"d";"e";"f";"g";"h"] 3 2;;
slice ["a";"b";"c";"d";"e";"f";"g";"h"] 3 20;;

  

let rec equivs_h fn= function
  |[] -> []
  |h::t -> 
      if (fn h) 
      then h::(equivs_h fn t)
      else equivs_h fn t;;

let rec equivs fn lst = 
  match lst with
  |[] -> [[]]
  |h::t -> 
      let l = (equivs_h (fn h) lst) in
      let t = remove_if t (fn h) in
      match t with
      | [] -> [l]
      | s::e -> l :: (equivs fn t);;
  
  
  
equivs (=) [1; 2; 3; 4; 2; 3; 4; 3; 4] ;;   
equivs (fun x y -> (=) (x mod 2) (y mod 2)) [1; 2; 3; 4; 5; 6; 7; 8];;


let rec is_prime x y= 
  if y > x/2 
  then true
  else (x mod y <> 0 && is_prime x (y+1));;

let rec prime_list x counter =
  if counter > x
  then []
  else if is_prime counter 2
  then counter::prime_list x (counter+1) 
  else prime_list x (counter+1) ;;

let goldbach n : int * int= 
  if n mod 2 <> 0 
  then (0,0)
  else let list = prime_list n 2 in
    let rec check l = 
      match l with
      | [] -> (0,0)
      |h::t ->if List.mem (n-h) l
          then (h,n-h)
          else check t in
    check list;;

let rec equivs_on f g lst = 
  match lst with 
  |[]-> true
  |h::t -> (f h) = (g h) && equivs_on f g t;;

let f i = i * i;;
let g i = 3 * i;;

  equivs_on f g [3];;
  equivs_on f g [1;2;3];;


let rec pairwisefilter cmp lst = 
  match lst with
  |[] ->[]
  |h::t -> if t <> []
      then (cmp h (List.hd t)) :: (pairwisefilter cmp (List.tl t))
      else h :: pairwisefilter cmp t;;


  pairwisefilter min [14; 11; 20; 25; 10; 11;23];;



let rec powerset list = 
  match list with
  | [] -> [[]]
  | h::t -> let set = powerset t in
      let rec add = function
        | [] -> []
        | s::e ->  (h :: s) :: add e in
      set @ (add set);;
        
        
powerset [3; 4; 10;23];;




  