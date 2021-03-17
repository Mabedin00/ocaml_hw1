let rec pow x = function 
    | 0 -> 1
    | n -> x * pow x (n-1);;
         

 let rec float_pow x = function
   | 0 -> 1.0
   | n -> float_pow x (n - 1) *. x;;

 let rec compress_h list curr_h = 
   match list with
   | [] -> []
   | h::t -> if h = curr_h 
       then compress_h t h 
       else h :: compress_h t h;;
  
 let compress list = compress_h list "";;

let rec remove_if list checker = 
  match list with
  | [] -> []
  | h::t -> if checker h
      then remove_if t checker
      else h::remove_if  t checker;; 


  let rec slice_h list s f i = 
    match list with
    | [] -> []
    | h::t -> if i >= s && i < f
        then h::slice_h t s f (i+1)
        else slice_h t s f (i+1) ;;

  let slice list s f = slice_h list s f 0;;
  

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


let rec pairwisefilter cmp lst = 
  match lst with
  |[] ->[]
  |h::t -> if t <> []
      then (cmp h (List.hd t)) :: (pairwisefilter cmp (List.tl t))
      else h :: pairwisefilter cmp t;;



let rec powerset list = 
  match list with
  | [] -> [[]]
  | h::t -> let set = powerset t in
      let rec add = function
        | [] -> []
        | s::e ->  (h :: s) :: add e in
      set @ (add set);;
        
        
  