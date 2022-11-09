type grid = int list list;;

(* question 3.1 *)
let g_example : grid = [[-2; 0; 1; 4]; [7; 2; -3; -4]; [6; -1; 3; 5]];;

(* question 3.2 *)
let rec length l = 
  match l with
  | [] -> 0 
  | _ :: t -> 1 + length t;; 

exception E2 of string;;

let height (g : grid) : int = 
  match g with
  | [] -> raise(E2 "Grid is empty")
  | h :: t -> length h;;

height g_example;;
(* check: height g_example *)

(* question 3.3 *)

let rec check_well_formed g = 
  match g with
  | [] | [[]] -> false
  | [a] -> true
  | h :: t -> if length h = height t then true else false;;

let wf_grid_exn (g : grid) : unit =
  if check_well_formed g = false then raise(E2 "Grid is not well-formed");;
(* check: wf_grid_exn g_example *)

(* question 3.4 *)
(* The answer of the example grid is 17. (3, 0) -> (0, 1) -> (0, 2)*)


(* question 3.5 *)
let rotate_up (arr : 'a list) : 'a list = 
  match arr with
  | [] -> [] 
  | h :: t -> t @ [h];;
(* check: rotate_up [-2; 0; 1; 4] *)

(* question 3.6 *)
let rotate_down (arr : 'a list) : 'a list = 
  List.rev (rotate_up (List.rev arr));;
(* check: rotate_down [-2; 0; 1; 4] *)

(* question 3.7 *)
let rec add a b =
  match a, b with
  | [], [] -> []
  | [], r -> []
  | l, [] -> []
  | hd1 :: tl1, hd2 :: tl2 -> (hd1 + hd2) :: (add tl1 tl2);;


let rec max a b c = 
  match a, b, c with
  | [], [], [] -> []
  | [], _, _ -> []
  | _, [], _ -> []
  | _, _, [] -> []
  | hd1 :: tl1, hd2 :: tl2, hd3 :: tl3 -> Stdlib.max hd1 (Stdlib.max hd2 hd3) :: (max tl1 tl2 tl3);;


let best_option (a : int list) (b : int list) : int list = 
  let next_up = add a (rotate_up b) in
  let next_same = add a b in
  let next_down = add a (rotate_down b) in 
  max next_up next_same next_down;;
(* check: best_option [-2; 0; 1; 4] [7; 2; -3; -4] *)

(* question 3.8 *) 
let rec get_sum a b =
  match b with
  | [] -> a
  | h :: t -> 
      get_sum (best_option a h) t;;

let sum (g : grid) : int list = 
  match g with
  | [] -> []
  | h :: t -> get_sum h t;;

(* check: sum [[-2; 0; 1; 4]; [7; 2; -3; -4]; [6; -1; 3; 5]] *)

(* question 3.9 *) 
let rec max_list (arr : int list) : int = 
  match arr with
  | [] -> raise(E2 "The array is empty")
  | h :: t -> 
      match t with
      | [] -> h
      | _ -> Stdlib.max h (max_list t);; 

(* check: max_list [11; 13; 8; 7] *)
    
(* question 3.10 *) 
let rec solve (g : grid) : int =
  max_list (sum g);;
(* check: g_example *)
solve g_example;;
           
  
  
