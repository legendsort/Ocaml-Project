type command = Up | Down | Left | Right | Seq of command list

(* Question 5-1 *)
exception E2 of string;; 

let rec evalpos (c : command) : (int * int) =
  match c with
  | Up -> 0, 1
  | Down -> 0, -1
  | Left -> -1, 0
  | Right -> 1, 0
  | Seq (ls) -> match ls with
    | [] -> 0, 0
    | h :: t -> 
        match t with
        | [] -> evalpos h
        | t -> 
            let (x, y) = evalpos (Seq t) in
            match h with
            | Up when y < 100 -> x, y + 1 
            | Down when y > -100 -> x, y - 1
            | Left when x > -100 -> x - 1, y
            | Right when x < 100 -> x + 1, y
            | _ -> raise (E2 "The turtle leaves the field.")
  
;;
(* check: evalpos (Seq [Up; Up; Left; Down; Right; Right]) *) 

