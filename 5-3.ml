type order = Up | Down | Left | Right
type command = Dir of order | Repeat of (int * order) | Seq of command list | Infinite of order

(* Question 5-3 *)
exception E2 of string;; 
exception E
  
let rec evalpos (c : command) : (int * int) =
  match c with
    
  | Seq (ls) -> 
      begin
        match ls with
      
        | [] -> 0, 0
        | h :: t -> 
        
            match t with
          
            | [] -> evalpos h
            | t -> 
                let (x, y) = evalpos (Seq t) in 
                match h with 
                | Dir(order) -> 
                    begin
                      match order with 
                      | Up when y < 100 -> x, y + 1 
                      | Down when y > -100 -> x, y - 1
                      | Left when x > -100 -> x - 1, y
                      | Right when x < 100 -> x + 1, y
                      | _ -> raise (E2 "The turtle leaves the field.")
                    end     
                | Repeat(n, order) -> 
                    begin
                      match order with 
                      | Up when y + n <= 100 -> x, y + n
                      | Down when y - n >= -100 -> x, y - n
                      | Left when x - n >= -100 -> x - n, y
                      | Right when x+ n <= 100 -> x + n, y
                      | _ -> raise (E2 "The turtle leaves the field.")
                    end
                | _ -> 0, 0
      end
      
  | Dir(order) -> 
      begin
        match order with 
        | Up -> 0, 1
        | Down -> 0, -1
        | Left -> -1, 0
        | Right -> 1, 0
      end     
  | Repeat(n, order) -> 
      begin
        match order with 
        | Up when n <= 100 -> 0, n
        | Down when n <= 100 -> 0, -n
        | Left when n <= 100 -> -n, 0
        | Right when n <= 100 -> n, 0
        | _ -> raise (E2 "The turtle leaves the field.") 
      end 
  | _ -> 0, 0
;;


(* check: evalpos (Seq [Repeat (2, Up); Dir Up; Dir Left; Dir Down;  Dir Right; Dir Right]) -> (1, 2) *) 

