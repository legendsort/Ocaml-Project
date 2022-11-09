(* question 4-1 *)

exception E2 of string;; 
           
let rec fibo (n : int) : int =
  match n with
  | 0 -> 0
  | 1 -> 1
  | n when n > 1 -> fibo (n - 1) + fibo (n - 2)
  | n -> raise(E2 "N is negatve.");;

(* check: fibo 8 *)

(* question 4-2 *)
let count = ref 0;; 
             
let rec fibocount (n : int) : int =
  count := !count + 1; 
  
  match n with
  | 0 -> 0
  | 1 -> 1
  | n when n > 1 -> fibocount (n - 1) + fibocount (n - 2)
  | n -> raise(E2 "N is negatve.");;


fibocount(1);;
!count;;
fibocount(2);;
!count;; 
fibocount(6);;
!count;;
fibocount(10);;
!count;;
fibocount(12);;
!count;; 
(*
  fibocount(1) -> 1
  fibocount(2) -> 1
  fibocount(6) -> 8
  fibocount(10) -> 206
  fibocount(12) -> 671
*)  
(* question 4-3 *)
let f = ref (List.init 1024 (fun x -> -1));; 
let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;


let rec fibocheck (n : int) : int =
  match n with
  | 0 -> 0
  | 1 -> 1
  | n -> if List.nth !f n != -1 then List.nth !f n else 
        let temp = memofibocount n in
        f := replace !f n temp;
        List.nth !f n
    
and 
  memofibocount (n : int) : int =
  count := !count + 1; 
  match n with
  | 0 -> 0
  | 1 -> 1
  | n -> fibocheck (n-1) + fibocheck (n-2);;



(* count is simiar with n *)

(* question 4-4 *) 
count := 0 ;;

memofibocount(1);;
!count;;
memofibocount(2);;
!count;; 
memofibocount(6);;
!count;;
memofibocount(10);;
!count;;
memofibocount(12);;
!count;; 
(*
  fibocount(1) -> 1
  fibocount(2) -> 2
  fibocount(6) -> 7
  fibocount(10) -> 12
  fibocount(12) -> 15

  memorized calculation's time complexity is O(n) but original is very big.
  so it cannot get the result on time.
*)

