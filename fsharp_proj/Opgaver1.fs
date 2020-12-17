// Learn more about F# at http://fsharp.org
module Opgaver1

open System

let main argv =
    printfn "Hello World from F#!"
    (* comments *)
  
    fun r -> System.Math.PI * r * r
   
    0 // return an integer exit code


let circleArea r = System.Math.PI * r * r

(*  Declarearecursivefunction f: int -> int ,where f ( n )=1+2+ ··· +( n − 1) + n for n ≥ 0 . 
(Hint: use two clauses with 0 and n as patterns.) 
State the recursion formula corresponding to the declaration. Give an evaluation for f (4) . *)

let rec f n = 
    match n with
    | 0 -> 0
    | n -> n + f(n-1);; 

let rec f2 (m,n) = 
    match (m,n) with
    | (_,0) -> m
    | (m,_) -> m + n + f2 (m, n-1) 
    ;;

let rec f3 (n,k) = 
    match (n,k) with
    | (n,0) -> 1
    | (n, k) when k = n -> 1
    | (n, k) -> f3(n-1, k-1) + f3(n-1, k)
    ;;

    (*  DeclareanF#function multiplicity x xs to find the number of times the value x occurs in the list xs .*)

let rec f5 (xs, s) =
    match (xs) with
    | ([]) -> 0
    | head::tail when head = s -> 1 + f5 (tail,s)
    | _::tail ->  f5 (tail,s);;
f5 ([1;2],2);;