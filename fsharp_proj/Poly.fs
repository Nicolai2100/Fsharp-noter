// Learn more about F# at http://fsharp.org
module Poly1

open System

let main argv =
    printfn "Hello World from F#!"
    (* comments *)
  
    fun r -> System.Math.PI * r * r
   
    0 // return an integer exit code

(* Poly projekt part 1 *)

// add Poly -> Poly -> Poly

let rec add (poly1, poly2) =
    match (poly1, poly2) with
    | ([], poly2) -> poly2
    | (poly1, []) -> poly1
    | (head1::poly1, head2::poly2) -> head1 + head2::add(poly1, poly2)  

add ([],[1;2]);;
add ([2;1],[]);;
add ([1;2],[1;0;2]);;


// mulC: int -> Poly -> Poly

//let rec mulC (x, poly) =
//    match (x, poly) with
//    | (0, _) -> []   
//    | (_, []) -> []
//    | (1, _) -> poly
//    | (_, head::poly) -> x * head::mulC(x,poly)  

let rec mulC (k, poly) =
    match (poly) with
    | ([]) -> []
    | (x::xtail) -> k * x ::mulC(k, xtail)  

mulC (2, [1; 2; 3; 1]) ;;
mulC (2, [2; 0; 0; 1]) ;;


// sub: Poly -> Poly -> Poly

let rec sub (poly1, poly2) =
    match (poly1, poly2) with
    | ([], head2::poly2) -> - head2::sub(poly1, poly2)
    | (poly1, []) -> poly1
    | (head1::poly1, head2::poly2) -> head1 - head2::sub(poly1, poly2)  

sub ([],[1;2]);;
sub ([2;1],[]);;
sub ([1;2],[1;0;2]);;


// mulX: Poly -> Poly

let rec mulX (poly) =
    match (poly) with
    | ([]) -> []
    | (poly) -> 0::poly

mulX [2;0;0;1];;


// mul: Poly -> Poly -> Poly
let rec mul (poly1, poly2) =
    match (poly1, poly2) with
    | ([], _) -> []
    | (_, []) -> []
    | (head1::poly1, poly2) -> add((mulC(head1, poly2), mul(poly1, mulX(poly2)))) ;;                          
mul ([2;3;0;1], [1;2;3]);;
//2 + 7x + 12x2 + 10x3 + 2x4 + 3x5.


// eval: int -> Poly -> int

let eval3 (a, xs) = 
    let rec solve a (xs) = 
        match(xs) with
        | [] -> 0
        | head::tail -> head * power(a, tail.Length) + solve a tail
    let reversed = List.rev xs
    solve a reversed;;               

eval3 (2, [2; 3; 0; 1]);;
