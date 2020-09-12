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
let rec mulC (k, poly) =
    match (poly) with
    | ([]) -> []
    | (x::xtail) -> k * x ::mulC(k, xtail)  

mulC (2, [1; 2; 3; 1]) ;;
mulC (2, [2; 0; 0; 1]) ;;


// sub: Poly -> Poly -> Poly

//let rec sub (poly1, poly2) =
//    match (poly1, poly2) with
//    | ([], head2::poly2) -> - head2::sub(poly1, poly2)
//    | (poly1, []) -> poly1
//    | (head1::poly1, head2::poly2) -> head1 - head2::sub(poly1, poly2)  

let sub (p1, p2) = add (p1, (mulC (-1, p2)));;
   
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

(* Poly 2 *)

(*
The function isLegal: int list -> bool
The function isLegal tests whether an integer lists is a legal representation of a polynomial.
*)

// let isLegal ns =  if List.rev ns.Head = 0 then false else true;;
//let isLegal ns = function 
//    | List.rev ns.Head = 0 then false else true;;


let isLegal (ns) = 
    let reversed = List.rev ns
    match(reversed) with
    | [] -> true
    | head::_ -> if head <> 0 then true 
                                 else false;;
isLegal ([])
isLegal ([0;0;0])
isLegal ([0;0;1])
isLegal ([0;0;1;0])

(*
The function prune: int list -> Poly
Any integer list can be turned into a legal representation of a polynomial by removal of 0's
occurring at the end of the list. The function prune should do this
*)

let rec prune (ns) = if isLegal(ns) then ns else
    let revNs = List.rev ns
    match(revNs) with
    | [] -> []
    | head::tail when head = 0 -> prune (List.rev (tail))
    | head::tail ->  List.rev (head::tail);;
    
prune ([])
prune ([0;0;0])
prune ([0;0;1])
prune ([0;1;2;0;0;0])

(*
The function toString: Poly -> string
Choose an appealing textual representation of a polynomial and declare an associated
toString function. (You may have a look at Appendix B.)
*)
let rec toString (ns) =
        let revNs =  if not (isLegal (ns)) then List.rev (prune(ns)) else List.rev ns
        match(revNs) with
        | [] -> ""
        | head::tail -> let str = toString (List.rev tail) 
                        
                        //Last run
                        if tail.Length > 1 then str + string(head) + "x^" + string tail.Length
                     
                        else str + string(head) + "x^" + string tail.Length  + " + "
    ;;
toString ([1;2;3;0;0])
toString ([1;2;3])

toString ([])
toString ([0;0;0])
toString ([0;0;1])


let snaps = "";;
   snaps.Length;;

(*
function derivative: Poly -> Poly
For a polynomial P(x) = a0 + a1  x + a2  x2 + ::: + an  xn, we recall that the derivative is
P0(x) = a1 + 2  a2  x + ::: + n  an  xn􀀀1

*)


(*
The function compose: Poly -> Poly -> Poly
The composition of polynomials P(x) and Q(x) is dened by: (P  Q)(x) = P(Q(x)).
For example, if P(x) = 2 + 4x3 and Q(x) = 3x + 2x2, then
(P  Q)(x) = P(Q(x)) = 2 + 4(3x + 2x2)3 = 2 + 108x3 + 216x4 + 144x5 + 32x6
Therefore, compose [2; 0; 0; 4] [0; 3; 2] should give [2; 0; 0; 108; 216; 144; 32].
*)

(*
Remark: It is possible to dene a function computing the integral of a polynomial. This,
however, requires division of numbers and for that it would be natural to base the decla-
rations on oating point numbers rather than on integers.
However, 
oating point numbers (type float) only provide approximations of the real
numbers, and these approximations will cause many extra technicalities when we con-
sider property-based testing in Part 5. These technicalities are not particularly related to
functional programming, so we stick to the integer-based types in this exercise.*)