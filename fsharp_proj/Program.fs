// Learn more about F# at http://fsharp.org

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

    (* *)

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
mul ([2;3;0;1],[1;2;3]);;
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


let rec repeat f i x =
    if i = 0 then x else repeat f (i-1) (f x);;
(* 
        type repeat (int -> int) -> int -> int -> int
(argument) -> result
 x could be float, not i
 computed:
 repeat h 2 0 
 repeat h 1 (h 0) = 1 
 repeat h 0 (h 1) = 2
 2
*)


(* 
    Uge 2
    HR 2.1, 2.2, 2.13, 4.3, 4.8, 4.9, 4.12
    Addendum to Exercise on polynimials: Part 1
    Declare a higher-order function: f so that add = f (+) and sub = f (-)
    Exercise on polynomials: Part 2. You may also consider/start on Part 3

*)

//2.1 f: int -> bool


let f (x) = ((x % 2 = 0 || x % 3 = 0) && x % 5 <> 0) ;;

f (24);;
f (27);;
f (29) ;;
f (30);;


//2.2 f: int -> bool
let rec pow (str, x) = 
    match (x) with
        | (0) -> ""
        | (x') when x' > 0 -> str + pow(str, x-1);;

pow ("2", 2);;
pow ("Hej ", 4);;
pow ("Hej ", 0);;
pow ("Hej ", -4);;


//pow: string * int -> string

//2.13, f: int -> bool

(*  

 Thefunctions curry and uncurry of types 
 curry :(’a * ’b -> ’c) -> ’a -> ’b -> ’c 
 uncurry : (’a -> ’b -> ’c) -> ’a * ’b -> ’c are defined in the following way: 
 curry f is the function g where gx is the function h where hy = f ( x, y ) . 
 uncurry g is the function f where f ( x, y ) is the value hy for the function h = gx . 
*)
let rec curry (str, x) = 
    match (x) with
        | (0) -> ""
        | (x') when x' > 0 -> str + pow(str, x-1);;

let rec uncurry (str, x) = 
    match (x) with
        | (0) -> ""
        | (x') when x' > 0 -> str + pow(str, x-1);;
        

pow ("2", 2);;
pow ("Hej ", 4);;
pow ("Hej ", 0);;
pow ("Hej ", -4);;



//4.3 declarefunction evenN: int -> int list such that evenN n generates the
// list of the first n non-negative even numbers., 

let evenN (x) = [0 .. 2 .. (x-1)*2];;
evenN 4;;





//4.8,  DeclareanF#function split such that: split
// [ x 0 ; x 1 ; x 2 ; x 3 ; ... ; x n − 1 ] =( [ x 0 ; x 2 ; ... ] , [ x 1 ; x 3 ; ... ] )  
    

let rec split = 
    function
    | [] -> ([],[]) 
    | x1::x2::rest -> let (a, b) = split rest
                      (x1::a, x2::b);;
split [1;2;1;2];;


//4.9,  DeclareanF#function split such that: split
(*
ip([ x 0 ; x 1 ; ... ; x n − 1 ],[ y 0 ; y 1 ; ... ; y n − 1 ]) = [( x 0 ,y 0 );( x 1 ,y 1 ); ...
;( x n − 1 ,y n − 1 )]
*)    

//let rec split = 
//    function
//    | [] -> ([],[]) 
//    | x1::x2::rest -> let (a, b) = split rest
//                      (x1::a, x2::b);;
//split [1;2;1;2];;

let rec zip xs ys =
    match (xs, ys) with
    | [], [] -> [(0,0)] 
    | x::rextx, y::resty -> let (list) = zip rextx resty
                            (x, y)::list;;
zip [1;2] [1;2];;


//| x::xs -> let (a, b) = zip (xRest, yRest)   (x::a, y::b)

split [1;2;1;2];;

    // 4.9, 4.12