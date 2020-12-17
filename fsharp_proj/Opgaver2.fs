// Learn more about F# at http://fsharp.org

open System

let main argv =
    printfn "Hello World from F#!"
    (* comments *)
  
    fun r -> System.Math.PI * r * r
   
    0 // return an integer exit code


let rec repeat f i x =
    if i = 0 then x else repeat f (i-1) (f x);;

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

let rec zip xs ys =
    match (xs, ys) with
    | [], [] -> [] 
    | x::rextx, y::resty -> let (list) = zip rextx resty
                            (x, y)::list;;
zip [1;2] [1;2];;

//4.12
(*
p(x) x > 0
the sum of elements in xs satisfying the condition of px
*)

let p x = x > 0;;
p 2;;
p -1;;
p 0;;

let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

let rec sum p x xs = 
    if p(x) && x = head::xs  then 1 + sum(p x xs)
    else 0 + sum(p x xs)  ;;
                                
let rec sum2 (p, x, xs) =
    match (xs) with
    | ([]) -> 0
    | (head::xs) when head <> x -> sum2(p, x, xs )
    | (head::xs) when p(x) && head = x -> 1 + sum2(p, x, xs );;  

sum2 (p, 1,[1;2]);;
sum2 (p, 2,[]);;
sum2 (p, 3,[1;0;2]);;
sum2 (p, 3,[3;0;3]);;