// Learn more about F# at http://fsharp.org

open System

let main argv =
    printfn "Hello World from F#!"
    (* comments *)
  
    fun r -> System.Math.PI * r * r
   
    0 // return an integer exit code


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
    | [], [] -> [] 
    | x::rextx, y::resty -> let (list) = zip rextx resty
                            (x, y)::list;;
zip [1;2] [1;2];;


   

//4.12
(*
p(x) x > 0
the sum of elements in xs satisfying the condition of px
*)

let p x = if x > 0 then true else false;;

p 2;;
p -1;;
p 0;;

// let p x = x > 0;;

//let rec sum xs = if p(x::xs) then let realSum = sum(xs)
//                                  realSum 
//                                  else 0;;


//let rec sum p xs
//    function
//    | [] -> 0
//    | x:xs when p(x) -> let (realSum) x = sum(xs)
//                        1+realSum;;


let rec split = 
    function
    | [] -> ([],[]) 
    | x1::x2::rest -> let (a, b) = split rest
                      (x1::a, x2::b);;
split [1;2;1;2];;
                       