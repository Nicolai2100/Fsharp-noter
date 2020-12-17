open Microsoft.FSharp.Collections

(*9.6 Declare a continuation-based version of the factorial function and compare the run time with
the results in Section 9.4.*)
//9.4 - Give iterative declarations of the list function List.length
let rec length = function
    | [] -> 0
    | _::tail -> 1 + length tail ;;

let rec lengthIt result = function 
    | [] -> 0 + result
    | _::tail -> lengthIt (1 + result) tail;;

let rec lengthCon xs k = 
    match xs with  
    | [] -> k 0
    | _::tail -> lengthCon tail (fun v -> k(v + 1));;

let arr = [0 .. 1000] ;;

length arr 
lengthIt 0 arr;;
lengthCon arr id

//Svær continuation based
type Tree<'a,'b> = | A of 'a 
                   | B of 'b
                   | Node of Tree<'a,'b> * Tree<'a,'b>;;

let rec fK t k = match t with
    | A a         -> k([a],[])
    | B b         -> k([], [b])
    | Node(t1,t2) -> fK t1 (fun (xs1,ys1) -> fK t2 (fun (xs2,ys2) -> k(xs1@xs2,ys1@ys2)));;
fK (Node(Node(Node(A 2, B "b"), A 3), Node(A 2, B "a"))) id





(*9.8 Develop a version of the counting function for binary trees
countA: int -> BinTree<’a> -> int
that makes use of an accumulating parameter. 
Observe that this function is not tail recursive.*)
type BinTree<'a> = | Leaf
                   | Node of BinTree<'a> * 'a * BinTree<'a>

let rec count = function
    | Leaf -> 0
    | Node(tl,n,tr) -> count tl + count tr + 1

let rec countA acc bTree =
    match bTree with
    | Leaf -> acc
    | Node(tl,n,tr) -> 1 + countA (0 + acc) tl + (countA (0 + acc) tr)

countA 0 Leaf 
countA 0 (Node(Leaf,0,Node(Leaf,1,Leaf))) 
count (Node(Leaf,0,Node(Leaf,1,Leaf))) 
Node(Leaf,0,Node(Leaf,1,Leaf))

//Summer exam 02157-2014, Problem 1.3.2
//2. Make a continuation-based tail-recursive variant of f.
let rec f n = function 
    | 0 -> 1
    | k when k>0 -> n * (f n (k-1))
    | _ -> failwith "illegal argument";;

let rec fTail n k result = 
    match k with
    | 0 ->  result
    | k when k > 0 && result > 0 ->  (fTail n (k - 1) (n * result))
    | k when k > 0 ->  (fTail n (k - 1) (n * (result + 1)))
    | _ -> failwith "illegal argument";;

f 2 1;;
fTail 2 1 0;;
f 2 2;;
fTail 2 2 0;;
f 2 3;;
fTail 2 3 0;;
f 2 4;;
fTail 2 4 0 ;;
f 2 10;;
fTail 2 10 0;;
f 2 13;;
fTail 2 13 0;;

//Fall exam 02157-2013, Problem 2(1+2)

let rec f i = function
    | [] -> []
    | x::xs -> (i,x)::f (i*i) xs;;

type 'a Tree = | Lf
               | Br of 'a Tree * 'a * 'a Tree;;
let rec g p = function
    | Lf -> None
    | Br(_,a,t) when p a -> Some t
    | Br(t1,a,t2) -> match g p t1 with
                     | None -> g p t2
                     | res -> res;;

//Please remember that the delaration of 'a option istype 'a option = None | Some of 'a;

//1. Give the types of f and g and desribe what eah of these two funtions omputes.
//Your desription for eah funtion should fous on what it omputes, rather than onindividual omputation steps.

//f tager to argumenter, i er af typen int eller float, grundet operatoren "*", listen er en generisk 'a list, da
//der ikke foretages operationer på elementerne. 
//Værdien af f er en 'b list med alle elementerne fra argumentet, sat ind som andet del af en en liste af tuples
//hvor i^1 ... i^n er første del - (int * 'a) list

//g søger et træ igennem for af returnere den branch, der tilfredssiller predikatet p. 
//Det kunne være et navn eller id den søgte efter 
// p er et predikat

//2. The funtion f is not tail recursive.
//1. Make a tail-re cursive variant of f using an acmulating parameter.
let rec f i = function
    | [] -> []
    | x::xs -> (i,x)::f (i*i) xs;;

let rec fA i accu = function
    | [] -> List.rev accu
    | x::xs -> fA (i*i) ((i,x)::accu) xs;;

//2. Make a continuation-based tail-recursive variant of f.
let rec f i accu = function
    | [] -> List.rev accu
    | x::xs -> f (i*i) ((i,x)::accu) xs;;

let rec fCon i k = function
    | [] -> k []
    | x::xs -> fCon (i*i) (fun v -> k((i,x)::v)) xs  ;;

f 2 ["a"; "b";"c";"d"]
fA 2 [] ["a"; "b";"c";"d"]
fCon 2 (fun x -> x) ["a"; "b";"c";"d"] 

//3. Give a brief discussion of which tail-reursive version of f you prefer?
//fA er i dette tilfælde at foretrække, da functionen vil performe hurtigere end fCon

//Fall exam 02157-2015, Problem 2.2.2
(*2. The function g1 is not tail recursive.
• Make a continuation-based tail-recursive variant of g1.*)
let rec g1 p = function
    | x::xs when p x -> x :: g1 p xs
    | _ -> [];;

let rec g1Con k p = function
    | x::xs when p x -> g1Con (fun v -> k(x::v)) p xs
    | _ -> k [];;
g1Con (fun x -> x) (fun x -> x < "e") ["a"; "b";"c";"d"]
g1Con (fun x -> x) (fun x -> x > "c") ["a"; "b";"c";"d"]