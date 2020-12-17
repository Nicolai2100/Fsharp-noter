open Microsoft.FSharp.Collections

//9.3 - Declare an iterative solution to exercise 1.6
(*
Declare a recursive function sum: int * int -> int, where
sum(m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)
for m ≥ 0 and n ≥ 0. (Hint: use two clauses with (m,0) and (m,n) as patterns.)
Give the recursion formula corresponding to the declaration
*)

let rec sum (a,b) = 
    match (a,b) with
    | (m,0) -> m
    | (m,n) -> (m + n) + (sum (m, (n-1))) ;;

let rec sumIt (a,b) result = 
    match (a,b) with
    | (m,0) -> m + result
    | (m,n) when (a > 0 && b > 0)  -> (sumIt (m, (n-1)) (m + n + result))
    | _ -> failwith "illegal argument"
    ;;

#time;;
sumIt (1000,1000) 0;;
sum (1000,1000);;
sum (1,1);;
sumIt (1,1) 0;;
sum (1,2);;
sumIt (1,2) 0;;
sum (5,3);;
sumIt (5,3) 0;;
sum (1,4);;
sumIt (1,4) 0;;
sum (1,5);;
sumIt (1,5) 0;;
sum (1,6);;
sumIt (1,0) 0;;

let xs16 = List.init 1000000 (fun i -> 16);;
#time;;
for i in xs16 do let _ = sum (i,i) in ();;
for i in xs16 do let _ = sumIt (i,i) 0 in ();;


//9.4 - Give iterative declarations of the list function List.length
let rec length = function
    | [] -> 0
    | _::tail -> 1 + length tail ;;

let rec lengthIt result = function 
    | [] -> 0 + result
    | _::tail -> lengthIt (1+result) tail;;

let arr = [0 .. 1000] ;;

length arr 
lengthIt 0 arr;;

#time;;
let xs16 = List.init 1000000 (fun i -> 16);;

for i in xs16 do let _ = length arr in ();;
for i in xs16 do let _ = lengthIt 0 arr in ();;

//Summer exam 02157-2014, Problem 1(1-3.1)
(*The function f is not tail recursive.
Make a tail-recursive variant of f using an accumulating parameter. *)

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

#time;;

//Fall exam 02157-2015, Problem 2(1,2.1,3) -- available here
(*
2. The function g1 is not tail recursive.
• Make a tail-recursive variant of g1 using an accumulating parameter.*)

let rec g1 p = function
    | x::xs when p x -> x :: g1 p xs
    | _ -> [];;

let rec g1Tail p xs result : int list = 
    match (xs) with
    | [] -> List.rev result
    | x::xs when p x -> g1Tail p xs (x::result) 
    | x::_ when not (p x) -> [];;


g1 (fun x -> x > -1) [0..7] ;; 
g1Tail (fun x -> true) [0..7] [] ;; 

g1 (fun x -> x > 0) [0..7] ;; 
g1Tail (fun x -> x > 0) [0..7] [] ;; 

g1Tail (fun x -> x > 0) [0..7] [];; 

//3. The function g2 is tail recursive. Give a brief informal explanation of why
let rec g2 f h n x =
    match n with
    | _ when n < 0 -> failwith "negative n is not allowed"
    | 0 -> x
    | n -> g2 h f (n-1) (f x);;
//n styrer antallet af gange metoden skal køre
// h og f er funktioner der på skift køres på x
// værdien af funktionskaldene bliver sendt videre i x til næste kørsel af g2
// når n er 0 er resultatet x

let fx x = 2 * x;;
let hx x = x - 1;;

g2 fx hx 1 0;;
g2 fx hx 2 0;;
g2 fx hx 3 0;;
g2 fx hx 4 0;;
g2 fx hx 5 0;;
g2 fx hx 6 0;;
g2 fx hx 7 0;;
g2 fx hx 8 0;;
g2 hx fx 9 0;;
g2 fx hx 9 0;;

//ExerciseWeek9.pdf
(*Give an argument showing that 'a -> 'b list -> ('a*'b) list is the most general
type of f and that 'a list -> 'b list -> ('a*'b) list is the most general type of
allPairs. That is, any other type for f is an instance of 'a -> 'b list -> ('a*'b) list.
Similarly for allPairs.*)

(* Grunden til at 'a -> 'b list -> ('a*'b) list er den mest generelle type for f, er tofoldig. For det første er typen for x underordnet, da der ikke 
bliver udført andre operationer på x end at værdien bliver sat ind i hver tuple i den returnerede liste.
For det andet kræver mønsteret "| [] -> []" at både argument og værdi er en liste. Hvilken form for liste er her igen underordnet, da de enkelte 
elementer i argumentet 'b list kun bliver indsat i en liste af tuples

Det samme gælder for allPairs. Listernes typer er underordnet, det er dog nødvendigt at de er lister, da metoden itererer over alle elementer i xs,
og bruger ys som argument til f.
*)
let rec f x = function
    | [] -> []
    | y::ys -> (x,y)::f x ys;;
//val f : 'a -> 'b list -> ('a * 'b) list
(*
from the form... it takes a
from the first clause .. -> 'c list
from the secound clause 
xs must be a list due to its clause

for allPairs 
*)
let rec allPairs xs ys =
    match xs with
    | [] -> []
    | x::xrest -> f x ys @ allPairs xrest ys;;
//val allPairs : 'a list -> 'b list -> ('a * 'b) list

(*2. Give an evaluation showing that [("a", 1); ("a", 2); ("a", 3)] is the value of the
expression f "a" [1;2;3]. Present your evaluations using the notation e1   e2 from the
textbook. You should include at least as many evaluation steps as there are recursive calls.*)
(*  
f "a" [1;2;3]
~> ("a",1)::(f "a" [2;3])    
~> ("a",1)::("a",2)::(f "a" [3])    
~> ("a",1)::("a",2)::("a",3)::(f "a" [])    
~> ("a",1)::("a",2)::("a",3)::[] = [("a",1);("a",2);("a",3)]
*)

//3. Explain why the type of f "a" [1;2;3] is (string * int) list.
(*
Når f modtager en tom liste som argument er værdien en tom generisk liste "| [] -> []". Derfor er værdien af f altid en generisk liste, 
som får en bestemt type afhængig af argumenternes typer.
Årsagen er at "a" er af typen string, hvert element i liste er af typen int, når de sættes sammen i en tuple
bliver typen derfor string * int, når disse tuples indsættes i en liste bliver listens type altså "(string * int) list"
*)

//4: The declaration of f is not tail recursive. Explain briefly why this is the case.
(*
f er ikke tail recursive. Dette kan ses ved at værdien for f bliver bygget op i stacken i stedet for at blive sendt med som argument
til det næste rekursive kald.
| y::ys -> (x,y)::f x ys;;
*)

(* 5: Provide a declaration of a tail-recursive variant of f that is based on an accumulating
parameter. Your tail-recursive declaration must be based on an explicit recursion. *)

let rec f x accu = function
| [] -> List.rev accu 
| y::ys -> f x ((x,y)::accu) ys;;
//val f : x:'a -> accu:('a * 'b) list -> 'b list -> ('a * 'b) list

(*6: Give another declaration of f that is based on a single higher-order function from the List
library. The new declaration of f should not be recursive.*)

let f x ys = List.foldBack (fun y accu -> (x,y)::accu) ys [];;

let f x ys = List.foldBack (fun y accu -> (x,y)::accu) ys [];;

List.map (fun y -> (x,y)) ys
List.allPairs [x] ys
f "a" ys ;;
