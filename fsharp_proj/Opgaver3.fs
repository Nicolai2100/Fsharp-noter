// Learn more about F# at http://fsharp.org

module Opgaver3

open System

let main argv =
    printfn "Hello World from F#!"
    (* comments *)
  
    fun r -> System.Math.PI * r * r
   
    0 // return an integer exit code

//4.18

let rec f g = function
    | [] -> []
    | x::xs -> (g x) :: f (fun y -> g(g(y) ) ) xs ;;

f (fun x -> x + 1) [0;0;0;0;0];;

(* 1 *)
type Name = string ;;
type no = string;;
type yb = int;;
type Themes = string list;;
type Description = (no * yb * Themes) ;;
type Member = Name * Description;;
type Register = Member List ;;

type Arrangement = Member -> bool ;;

let p1 (name,(no, yb, ths)) = yb > 1982 && List.contains "jazz" ths;;

let m1: Member = ("Karl Karlsen", ("22342016", 1991, ["ost";"jazz"]));;
let m2: Member = ("Karl2 Karlsen2", ("22342016", 1977, ["ost";"jazz"]));;
let memberList:Register = [m1;m2];;
p1 (m1);;

//3. A declaration of a function extractTargetGroup p r that gives a list with names and 
//phone numbers of the members in register r that may be interested in the arrangement p. 
//State the type of extractTargetGroup in a comment. Make use of the type names introduced
//under point 1. above, so that the type reflects the intention with the function. 

// let extractTargetGroup (p : Arrangement, r : Register) = List.filter ( fun x -> p x ) r  ;;

let rec extractTargetGroup (p : Arrangement, r : Register) = 
    match r with
    | [] -> []
    | (name,Description)::register when p (name, Description) -> let newMemberList = extractTargetGroup (p, register) 
                                                                 let number,_,_ = Description
                                                                 (name, number)::newMemberList
    | _::register -> extractTargetGroup (p, register)  ;;      

extractTargetGroup (p1, memberList);;


(* 3.3 *)

(*
The set of complex numbers is the set of pairs of real numbers. Complex numbers behave almost
like real numbers if addition and multiplication are defined by:
(a, b) + (c, d) = (a + c, b + d)
(a, b) · (c, d) = (ac − bd, bc + ad)
1. Declare suitable infix functions for addition and multiplication of complex numbers.
2. The inverse of (a, b) with regard to addition, that is, −(a, b), is (−a,−b), and the inverse of
(a, b) with regard to multiplication, that is, 1/(a, b), is (a/(a2+b2),−b/(a2+b2)) (provided
that a and b are not both zero). Declare infix functions for subtraction and division of complex
numbers.
3. Use let-expressions in the declaration of the division of complex numbers in order to avoid
repeated evaluation of identical subexpressions.
*)

// 3.3 - 1
let (.+.) (a,b) (c,d) = (a+c, b+d);;
(.+.) (1, 2) (2,2);; 
(1, 2) .+. (2,2);; 

let (.*.)(a,b)(c,d) = (a*c - b*d, b*c + a*d);;
(1, 2) .*. (2,2);; 

// 3.3 - 2
let (./.) 1 (a:int,b:int) = ((float a/ float (a*a + b*b)),  (float -1* float b / float (a*a + b*b)));;
1 ./. (2, 5) ;;

// 3.3 - 3
(*
3. Use let-expressions in the declaration of the division of complex numbers in order to avoid
repeated evaluation of identical subexpressions.
*)

// Exercise on sorting, correctness and property-based testing (available on FileSharing Thursday)

//merge([1;4;9;12],[2;3;4;5;10;13]) = [1;2;3;4;4;5;9;10;12;13].
let rec merge (xs, ys) = 
    match (xs, ys) with
    |[], [] -> []
    |[], _ -> ys
    |_, [] -> xs
    |xHead::xTail, yHead::yTail -> if xHead < yHead 
                                   then xHead::merge (xTail, yHead::yTail) 
                                   else yHead::merge (yTail, xHead::xTail)    ;;
merge([0],[2;3;4;5;10;13]) ;;
merge([2;3;4;5;10;13],[0]) ;;
merge([2;3;4;5;10;13],[1;6;7;8;9;11;12]) ;;

(*
Declare a function to split a list into two lists of (almost) the same lengths. You may
declare the function split such that
split [x0; x1; x2; x3; : : : ; xn􀀀1] = ([x0; x2; : : :]; [x1; x3; : : :])
*)
let rec split = function
    | [] -> [],[]
    | head::tail -> let (a,b) = split tail
                    if tail.Length % 2 = 0 
                    then (head::a,b)
                    else (a,head::b);;
split([0;1;2;3;4;5;6]) ;;
                    

(*The sort function
The idea behind top-down merge sort is a recursive algorithm: take an arbitrary list xs
with more than one element and split it into two (almost) equal-length lists: xs1 and xs2.
Sort xs1 and xs2 and merge the results. The empty list and lists with just one element are
the base cases.
Declare an F# function for top-down merge sort.*)

let rec merge (xs, ys) = 
    match (xs, ys) with
    |[], [] -> []
    |[], _ -> ys
    |_, [] -> xs
    |xHead::xTail, yHead::yTail -> if xHead < yHead 
                                   then xHead::merge (xTail, yHead::yTail) 
                                   else yHead::merge (yTail, xHead::xTail);;
let rec split = function
| [] -> [],[]
| head::tail -> let (a,b) = split tail
                if tail.Length % 2 = 0 
                then (head::a,b)
                else (a,head::b);;

let rec sort = function
    | [] -> []
    | list when list.Length = 1 -> list
    | list -> 
              let splittedLists = split list
              let list1 = sort (fst splittedLists)
              let list2 = sort (snd splittedLists)
              merge (list1, list2);;
sort([7;6;4;2;1;0;14;0]) ;;

(*Declare an F# function ordered: int list -> bool to test whether a list is ordered.
A function that returns a truth value, such as ordered is also called a predicate.
One part of the correctness properties of sort can be expressed by the predicate:*)

let rec ordered = function
    |[] -> true
    |list when list.Length = 1 -> true 
    |xHead::yHead::xTail -> if xHead <= yHead 
                            then true && ordered (yHead::xTail) 
                            else false;;
ordered([7;6;4;2;1;0;14;0]) ;;
ordered([0;1;4]) ;;

//let ordered xs = List.fold (fun x y -> x >= y) xs    ;;
// List.fold (fun x y -> x + y) 0 [1; 2; 3] // val it : int = 6

let orderedSort(xs: int list) = ordered(sort xs)
orderedSort([7;6;4;2;1;0;14;0]) ;;
