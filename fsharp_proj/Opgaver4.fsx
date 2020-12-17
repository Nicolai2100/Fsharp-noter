#I @"C:\Users\NicolaiLarsen\.nuget\packages\fscheck\2.14.3\lib\net452"
#r @"FsCheck.dll"

open FsCheck
let commProp(x,y) = x+y = y+x;;
let commPropTest = Check.Quick commProp;;
let commPropTestVerbose = Check.Verbose commProp;;
//C:\Users\NicolaiLarsen\.nuget\packages\fscheck\2.14.3\lib\net452

// 2 - HR 5.3
(*Solve Exercise 4.12 using List.fold or List.foldBack.
Declare a function sum(p, xs) where p is a predicate of type int -> bool and xs is a list of
integers. The value of sum(p, xs) is the sum of the elements in xs satisfying the predicate p.
Test the function on different predicates (e.g., p(x) = x > 0).*)

let p x = x > 0;
//gammel sum
let rec sum (p, xs) =
    match (xs) with
    | ([]) -> 0
    | (head::tail) when p(head) -> 1 + sum(p, tail )
    | (head::tail) -> sum(p, tail ) ;;  

sum (p, [1;2]);;
sum (p, []);;
sum (p, [1;0;2]);;
sum (p, [3;0;-3]);;

let p x = x > 0; 
// rækkefølgen af argumenterne skal være omvendt for fold / unfold
let sum (p, xs):int = List.foldBack (fun x n -> if p (x) then n + 1 else n) xs 0 ;;
let sum (p, xs):int = List.fold (fun n x -> if p (x) then n + 1 else n) 0 xs ;;
sum (p, [1;2]);;
sum (p, []);;
sum (p, [1;0;2]);;
sum (p, [3;0;-3]);;

(*
3 - Make a revised version of the Cash register example in Section 4.6 where:
The function findArticle is replaced by an application of List.tryFind
*)
type ArticleCode = string;;
type ArticleName = string;;
type Price = int;; // pr where pr >= 0
type Register = (ArticleCode * (ArticleName*Price)) list;;

type NoPieces = int;; // np where np >= 0
type Item = NoPieces * ArticleCode;;
type Purchase = Item list;;

type Info = NoPieces * ArticleName * Price;;
type Infoseq = Info list;;
type Bill = Infoseq * Price;;

let pur = [(3,"a2"); (1,"a1")];;

//findArticle: ArticleCode -> Register -> ArticleName * Price
let rec findArticle ac = function
    | (ac', adesc)::_ when ac=ac' -> adesc
    | _::reg -> findArticle ac reg
    | _ ->
    failwith(ac + " is an unknown article code");;
//val findArticle : string -> (string * ’a) list -> ’a

let reg = [("a1",("cheese",25));
    ("a2",("herring",4));
    ("a3",("soft drink",5)) ];;

findArticle "a2" reg;;

// The function makeBill is declared using List.foldBack
let rec makeBill reg = function
    | [] -> ([],0)
    | (np,ac)::pur -> let (aname,aprice) = findArticle ac reg
                      let tprice = np*aprice
                      let (billtl,sumtl) = makeBill reg pur
                      ((np,aname,tprice)::billtl,tprice+sumtl);;
makeBill reg pur;;

let makeBill (pur, reg) = List.foldBack (fun (np,ac) (n, y) -> 
(np, fst (findArticle ac reg), np * snd (findArticle ac reg))::n, y+np * (snd (findArticle ac reg)))  pur ([], 0) ;;
makeBill (pur, reg);;

(* 
4 - Solve the following two problems from old exams, where you should start with solutions based on "plain" recursive functions. 
Provide, thereafter, solutions to questions using functions from the List library, when that makes sense.
a- Problem 2 from Exam, Summer 2015.

1. Declare a function mixMap so that
mixMap f [x0; x1; . . . ; xm] [y0; y1; . . . ; ym] = [f(x0, y0); f(x1, y1); . . . ; f(xm, ym)]
2. Declare a function unmixMap so that
unmixMap f g [(x0, y0); (x1, y1); . . . ; (xn, yn)] = ([f x0; f x1; . . . ; f xn], [g y0; g y1; . . . ; g yn])
3. Give the most general types for mixMap and unmixMap.
*)

let f (x, y) = (x * 2, y * 2);
let f2 x = (x * x);
let g x = x * 2;

let rec mixMap (f, xs, ys) =
    match (xs, ys) with
    | ([],[]) -> []
    | (xHead::xTail, yHead::yTail) -> let a = mixMap (f, xTail, yTail)
                                      f (xHead, yHead)::a ;;  
mixMap (f, [1;1;1],[2;2;2]);;
mixMap (f, [],[]);;

let rec unMixMap (f, g, xs) =
    match xs with
    | ([]) -> ([],[])
    | ((xHead, yHead)::tail) -> let (a,b) = unMixMap (f, g, tail)
                                (f xHead::a, g yHead::b) ;;  

unMixMap (f2, g, [(1, 2); (1, 2); (3, 4); (3, 4) ]);;

// Solutions to questions using functions from the List library.

let f (x, y) = (x * 2, y * 2);
let f2 x = (x * x);
let g x = x * 2;

// let mixMap2 (f, (xs, ys)  : 'a list * 'b list) = List.foldBack (fun (x, y) n  ->  f (x::xs, y::ys)::n) (xs, ys) [] : (int * int) list ;;
// mixMap2 (f, ([1;2;3],[3;2;1]));;
// mixMap2 (f, [],[]);;
// 3. Give the most general types for mixMap and unmixMap.
// ints, floats?

(* 
b- Problem 1(questions 1- 5) from Exam, Fall 2013.
*)
// 1. Declare a function inv: Multiset<'a> -> bool such that inv(ms) is true when ms
// satisfies the multiset invariant

type Multiset<'a when 'a : equality> = ('a * int) list;;
let invariantS (inv:Multiset<'a>) = List.foldBack (fun (x,y) n  -> n = (List.contains (x, y) inv) && n) inv true ;;

let set1 = [("b",3); ("a",5); ("d",1)];;
let set2 = [("b",3); ("a",5); ("d",1); ("b",3);];;

let rec invariantS2 xs =
    match xs with
    | ([]) -> true
    | ((x,y)::tail) -> let a = invariantS2 tail
                       a && not (List.contains (x, y) tail) ;;  

invariantS2 set1;;
invariantS2 set2;;
invariantS2 set1;;

(*
Declare a function insert: 'a -> int -> Multiset<'a> -> Multiset<'a>, where
insert e n ms is the multiset obtained by insertion of n ocurrences of the element e
in ms . For example: insert "a" 2 [("b",3); ("a",5); ("d",1)℄ will result in a
multiset having 7 occurrences of "a".
*)

let rec insert e n ms =
    match ms with
    | ([]) -> []
    | ((x,y)::tail) when e = x -> let a = insert e n tail
                                  (x, n+y)::a
    | ((head)::tail) -> let a = insert e n tail
                        head::a;;

insert "a" 2 [("b",3); ("a",5); ("d",1)]

(*
Declare a function numberOf, where numberOf e ms is the multiplicity (i.e. the numberof 
occurrences) of e in the multiset ms. State the type of the declared function.
*)
let set1 = [("b",3); ("a",5); ("d",1)];;

let numberOf e ms = List.foldBack (fun (value, n) count -> if value = e then count + n else count)  ms 0 ;;
let numberOf e ms = List.fold (fun count (value, n) -> if value = e then count + n else count) 0 ms;;

numberOf "b" set1;;

//State the type of the declared function
// val numberOf : e:'a -> ms:('a * int) list -> int when 'a : equality
// e: 'a
// ms: ('a * int) list

(*
Declare a function delete, where delete e ms is the multiset obtained from ms by
deletion of one occurrence of the element e.
*)

let delete e ms = List.fold (fun arr (value, n) -> if value = e then arr else (value, n)::arr) [] ms;;
delete "b" set1;;
delete "a" set1;;
delete "o" set1;;

let rec union (xs: Multiset<'a>, ys: Multiset<'a>):Multiset<'a> = 
    match (xs, ys) with
    | ([],[]) ->   []
    | ((xHeadVal, xHeadN)::xTail, []) -> let mSet = union (xTail, ys)
                                         (xHeadVal, xHeadN)::mSet
    | ([], (yHeadVal, yHeadN)::yTail) -> let mSet = union (xs, yTail)
                                         (yHeadVal, yHeadN)::mSet
    | ((xHeadVal, xHeadN)::xTail, yTail) -> let mSet = union (xTail, delete xHeadVal yTail)
                                            let numberOfXValInYs = numberOf xHeadVal yTail
                                            if numberOfXValInYs > 0 then 
                                                //insert xHeadVal (xHeadN + numberOfXValInYs) mSet
                                                (xHeadVal, xHeadN + numberOfXValInYs)::mSet
                                            else 
                                                //insert xHeadVal xHeadN mSet
                                                (xHeadVal, xHeadN)::mSet
                                            ;;

let rec union (xs: Multiset<'a>, ys: Multiset<'a>) : Multiset<'a> = 
    match (xs, ys) with
    | (xs, []) -> xs
    | ([], ys) -> ys
    | ((xHeadVal, xHeadN)::xTail, (yHeadVal, yHeadN)::yTail) -> let mSet = union (xTail, yTail)
                                                                insert xHeadVal xHeadN mSet
                                                                insert yHeadVal yHeadN mSet;;

union ([("b",3); ("a",5); ("d",1)], [("a",3); ("b",4); ("c",2)]);;
// [("a", 8); ("b", 7); ("c",2); ("d", 1)]
union ([], []);;

(* 
We shall now represent multisets by maps from elements to multiplicities:
type MultisetMap<'a when 'a : comparison> = Map<'a,int>;;
This representation of a multiset ms has a simpler invariant: the multiplicity n of each
entry (e, n) of ms satisfies n > 0.
6. Give new declarations for inv, insert and union on the basis of the map representation. 
*)

type MultisetMap<'a when 'a : comparison> = Map<'a,int>;;

let m = Map.ofList [(0,"a") ; (2, "c"); (3,"d")];;

let set1 = [("b",3); ("a",5); ("d",1)];;
let set2 = [("b",3); ("a",5); ("d",1); ("b",3);];;

let m1 = Map.ofList set1;;
let m2 = Map.ofList set2;;

let rec invariantMap xs =
   match Map.tryFind with   
   | None -> raise FindArticle
   | Some(aname,aprice) ->
       let tprice = np*aprice
       let (infos,sumbill) = makeBill reg pur  
       ((np,aname,tprice)::infos, tprice+sumbill);;
 

// Insert
let insert3 (e: 'a, n:int, ms:Map<'a, int>): Map<'a, int> = 
    let result = ms.TryFind e
    match result with
        | Some n2 -> let returnMap = ms.Remove e 
                     let newN = (n + n2)
                     returnMap.Add (e, newN) 
        | None -> ms.Add (e, n) ;;
        
insert3 ("e", 2, (Map.ofList [("b",3); ("a",5); ("d",1)]));;
insert3 ("a", 2, (Map.ofList [("b",3); ("a",5); ("d",1)]));;

// Union
let rec union (xs: Multiset<'a>, ys: Multiset<'a>):Multiset<'a> = 
    match (xs, ys) with
    | (xs, []) -> xs
    | ([], ys) -> ys
    | ((xHeadVal, xHeadN)::xTail, (yHeadVal, yHeadN)::yTail) -> let mSet = union (xTail, yTail)
                                                                insert xHeadVal xHeadN mSet
                                                                insert yHeadVal yHeadN mSet;;
type MultisetMap<'a when 'a : comparison> = Map<'a,int>;;
let numberOf e ms = List.foldBack (fun (value, n) count -> if value = e then count + n else count)  ms 0 ;; 
let unionMap (xs: Map<'a, int>, ys: Map<'a, int>) = Map.foldBack (fun (key, value) map -> insert3 (key, value, map)) xs [];;
let unionMap (xs: Map<string, int>, ys: Map<string, int>) = Map.foldBack (fun (key, value) -> map.Add (key, value)) xs [];;


let unionMap (xs: Map<string, int>, ys: Map<string, int>) = 
    let f = insert3()
    Map.foldBack (fun (key, value) -> map.Add (key, value)) xs [];;


let m1 = Map.ofList [("b", 3); ("a", 5); ("d",1)];;
let m2 = Map.ofList([("a",3); ("b",4); ("c",2)]);;

unionMap (m1, m2);;

// [("a", 8); ("b", 7); ("c",2); ("d", 1)]
union ([], []);;

let m = Map.ofList [("a", 1) ; ("b", 2); ("c", 3)];;
m.Add ("d", 4);;


let list1: (string * int) list = [];;
Map.ofList list1;;