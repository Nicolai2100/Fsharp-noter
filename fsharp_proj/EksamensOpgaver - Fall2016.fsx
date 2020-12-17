
//Problem 1

type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list
let sb1 = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30); ("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;
let sb2 = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 100); ("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;
let sb3 = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", -30); ("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;

//inv: Scoreboard -> bool,
let rec inv scores = 
    if not (List.fold (fun bol (_,_,score) -> (score >= 0) && bol)  true scores) 
    then false
    else
    match scores with
    | [] -> true
    | (_,_,score1)::(name2,event2,score2)::rest -> if score1 < score2 then false else inv ((name2,event2,score2)::rest)
    | (_,_,_)::_ -> true ;;
inv sb1
inv sb2
inv sb3

//insert: Score -> Scoreboard -> Scoreboard
let rec insert (name,event,score) sb = 
    if not (inv [(name,event,score)]) 
    then failwith("Error in input")
    else
    match sb with
    | [] -> [(name,event,score)]
    | (n,e,s)::rest -> if s <= score 
                       then (name,event,score)::(n,e,s):: rest 
                       else (n,e,s):: insert (name,event,score) rest ;;

insert ("p1", "fishing", 27) sb1
insert ("p1", "fishing", 29) sb1
insert ("p1", "fishing", 36) sb1

// get: Name*Scoreboard -> (Event*Point) list
let rec get (name,sb) = 
    match sb with
    | [] -> []
    | (n,e,s)::rest when n = name -> (e,s) :: get (name,rest)
    | _::rest -> get (name,rest);;
get ("Joe", sb1)

//top: int -> Scoreboard -> Scoreboard option

let top k sb = 
    if k > List.length sb 
    then None 
    else Some (List.rev (List.fold (fun ls (n,e,s) -> 
                                            if (List.length ls) < k 
                                            then (n,e,s)::ls 
                                            else ls ) [] sb))  ;;

top 1 sb1
top 2 sb1
top 3 sb1
//Problem 2

let rec replace a b xs = List.foldBack (fun head ls -> if head = a then b::ls else head::ls)  xs [];;
replace 2 7 [1; 2; 3; 2; 4]

//Tail recursion
let rec replaceAc a b akku = function
    | [] -> List.rev akku
    | head::tail when head = a -> replaceAc a b (b::akku) tail
    | head::tail -> replaceAc a b (head::akku) tail;;

replaceAc 2 7 [] [1; 2; 3; 2; 4] ;;

//Problem 4
type Tree<'a,'b> = | A of 'a 
                   | B of 'b
                   | Node of Tree<'a,'b> * Tree<'a,'b>;;

A 2
B "b"
Node(A 2, B "b")
let rec counts = function   
    | A _ -> 1
    | B _ -> 0
    | Node(a,b) -> counts a + counts b;;

counts (Node(Node(Node(A 2, B "b"), A 3), Node(A 2, B "b")))

//subst:’a -> ’a -> ’b -> ’b -> Tree<’a,’b> -> Tree<’a,’b> when ’a : equality and ’b : equality
let rec subst a a' b b' = function
    | A x -> if x = a then A a' else A a
    | B x -> if x = b then B b' else B b
    | Node(x,y) -> Node(subst a a' b b' x, subst a a' b b' y);;

subst 2 4 "b" "B" (Node(Node(Node(A 2, B "b"), A 3), Node(A 2, B "b")))

let rec f = function
    | A a -> ([a],[])
    | B b -> ([], [b])
    | Node(t1,t2) -> let (xs1,ys1) = f t1
                     let (xs2,ys2) = f t2
                     (xs1@xs2, ys1@ys2);;
f (Node(Node(Node(A 2, B "b"), A 3), Node(A 2, B "a")))
f (A 2)
f (B "2")

let rec fK t k = match t with
    | A a         -> k([a],[])
    | B b         -> k([], [b])
    | Node(t1,t2) -> fK t1 (fun (xs1,ys1) -> fK t2 (fun (xs2,ys2) -> k(xs1@xs2,ys1@ys2)));;
fK (Node(Node(Node(A 2, B "b"), A 3), Node(A 2, B "a"))) id

let td = N("g", []);;
let tc = N("c", [N("d",[]); N("e",[td])]);;
let tb = N("b", [N("c",[])]);;
let ta = N("a", [tb; tc; N("f",[])]);;

//1. Declare a function toList t
let rec toList (N(x,y)) = x :: List.collect toList y ;;
toList td // g
toList tc // c, d, e, g

// map has type: ('a -> 'b) -> T<'a> -> T<'b>
let rec map f (N(v,ts)) = N(f v, List.map (map f) ts);;
map id ta

// A solution based on mutual recursion
let rec map1 f (N(v,ts)) = N(f v, mapAux f ts)
and mapAux f = function 
               | [] -> []
               | t::ts -> map1 f t :: mapAux f ts;;

//3
let rec isPath path t = 
   match (path, t) with
   | ([], _)                                               -> true
   | (i::path', N(v,ts)) when 0 <= i && i < List.length ts -> isPath path' (List.item i ts)  
   | _                                                     -> false;;
isPath [0;0] ta 

//4
let rec get1 path t = 
   match (path,t) with 
   | ([],_)           -> t 
   | (i::is, N(_,ts)) -> get1 is (List.item i ts);;
get1 [1;0] ta
get1 [] tb

//5
let rec tryFindPathTo v (N(v',ts)) = if v=v' then Some []
                                     else tryFindInList 0 v ts 
and tryFindInList i v = function 
                        | []                    -> None
                        | N(v',_)::ts when v=v' -> Some [i]
                        | N(_,ts')::ts          -> match tryFindInList 0 v ts' with
                                                   | None -> tryFindInList (i+1) v ts
                                                   | Some is -> Some(i::is);;
tryFindPathTo "a" ta
tryFindPathTo "b" ta
tryFindPathTo "c" ta
tryFindPathTo "d" ta
tryFindPathTo "e" ta
tryFindPathTo "f" ta
tryFindPathTo "g" ta
tryFindPathTo "h" ta
