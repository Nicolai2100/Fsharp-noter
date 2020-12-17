//Trees

//Problem 4

type Tree<'a,'b> = | A of 'a 
                   | B of 'b
                   | Node of Tree<'a,'b> * Tree<'a,'b>;;

//1
A(true)
B([1;2;3])
Node(A(true),B([1;2;3]))

//2
let rec count = function
    | A(_) -> 1
    | B(_) -> 0
    | Node(a,b) -> count a + count b;;
        
count (Node(A(true),B([1;2;3])))
count (Node(A(true), Node(A(true), B([1;2;3]))))

//3
let rec subst a a' b b' = function
    | A(x) when x = a -> A(a') 
    | A(x) -> A(x) 
    | B(x) when x = b -> B(b')
    | B(x) -> B(x)
    | Node(a1,b1) -> Node((subst a a' b b' a1), (subst a a' b b' b1)) ;;

subst true false [1;2;3] [5;6] (Node(A(true), Node(A(true), B([1;2;3]))))

//4

let rec g = function
    | Node(t1,t2) -> Node(g t2, g t1)
    | leaf -> leaf;;

//Problem 3 

type Tree<'a> = Lf | Br of Tree<'a> * 'a * Tree<'a>;;
let t = Br(Br(Br(Lf,1,Lf),2,Br(Lf,3,Lf)),4,Br(Br(Lf,5,Lf),6,Br(Lf,7,Lf)));;

let rec reflect = function
    | Lf -> Lf
    | Br(x,y,z) -> (Br((reflect z),y, (reflect x))) ;;

reflect (Br(Br((Lf,1,Lf)),4,Br(Br(Lf,5,Lf),6,Br(Lf,7,Lf))));;
reflect t;;
t

//accummulate t
let sumX x xs =
    if ((List.length xs) > 0) then
        x + (List.head xs)
    else
        x;; 

let rec sumHelper sum t = 
    match t with
    | Lf -> sum
    | Br(x,y,z) -> sumHelper ( sumHelper ( (sumX y sum):: sum)  x) z ;;

sumHelper [] (Br(Br(Lf,2,Lf), 4, Br(Lf,6,Lf)))
sumHelper [] (Br(Br(Br(Lf,1,Lf),2,Br(Lf,3,Lf)),4,Br(Br(Lf,5,Lf),6,Br(Lf,7,Lf))));;

let rec reflectHelper (t,ls) =
    match (t,ls) with
    | (Lf,_) -> Lf
    | (Br(x,y,z), head::rest) -> (Br((reflectHelper (x, rest.[0..(rest.Length/2)])), head, (reflectHelper (z, rest.[(rest.Length/2)..])))) 
    ;;
reflectHelper ((Br(Br(Lf,2,Lf), 4, Br(Lf,6,Lf))), [1;2;3])

let accummulate t =
    let vals = sumHelper [] t
    reflectHelper (t, (List.rev vals));;
    
accummulate (Br(Br(Lf,2,Lf), 1, Br(Lf,3,Lf)))
accummulate (Br(Br(Br(Lf,1,Lf),1,Br(Lf,1,Lf)),1,Br(Br(Lf,1,Lf),1,Br(Lf,1,Lf))));;
accummulate (Br(Br(Br(Lf,1,Lf),2,Br(Lf,3,Lf)),4,Br(Br(Lf,5,Lf),6,Br(Lf,7,Lf))));;





// Problem 5 -- Ikke bineær!

type T<'a> = N of 'a * T<'a> list;;
type Path = int list;;

let td = N("g", []);;
let tc = N("c", [N("d",[]); N("e",[td])]);;
let tb = N("b", [N("c",[])]);;
let ta = N("a", [tb; tc; N("f",[])])

//1
let rec toList (N(v,ts)) = v::List.collect toList ts;;

// A solution based on mutual recursive functions:
let rec toList1(N(v,ts)) = v :: toListAux ts
and toListAux = function 
            | []    -> []
            | t::ts -> toList1 t @ toListAux ts

//2

// map has type: ('a -> 'b) -> T<'a> -> T<'b>
 
let rec map f (N(v,ts)) = N(f v, List.map (map f) ts);;

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

//4
let rec get1 path t = 
   match (path,t) with 
   | ([],_)           -> t 
   | (i::is, N(_,ts)) -> get1 is (List.item i ts);;


//5
let rec tryFindPathTo v (N(v',ts)) = if v=v' then Some []
                                 else tryFindInList 0 v ts 
and tryFindInList i v = function 
                    | []                    -> None
                    | N(v',_)::ts when v=v' -> Some [i]
                    | N(_,ts')::ts          -> match tryFindInList 0 v ts' with
                                               | None -> tryFindInList (i+1) v ts
                                               | Some is -> Some(i::is);;

                     