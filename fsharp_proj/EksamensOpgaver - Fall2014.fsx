//From the exam fall 2014

//Problem 1
type Rel<'a,'b> = ('a * 'b list) list
let rel: Rel<int,string> = [(1, ["a"; "b"; "c"]); (4,["b"; "e"])];;

//apply: ’a -> Rel<’a,’b> -> ’b list
let rec apply a = function
    | ([]) -> []
    | (x,ys)::rest when x = a -> ys
    | (x,ys)::rest -> apply a rest ;;

apply 1 rel
apply 4 rel
apply 0 rel

//inRelation x y rel
let rec inRelation x y = function
    | ([]) -> false
    | (a,bs)::rest when x = a -> (List.contains y bs)
    | (_,_)::rest -> inRelation x y rest ;;
inRelation 4 "e" rel; 
inRelation 1 "e" rel; 

//insert x y rel 
let rec insert x y rel = 
   let mapp = Map.ofList rel
   let mapp2 = if Map.containsKey x mapp then Map.add x (y::(Map.find x mapp)) mapp else (Map.add x [y] mapp)
   Map.toList mapp2;;
insert 4 "e" rel; 
insert 1 "e" rel;

//toRel:(’a*’b) list -> Rel<’a,’b>
let rec toRel akku = function
    | [] -> akku
    | (x,y)::rest -> toRel ((insert x y akku)) rest;;
toRel [] [(2,"c");(1,"a");(2,"b")] 

//Problem 2
//multTable: int -> seq<int>

let multTable n = 
 (Seq.take 10 ( seq {
               for i in (Seq.initInfinite (fun i -> i+1)) do
               yield (i * n) } ));;
multTable 3

//2. tableOf: int -> int -> (int -> int -> ’a) -> seq<int*int*’a>
let mySeq = seq { for i in 1 .. 10 -> 3*i }
let takeSeq = Seq.take 5 mySeq
Seq.iter (fun i -> printf "%d " i) takeSeq

let tableOf m n f = 
    (Seq.take (m+n-1)
              (seq {
                    for i in (Seq.initInfinite(fun i -> i + 1)) do
                        if i < n then yield (1, i, (f 1 i))
                        else 
                        yield ((i-n+1), n, (f (i-n+1) n))
                })) ;;
tableOf 3 4 (+);;
Seq.take 10 ( Seq.rev (tableOf 3 4 (+)))

//3. Give a declaration for the infinite sequence of strings "a", "aa", "aaa", "aaaa", . . ..
let rec rs n letter = if n > 1 then rs (n-1) (letter + "a") else letter ;; 
rs 3 "a"

let infStrings = 
    (seq {
        for i in (Seq.initInfinite(fun i -> i + 1)) do
            yield (rs i "a")
    }) ;;
infStrings


//Problem 4
type Outcome = | S | F ;;// S: for success and F: for failure
type Sample = Outcome list;;
type ProbTree = | Branch of string * float * ProbTree * ProbTree
                | Leaf of string;;

let exp = Branch(">2",0.67, Branch(">3",0.5, Leaf "A", Leaf "B"), Branch(">3",0.5, Leaf "C", Leaf "D"))
let exp2 = Branch(">2",0.67, Branch(">3",0.5, Leaf "A", Leaf "B"), Branch(">3",1.5, Leaf "C", Leaf "D"))
let ex = Branch(">3",0.5, Leaf "A", Leaf "B");;

//probOK: ProbTree

let rec probOK = function
    | Leaf _ -> true
    | Branch(s,p,tl,tr) -> ( p >= 0.0 && p <= 1.0 ) && probOK tl && probOK tr;;
probOK exp
probOK exp2

//isSample(os, t) 
let rec isSample (os,t) = 
    match (t, os) with
    | (Leaf _, []) -> true
    | (Leaf _, _) -> false
    | (Branch(s,p,tl,tr), h::tail) -> match h with  
                                      | F -> isSample (tail,tl)
                                      | S -> isSample (tail,tr)
    | (Branch(_,_,_,_), []) -> false ;;

isSample ( [F; F] , exp )
isSample ( [F; S] , exp )
isSample ( [S; F] , exp )
isSample ( [S; S] , exp )
isSample ( [F] , exp )
isSample ( [F; S; F; F ; F] , exp )

//descriptionOf os t 
let rec dofHelper os t (akku,p,n) = 
    match (os, t) with
    | ([], Leaf x) -> (List.rev akku,p,x)
    | (h :: tail, Branch(s, p', tl, tr)) -> 
        match h with
        | S -> dofHelper tail tl  ((S,s)::akku,(p*p'),n) 
        | F -> dofHelper tail tr   ((F,s)::akku,(( (1.0 - p')*p)),n)   ;;
dofHelper [S; S] exp ([],1.0,"")
dofHelper [S; F] exp ([],1.0,"")
dofHelper [F; S] exp ([],1.0,"")
dofHelper [F; F] exp ([],1.0,"")

let rec descriptionOf os t = dofHelper os t ([], 1.0, "");;
descriptionOf [S; S] exp 
descriptionOf [S; F] exp 
descriptionOf [F; S] exp 
descriptionOf [F; F] exp 

//allDescriptions: PropTree -> Set<Description>
let rec countBrs t = 
    match t with
    | (Leaf _) -> 0
    | (Branch(s,p,tl,tr)) -> 1 + countBrs tl + countBrs tr ;;
countBrs exp

let rec getPaths t akku = 
    match t with
    | (Leaf _) -> (List.rev akku)
    | (Branch(s,p,tl,tr)) -> (getPaths tl (F::akku)) @ (getPaths tr (S::akku)) ;;
getPaths exp []

let rec cutPaths k osls : (Outcome list list) = 
    match osls with
    |[] -> []
    |ls when ls.Length > k  -> (fst (List.splitAt k ls)) :: cutPaths k (snd (List.splitAt k ls))
    |ls -> [ls] ;;
cutPaths 2 (getPaths exp []) 

let allDescriptions t = 
    let brs = countBrs t
    let paths = getPaths t []
    let pathLists = cutPaths (brs-1) paths
    let rec desMaker osl tree = 
        match osl with
        | [] -> []
        | head::tail -> (descriptionOf head t)::desMaker tail tree
    desMaker pathLists t;;

allDescriptions exp