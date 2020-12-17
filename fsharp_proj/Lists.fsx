//Lists

List.sortBy (fun elem -> abs elem) [1; 4; 8; -2; 5]
List.sort [(1,"e"); (4,"a"); (8,"c") ; (-2,"d"); (5,"b")]
List.sort [("e",1); ("a",4); ("c",8) ; ("d",-2); ("b",5)]
List.sortBy (fun (x,y) -> y) [(1,"e"); (4,"a"); (8,"c") ; (-2,"d"); (5,"b")]

let containsNumber number list = List.exists (fun elem -> elem = number) list
containsNumber -2 [1; 4; 8; -2; 5]

type Name = string;;
type Event = string;;
type Point = int;;
type Score = Name * Event * Point;;
type Scoreboard = Score list;;

let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30); ("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;
let sb2 = [("Peter", "May Fishing", 30); ("Joe", "June Fishing", 35);  ("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;

(*Declare a function: inv: Scoreboard -> bool, that checks whether a scoreboard satisfies this constraint*)

let inv sb = 
    let rec invF sbList = 
        match sbList with
            | (_, _, p1)::(_,_,p2)::tail -> if p1 <= p2 && p1 > 0 then
                                                invF tail
                                            else 
                                                false
            | (_, _, _)::tail -> true
            | [] -> true
    invF (List.rev sb);;

inv sb
inv sb2 

(*2. Declare a function insert: Score -> Scoreboard -> Scoreboard, so that insert s sb
gives the scoreboard obtained from sb by insertion of s. The result must satisfy inv*)

let rec insert (insN, insE, insP) sb = 
    match sb with
    | [] -> (insN, insE, insP)::sb
    | (n1, e1, p1)::tail -> if insP >= p1 then      
                                            (insN, insE, insP)::(n1,e1,p1)::tail
                                          else 
                                            (n1, e1, p1):: (insert (insN, insE, insP) tail) ;; 

insert ("Joe", "June Fishing", 37) sb 
insert ("Joe", "June Fishing", 32) sb 
insert ("Joe", "June Fishing", 1) sb 

(*3. Declare a function get: Name*Scoreboard -> (Event * Point) list, where the value
of get(n, sb) is a list of pairs of events and points obtained from n’s scores in sb. For
example get("Joe",sb) must be a list with the two elements: ("June Fishing", 35)
and ("May Fishing", 28).*)

let rec get (name, sb) = 
    match sb with
    | [] -> []
    | (n, e, p)::tail when n = name -> (e,p)::(get (name, tail))
    | _::tail -> get (name, tail)
    ;;

get("Joe", sb)
get("Karla", sb)
get("Karla", sb)

(*Declare a function top: int -> Scoreboard -> Scoreboard option. The value of
top k sb is None if k < 0 or sb does not contain k scores; otherwise the value is Some sb'
where sb0 contains the first k scores of sb.*)

let top k (sb: Scoreboard) = 
    if k < 1 || k > sb.Length then 
        Some sb.[0..k-1]
    else 
        None;;


let rec replace a b xs = 
    match xs with 
    | [] -> []
    | x::tail when x = a -> b::(replace a b tail)
    | x::tail -> x::(replace a b tail);;

replace 1 7 [1;2;3;1]
replace 1 7 [1;2;3;1]

let rec replaceA a b xs accu = 
    match xs with 
    | [] -> List.rev accu
    | x::tail when x = a -> replaceA a b tail (b::accu)
    | x::tail -> replaceA a b tail (x::accu);;

replaceA 1 7 [1;2;3;1] []