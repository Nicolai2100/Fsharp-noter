//Fall 2013
//Problem 1 (30%)

type Multiset<'a when 'a : equality> = ('a * int) list;;

let ms1 = [("b",3); ("a",5); ("c",1)];;
let ms2 = [("b",3); ("a",5); ("c",1); ("c",3)];;

let s1 = Set.empty.Add "b";;
s1;;
s1.Contains "b"

//inv: Multiset<'a> -> bool

let helperf ms =
    List.fold (fun (ls,bol) (x,y) -> if Set.contains x ls 
                                       then (ls,false) 
                                     else 
                                        (Set.add x ls,true && bol ) ) (Set.empty,true) ms  ;;

let inv ms = 
    let (a,b) = helperf ms
    b;;
inv ms1;;  
inv ms2;;
   
//insert: 'a -> int -> Multiset<'a> -> Multiset<'a>
let insert valIns num ms  = 
    let (set,_) = helperf ms
    if (Set.contains valIns set) then
        List.foldBack (fun (x,y) xs -> if x = valIns then (x,(y+num))::xs else (x,y)::xs) ms [] 
    else 
        (valIns,num)::ms    
    ;;
insert "e" 2 ms1;;  
insert "a" 2 ms1;;  

//numberOf e ms 
let numberOf e ms = 
    List.fold (fun count (x,y) -> if x = e then y else count)  0 ms    ;;
numberOf "a" ms1;;  
numberOf "b" ms1;;  
numberOf "c" ms1;;  
numberOf "e" ms1;;  

//delete e ms
let delete e ms  = 
    let (set,_) = helperf ms
    if (Set.contains e set) then
        List.foldBack (fun (x,y) xs -> if x = e then xs else (x,y)::xs) ms [] 
    else 
        ms    
    ;;
delete "e" ms1;;  
delete "a" ms1;;  

// union: Multiset<'a> * Multiset<'a> -> Multiset<'a>,
let union (ms1:Multiset<'a>) (ms2:Multiset<'a>) = 
    List.foldBack (fun (x,y) xs -> (insert x y xs)) ms1 ms2 ;;

union [("b",3)]  [("a",1)] ;;
union [("b",3); ("a",5); ("d",1)]  [] ;;
union [("b",3); ("a",5); ("d",1)] [("b",3); ("a",5); ("d",1)];;
union [] [("b",3); ("a",5); ("d",1)];;

type MultisetMap<'a when 'a : comparison> = Map<'a,int>;;

//6. Give new declarations for inv, insert and union on the basis of the map representation

// inv vil altid være true, grundet Maps datatruktur regler
let msMap1 = Map.ofList [("b",3); ("a",5); ("c",1)];;
let msMap2 = Map.ofList [("b",3); ("a",5); ("c",1) ; ("c",1)];;
msMap1;;
msMap2;;
msMap1.Add ("a", 2)

let insertMap valIns num ms  = 
    if (Map.containsKey valIns ms) then
        let y = Map.find valIns ms
        Map.add valIns (y+num) ms 
    else 
        Map.add valIns num ms;;

insertMap "e" 2 msMap1;;  
insertMap "a" 2 msMap1;;  

inv ms1;;  
inv ms2;;

let unionMap ms1 ms2 = 
    Map.foldBack (fun a b accu -> insertMap a b accu ) ms1 ms2;;
    

unionMap (Map.ofList [("b",3)])  (Map.ofList [("a",1)]) ;;
unionMap (Map.ofList [("b",3)])  (Map.ofList []) ;;
unionMap (Map.ofList [])  (Map.ofList [("a",1)]) ;;
unionMap (Map.ofList [("b",3); ("a",5); ("d",1)])  (Map.ofList [("b",3); ("a",5); ("d",1)]) ;;


//Problem 3 (40%)

type Title = string;;
type Section = Title * Elem list
            and Elem = Par of string | Sub of Section;;

type Chapter = Title * Section list;;
type Book = Chapter list;;

let sec11 = ("Background", [Par "bla"; Sub(("Why programming", [Par "Bla."]))]);;
let sec12 = ("An example", [Par "bla"; Sub(("Special features", [Par "Bla."]))]);;
let sec21 = ("Fundamental concepts", [Par "bla"; Sub(("Mathematical background", [Par "Bla."]))]);;
let sec22 = ("Operational semantics", [Sub(("Basics", [Par "Bla."])); Sub(("Applications", [Par "Bla."]))]);;
let sec23 = ("Further reading", [Par "bla"]);;
let sec31 = ("Overview", [Par "bla"]);;
let sec32 = ("A simple example", [Par "bla"]);;
let sec33 = ("An advanced example", [Par "bla"]);;
let sec41 = ("Status", [Par "bla"]);;
let sec42 = ("What's next?", [Par "bla"]);;
let h1 = ("Introduction", [sec11;sec12]);;
let h2 = ("Basic Issues", [sec21; sec22;sec23]);;
let h3 = ("Advanced Issues", [sec31;sec32;sec33]);;
let h4 = ("Conclusion", [sec41;sec42]);;
let book1 = [ h1; h2; h3; h4];;
book1;;

//1. Declare a function maxL to find the largest integer occurring in a list with non-negative
//integers. The function must satisfy maxL [] = 0
let maxL xs = List.fold (fun x lg -> if (x > lg) then x else lg) 0 xs;;
let listen = [1;2;3;4;2;18;1];;
maxL [];;
maxL listen;;

//2
let rec overview = function 
    | [] -> []
    | chp::rest -> (fst chp)::overview rest
;;
overview book1;;

//3
//1
 let testElem1 = Sub(("Why programming", [Par "Bla."]));;
//2
 let testElem2 = Sub(("Why programming", [(Sub(("t2", [Par "Bla."])))]));;
//3
 let testElem3 = Sub(("Why programming", [(Sub(("t2", [(Sub(("t2", [Par "Bla." ; Par "Bla. 2"])))])))]));;

 //depthElem: Elem -> int
let rec depthElem elem = 
    match elem with
    | Par _ -> 0
    | Sub (x,y) -> 1 + match y with
                       | Par _ :: rest -> 0
                       | Sub (xx,yy) :: rest -> depthElem (Sub (xx,yy))
                       |  [] -> 0 ;;
depthElem testElem1;;
depthElem testElem2;;
depthElem testElem3;;

//depthSection: Section -> int
let depthSection (_,ls) = 2 + List.fold (fun depth elem -> if ((depthElem elem) > depth) then depthElem elem else depth  ) 0 ls
;;
depthSection ("",[]);;
depthSection sec31;;
depthSection ("",[testElem1]);;
depthSection ("",[testElem2]);;
depthSection ("",[testElem3]);;

//depthChapter: Chapter -> int
let depthChapter (_,ls) = 1 + List.fold (fun depth elem -> if ((depthSection elem) > depth) then depthSection elem else depth  ) 0 ls
;;
depthChapter ("",[]);;
depthChapter ("",[sec31]);;
depthChapter ("",[("",[testElem3])]);;
depthChapter h1

//depthBook: Book -> int
let depthBook ls = 0 + List.fold (fun depth elem -> if ((depthChapter elem) > depth) then depthChapter elem else depth  ) 0 ls
;;
depthBook book1;;

type Numbering = int list;;
type Entry = Numbering * Title;;
type Toc = Entry list;;



let rec tocB2 (elem,ls) (akku)  = List.fold ( fun listen (e',ls') -> (akku+1,e' @  )::listen ) [] ls
and elemRun akku elem = match elem with
    | Par _ -> []
    | Sub(title, []) -> [(akku+1,title)]
    | Sub(title, ls) -> (akku+1,title) :: tocB2 (akku+1) ("",ls) 
;;
tocB 1 sec11 ;
let sec11 = ("Background", [Par "bla"; Sub(("Why programming", [Par "Bla."]))]);;


tocB 0 book1

let rec tocB1 (akku) = function
    | [] -> []
    | (title, ls)::tail -> (akku+1,title) :: tocB1 (akku+1) tail
    ;;
tocB 0 book1