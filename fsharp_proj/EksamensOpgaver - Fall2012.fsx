//From the exam fall 2012

//Problem 1
type Name = string;;
type Score = int;;
type Result = Name * Score;;
let ress = [("Hans", 10);("Hansen", 30);("Hansine", 100)];;
let res2 = [("Hans", 10);("Hansen", 30);("Hansine", 101)];;

//legalResults: Result list -> bool 
let legalResults resList = 
    List.fold (fun rs (nam,scor) -> rs && (scor <= 100 && scor >= 0)) true resList;;

legalResults ress
legalResults res2

//maxScore
let maxScore (resList:Result list) = 
    List.fold (fun rs (_,scor) -> if rs > scor then scor else rs) 0 resList;;
maxScore ress
maxScore res2

//best: Result list -> Result
let best ls = 
    List.fold (fun rs (nam,scor) -> if scor > rs then scor else rs) 0 ls;;
best ress;;

// average: Result list -> float
let average ls = 
    let sum = List.fold (fun rs (nam,scor) -> rs+scor) 0 ls
    sum / (List.length ls);;
average ress;;

//delete: Result -> Result list -> Result list.

let delete (name,score) rs =
    List.foldBack (fun (nam,scor) listen -> if nam = name then listen else (nam,scor)::listen) rs [];;

delete ("Hans", 10) ress;;

let ins rs (name,score) = 
    if (List.length rs > 0) then
        List.foldBack (fun (nam,scor) listen -> if scor > score then (name,score)::(nam,scor)::listen else (nam,scor)::listen) rs []
    else 
        [(name,score)];;

let ins rs (name,score) = 
    let tempList = List.foldBack (fun (nam,scor) listen -> if scor > score 
                                                           then (name,score)::(nam,scor)::listen 
                                                           else (nam,scor)::listen) rs []
    if List.length tempList < 1
    then [(name,score)]
    else tempList;;

ins [] ("hej",10);;

//bestN: Result list -> int -> Result list
let bestN rs N = 
    let sortedRs = List.rev (List.sort rs)
    List.fold (fun listen (n,s)  -> if (List.length listen) < N  
                                    then (n,s)::listen 
                                    else listen)  [] sortedRs ;;


bestN ress3 2
List


//Problem 2

type Typ = | Integer
           | Boolean
           | Ft of Typ list * Typ;;
type Decl = string * Typ;;


//distinctVars: Decl list -> bool,

Ft([Integer;Integer],Integer)
Ft([Integer;Integer],Boolean)
Decl("1",Ft([Integer;Integer],Boolean))
Decl("2",Ft([Integer;Integer],Boolean))
Decl("2",Ft([Integer;Integer],Boolean))

let decs1 = [Decl("1",Ft([Integer;Integer],Boolean)) ; Decl("2",Ft([Integer;Integer],Boolean)) ]
let decs2 = [Decl("1",Ft([Integer;Integer],Boolean)) ; Decl("2",Ft([Integer;Integer],Boolean)) ; Decl("2",Ft([Integer;Integer],Boolean)) ]
let decs3 = [Decl("3",Ft([Integer;Integer],Boolean)) ];;
let rec helperf akku ls = 
    match ls with
    | [] -> true
    | (x,_)::rest -> if (List.contains x akku) then false else true && (helperf (x::akku) rest) ;;

let distinctVars decl = helperf [] decl ;; 

distinctVars decs1  
distinctVars decs2 

type SymbolTable = Map<string,Typ>;;

//toSymbolTable: Decl list -> SymbolTable
let toSymbolTable decls = Map.ofList decls;;
toSymbolTable decs1;;

//extendST: SymbolTable -> Decl list -> SymbolTable

let insertMap valIns num ms  = 
        Map.add valIns num ms;;

insertMap "7" (Ft([Integer;Integer],Boolean)) (Map.ofList decs1);;  

let extendST ms1 ls =
    Map.foldBack (fun a b accu -> insertMap a b accu ) ms1 (Map.ofList ls);;

extendST (Map.ofList decs1) decs3

type Exp = | V of string
           | A of string * Exp list;;

//symbolsDefined: SymbolTable -> Exp -> bool,
let symbolsDefined syms exs = 
    match


decs1
symbolsDefined (Map.ofList decs1) (A(">",[V "x";V "y"]))
 






//Problem 3 