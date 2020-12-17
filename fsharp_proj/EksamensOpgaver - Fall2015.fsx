//From the exam fall 2015

//Problem 1
type Appliance = string
type Usage = Appliance * int
let ad1 = ("washing machine", 2)
let ad2 = ("coffee machine", 1)
let ad3 = ("dishwasher", 2)
let ad4 = ("test2", 20)
let ats = [ad1; ad2; ad3; ad1; ad2]
let ats2 = [ad1; ad2; ad3; ad1; ad4 ; ad4]
let ats3 = [ad1; ad2; ad3; ad1; ("test",-1)]

//inv: Usage list -> bool,
let inv usls =  List.fold (fun b (n,s)  -> b && s > 0 )  true usls ;;
inv ats
inv ats3

//durationOf: Appliance -> Usage list -> int
let durationOf ap usls =  List.fold (fun count (n,s) -> if n = ap then s + count else count)  0 usls ;;
durationOf "washing machine" ats
durationOf "coffee machine" ats
durationOf "dishwasher" ats
durationOf "test" ats3

let wellFormed usls = List.fold (fun b (n,s)  ->  ((inv usls) && (durationOf n usls) < 24))    true usls ;;
wellFormed ats
wellFormed ats2

//delete(a, ats)
let delete (a,ts) = List.fold (fun ls (n,s) -> if a = n then ls else (n,s)::ls) [] (List.rev ts) ;;
delete ("washing machine", ats) 
delete ("coffee machine", ats) 
delete ("test2", ats2)

type Price = int
type Tariff = Map<Appliance, Price>
let trf1 = Map.ofList [("coffee machine",34) ; ("washing machine",775) ; ("dishwasher",329) ];;
let trf2 = Map.ofList [("coffee machine",34) ; ("washing machine",775) ; ("nøms",329) ];;
//isDefined ats trf
let isDefined ats trf = 
    let mapls = Map.ofList ats
    let isdef = Map.foldBack (fun a b bol -> bol && (Map.containsKey a mapls) ) trf true
    isdef
    ;;
isDefined ats trf1
isDefined ats2 trf1
isDefined ats trf2

//priceOf: Usage list -> Tariff -> Price
let rec priceOf trf = function
    | [] -> 0
    | (n,_)::rest when Map.containsKey n trf -> (Map.find n trf) + priceOf trf rest 
    | _ -> failwith("Appliance is not defined in the tariff!");;
priceOf  trf1 ats
priceOf  trf1 ats2
priceOf  trf2 ats

//Problem 2

//Problem 3 
type Name = string;;
type Flow = int;; // can be assumed positive in below questions
type River = R of Name * Flow * Tributaries
and Tributaries = River list;;

//1. Declare F# values riv and riv3 corresponding to the rivers “R” and “R3”.

//• A river named “R” has flow 10m3/s from its source and it has three tributaries named
//“R1”, “R2” and “R3”, respectively.
let tribEmpty: Tributaries = [];;
let r1 = R("R1", 5, [])
let r3 = R("R3", 8, [])
let r4 = R("R4", 2, [])
let r2 = R("R2", 15, [r4])
let r = R("R", 10, [r1;r2;r3])


//2. Declare a function contains : Name → River → bool. 
let rec contains n = function
   | R(n',_,_) when n=n' -> true
   | R(_,_,ts)           -> List.exists (contains n) ts;;

contains "R" r
contains "R1" r
contains "R2" r
contains "R3" r
contains "R4" r
contains "R4" r2
contains "R7" r

// alternative solution
// contains1: Name -> River -> bool
// containsTs: Name -> Tributaries -> bool
let rec contains1 n = 
    function
    | R(n',_,_) when n=n' -> true
    | R(_,_,ts)           -> containsTs n ts
and containsTs n = 
    function
    | []     -> false
    | r::ts  -> contains1 n r || containsTs n ts;;

// 3                          
// allNames: River -> Name list
let rec allNames (R(n,fl,ts)) = n::List.collect allNames ts     
allNames r
allNames r1
allNames r2
// 4
// totalFlow: River -> int
let rec totalFlow (R(_,f,ts)) = List.fold (fun s r -> s + totalFlow r) f ts;;

// alternative solution
// totalFlow1: River -> int
// totalFlowTs: Tributaries -> int
let rec totalFlow1 (R(_,f,ts)) = f + totalFlowTs ts
and totalFlowTs = function
                  | []    -> 0
                  | r::ts -> totalFlow1 r + totalFlowTs ts;;

// 5
// maxFlow: Name*Flow -> Name*Flow -> Name*Flow
let maxFlow (n,fl) (n',fl') = if fl>fl' then (n,fl) else (n',fl') 
                                    
// mainSource: River -> Name*Flow
// mainSourceTs: (Name*Flow) -> Tributaries -> Name*Flow
let rec mainSource (R(n,fl,ts)) = mainSourceTs (n,fl) ts
and mainSourceTs res = 
   function
   | []    -> res
   | r::ts -> mainSourceTs (maxFlow res (mainSource r)) ts       

// 6
// tryInsertTributary: Name -> River -> River -> River option
// tryInsertTributaryInList: Name -> River -> Tributaties -> Tributaries option
let rec tryInsertTributary n t = 
   function
   | R(n',fl,ts) when n=n' -> Some(R(n',fl,t::ts))
   | R(n',fl,ts)           -> match tryInsertTributaryInList n t ts with
                              | None     -> None
                              | Some ts' -> Some(R(n',fl,ts'))
and tryInsertTributaryInList n t =
   function 
   | []    -> None
   | r::ts -> match tryInsertTributary n t r with 
              | None    -> match tryInsertTributaryInList n t ts with
                           | None     -> None
                           | Some ts' -> Some(r::ts')
              | Some r' -> Some(r'::ts);;
              