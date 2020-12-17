



(* Problem 1 *)

//1
type Person = string
type Contacts = Person list
type Register = (Person * Contacts) list
let reg1 = [("p1", ["p2"; "p3"]); 
                
                ("p2", ["p1"; "p4"]);

            ("p3", ["p1"; "p4"; "p7"]); ("p4", ["p2"; "p3"; "p5"]);
            ("p5", ["p2"; "p4"; "p6"; "p7"]);
            ("p6", ["p5"; "p7"]); ("p7", ["p3"; "p5"; "p6"])];;
let reg2 = [("p1", ["p2"; "p3"]); 
("p2", ["p1"; "p4"]);

            ("p2", ["p1"; "p4"; "p7"]); ("p4", ["p2"; "p3"; "p5"]);
            ("p5", ["p2"; "p4"; "p6"; "p7"]);
            ("p1", ["p5"; "p7"]); ("p7", ["p3"; "p5"; "p6"])];;

let rec invariant1H p (ps:Contacts) = List.fold (fun holds p' -> holds && p <> p' ) true ps

let rec invariant1 ((p:Person), (ps:Contacts)) = 
    match ps with
    | [] -> true
    | p'::tail -> (invariant1H p' tail) && (invariant1 (p,tail));; 

invariant1 ("p3", []);;
invariant1 ("p3", ["p1"; "p2"]);;
invariant1 ("p3", ["p1"; "p1"]);;

//2
let rec invariant2 reg =
    match reg with
    | [] -> true
    | (p,ps)::tail when ((List.length ps) > 1) && (not (Map.containsKey p (Map.ofList tail))) -> true
    | _::_ -> false;;
invariant2 reg1
invariant2 reg2

//3
let rec insert p ps = if List.contains p ps then ps else p::ps

let rec extractPs p reg = 
    match reg with
    | [] -> []
    | (p',ps')::_ when p' = p -> ps'
    | (_,_)::tail -> extractPs p tail;;
extractPs "p1" reg1

//4 addContact p1 p2 reg
let rec addContact p1 p2 reg =
    match reg with
        | [] -> []
        | (p',ps')::tail when p' = p1 -> (p', insert p2 ps'):: addContact p1 p2 tail
        | (p',ps')::tail -> (p',ps')::addContact p1 p2 tail;;
addContact "p1" "p7" reg1

//5
let rec combine ps1 ps2 = List.foldBack insert ps1 ps2;;

let rec allContacts p (reg:Register) =
    let ps = List.fold (fun ls (p',ps') -> if p' = p then ps' else ls ) [] reg
    List.fold (fun ls p' -> combine ps (extractPs p' reg)) [] ps ;;
allContacts "p1" reg1

(* Problem 2 *)
(* 1

(int -> ’a -> ’b) -> ’a list -> int -> ’b list er den med generelle type for h, da

- f er (int -> 'a -> 'b), da den: 
    1. tager j som første argument, og j er en int 
    2. tager elementer fra xs som andet argument, og xs listen er af typen 'a list
- xs er en 'a list, da:
    1. Det er en liste grundet man kan bruge cons på den
    2. Metoden f bruger elementer fra xs som 2. argument, og f er af typen int->'a -> 'b
- j er en int, da (j+1) indgår i metoden
Værdien af h er en 'b liste da denne bliver bygget op via cons, med værdierne fra f

(int -> ’a -> ’b) -> ’a list -> ’b list er den med generelle type for mapi, grundet oven stående
samt 'b list er typen af h, og er netop mapi's værdi
*)

(* 2

mapi (fun i x -> (i,x)) ["a";"b";"c"] = h (fun i x -> (i,x)) ["a";"b";"c"] 0
~> (0,"a")::(f ["b";"c"] (0+1)) 
~> (1,"b")::(0,"a")::(f ["c"] (1+1)) 
~> (2,"c")::(1,"b")::(0,"a")::(f ["c"] (2+1)) 
~> (2,"c")::(1,"b")::(0,"a")::([]) => [(2,"c");(1,"b");(0,"a")] *)

// 3
//Typen for ["a";"b";"c"] er string list, og listen også kan indeholde længere strenge

//Typen for (fun i x -> (i,x)) er en i:'a -> x:'b -> 'a * 'b

//Typen for "mapi (fun i x -> (i,x)) ["a";"b";"c"] er (int*string) list
//grundet den integer mapi giver som argument til metoden h, i mapis deklaration 

// 4 - a tail-recursive variant of h that is based on an accumulating parameter.
let rec hTail f xs j akku = match xs with
    | [] -> List.rev akku
    | x::rest -> hTail f rest (j+1) ( f j x ::akku) ;;

hTail (fun i x -> (i,x)) ["a";"b";"c"] 0 [];;

(* Problem 3 *)
type Name = string;;
type Part = | S of Name // Simple part
            | C of Name * Part list;; // Composite part
//1
let rec checkparts = function
    | S _ -> true
    | C(n,[]) -> false
    | C(n,ps) -> List.fold (fun comparts n -> match n with 
                                              | S _ -> comparts
                                              | C(n,ps) -> comparts && (checkparts (C(n,ps)) )) true ps ;;
checkparts (C (("3"), []) )
checkparts (C (("3"), [(S "2")]) )
checkparts (C (("3"), [(C (("3"), [(S "2")]) ) ; (C (("3"), [(S "2")]) )] ) )

//2

let cE1 = (C (("3"), [(C (("3"), [(S "2")]) ) ; (C (("3"), [(S "2")]) )] ) )
let cE2 = (C (("3"), [(C (("3"), [(S "2")]) ) ; (C (("3"), [(C (("3"), [(S "2")]) )]) )] ) )

let rec depthElem1 elem = 
    match elem with
    | S _ -> 0
    | C (n,ps) -> 1 + match ps with
                      | S _ :: rest -> 0
                      | C (n',ps') :: rest -> depthElem1 (C(n', ps'))
                      |  [] -> 0 ;;

depthElem1 (C (("3"), [(C (("3"), [(S "2")]) ) ; (C (("3"), [(S "2")]) )] ) )

let deptAllElements = function
    | S _ -> 0
    | C (n,ps) -> 1 + List.fold (fun max e -> if max > (depthElem1 e) then max else (depthElem1 e)  ) 0 ps;;
deptAllElements cE1
deptAllElements cE2

//3
//The type declaration for Part comprises: Part, S, C, Name, * and list. Give a brief
//description of each of these 6 components of the declaration.
(*
Name er en vilkårlig streng, der identificeret objektet
"Part" er navnet på den type, der består af to forskellige objekter nemlig
"*" betyder der er tale om en tuple
S er en contructor for en subtype af Part der matcher et pattern a la S "name"
C er en contructor for en subtype af Part der matcher et pattern a la C "name" [(S "name") ...]
Part list i C betyder at de elementer der lægger i et objekt af typen C, skal være af typen
Part.
*)

//4
let p = (C (("P"), [(S "3"); (C (("Comp 1"), [(S "1");(S "2")]) ) ; (C (("Comp 2"), [(C (("Comp 3"), [(S "4");(S "5")]) )]) )] ) ) ;;

//5 Part -> Set<Name>
let rec allSimplePartsName = function
    | S n -> [n] 
    | C(n,ps) -> List.collect allSimplePartsName ps;;
allSimplePartsName p;;

let simplePartNamesSet p = Set.ofList (allSimplePartsName p );;
simplePartNamesSet p;;

//6 Part -> OccurrenceCount

type OccurrenceCount = Map<Name,int> ;;

//Koden compiler ikke, grundte fejl med en type
//let rec oc (map:Map<Name,int>) ps = 
//    match ps with
//    | [] -> map
//    | head::tail -> match head with
//                    | S n when map.ContainsKey n -> oc (map.Add (n, (1 + (Map.find n map)))) tail
//                    | S n -> oc (map.Add (n,1)) tail
//                    | C(n,ps) when map.ContainsKey n -> List.fold (fun map' (e,es) -> oc map' es ) map ps
//                    | C(n,ps) -> oc (map.Add (n,1)) tail ;;
//oc Map.empty ([S "3"; C ("Comp 1", [S "1"; S "2"]);C ("Comp 2", [C ("Comp 3", [S "3"; S "2"])])])

(* Problem 4 *)

//1
let rec g i =
    if i=0 then 0
    else if i=1 then 1
    else g ((i-1) + (g (i-2))) ;;

g 5;;
g 4;;

//2
let pos = Seq.initInfinite (fun i -> i) ;;
let seq1 = seq {
                  for i in pos do
                  if i % 2 = 0 then  yield (2*i+1) else yield -1*(2*i+1) }
seq1

//3
let seq2 = seq {
                  for i in seq1 do
                  yield(1.0/ float i) }
seq2

//4
let pos2 = Seq.initInfinite (fun i -> i+1) ;;
let seq3 = seq {
            for i in pos2 do
            yield( (Seq.sum (Seq.take i seq2) ) ) }
seq3
