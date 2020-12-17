open Microsoft.FSharp.Collections

type Lid = string;;
type Flight = string;;
type Airport = string;;
type Route = (Flight * Airport) list;;
type LuggageCatalogue = (Lid * Route) list;;

let lid1 = "DL 016-914";;
let lid2 = "SK 222-142";;
let flight1a = "DL 189";;
let flight1b = "DL 124";;
let flight1c = "SN 733";;
let flight2a = "SK 208";;
let flight2b = "DL 124";;
let flight2c = "SK 122";;
let destBru = "BRU";;
let destAtl = "ATL";;
let destCph = "CPH";;
let destJfk = "JFK";;
let route1 = [(flight1a, destAtl); (flight1b, destBru); (flight1c, destCph)];;
let route2 = [(flight2a, destAtl); (flight2b, destBru); (flight2c, destJfk)];;

let luggCat = [lid1, route1; lid2, route2]
luggCat
(*
Declare a function findRoute: Lid*LuggageCatalogue -> Route, that  finds the route
for a given luggage identication in a luggage catalogue. A suitable exception should be
raise if a route is not found.
*)
//
let rec findRoute (lid, luggageCat) = 
    match (lid,luggageCat) with
    | ("", _) -> failwith "error 2"
    | (_, []) -> failwith "Luggage ID not in luggage catalouge"
    | (lid, head::_) when lid = fst head -> head
    | (lid, _::rest) -> findRoute (lid,rest)
   ;;
    
findRoute (lid1, luggCat);;
findRoute (lid2, luggCat);;
findRoute ("abc", luggCat);;

(*
Declare a function inRoute: Flight -> Route -> bool, that decides whether
a given flight occurs in a route.
*)
let rec inRoute flight = function
    | [] -> false
    | head::_ when flight = fst head -> true
    | _::tail -> inRoute flight tail;;

inRoute "SK 208" route1;;
inRoute "SK 208" route2;;

(*
3. Declare a function withFlight f lc, where f is a 
ight and lc is a luggage catalogue. The
value of the expression withFlight f lc is a list of luggage identiers for the pieces of
luggage that should travel with f according to lc. The sequence in which the identiers
occur in the list is of no concern.
For the above example, both "DL 016-914" and "SK 222-142" should travel with the 
flight "DL 124"
*)

//let route1 = [(flight1a, destAtl); (flight1b, destBru); (flight1c, destCph)];;
//let route2 = [(flight2a, destAtl); (flight2b, destBru); (flight2c, destJfk)];;
//let luggCat = [lid1, route1; lid2, route2]

let rec withFlight flight lc = 
    match (flight, lc) with
        //| (flight, _) when flight.Length -> failwith "No flight number entered!"
        | (_, []) -> []
        | (flight, head::tail) when (inRoute flight (snd head)) -> let lids = withFlight flight tail
                                                                   (fst head)::lids
        | (flight, _::tail) -> withFlight flight tail
    ;;

withFlight flight1a luggCat ;;
withFlight flight1b luggCat ;;
withFlight flight1c luggCat ;;
withFlight flight2a luggCat ;;
withFlight "SK 208" luggCat;;

(*
4. Declare a function extend: Lid*Route*ArrivalCatalogue -> ArrivalCalalogue so that
extend(lid; r; ac) is the arrival catalogue obtained by extending ac with the information
that lid will arrive at each airport contained in route r.
*)
type ArrivalCatalogue = (Airport * Lid list) list;;

[("ATL", ["DL 016-914"; "SK 222-142"]);
("BRU", ["DL 016-914"; "SK 222-142"]);
("JFK", ["SK 222-142"]);
("CPH", ["DL 016-914"])]
let route1 = [(flight1a, destAtl); (flight1b, destBru); (flight1c, destCph)];;

let arrCat = [("ATL", ["DL 016-914"]);("BRU", ["DL 016-914"]);("CPH", ["DL 016-914"])];;

//tilføje lid til
//snd head::route
//for hvert route element
//Hjælpemetode til at indsætte et lid i et AC

let rec insertLidFromFlight lid (flight, dest) ac = 
    //let rec findSpec = 
    match (ac) with
        | [] -> []
        | head::tail -> let newArrivalCat = insertLidFromFlight lid (flight, dest) tail
                        if fst head = dest then 
                            let tempList = lid::(snd head)
                            ((fst head), tempList)::newArrivalCat
                        else ((fst head), (snd head))::newArrivalCat;;

let rec doesACContainDest dest ac = 
    match (ac) with
    | [] -> false
    | head::_ when (fst head) = dest -> true
    | _::tail -> doesACContainDest dest tail;;

doesACContainDest "BRU" arrCat;;
doesACContainDest "CPH" arrCat;;
doesACContainDest "ATL" arrCat;;
doesACContainDest "JFK" arrCat;;

let tempList = []
let tempList2 = "1234"::tempList
("BRU", tempList2)::arrCat

let rec insertLidFromFlight lid (flight, dest) ac = 
    match (ac) with
        | [] -> []
        //| head::tail when doesACContainDest dest (fst head) -> let newArrivalCat = insertLidFromFlight lid (flight, dest) tail
        //                                                       let tempList = [lid]
        //                                                       (dest, tempList)::newArrivalCat
        | head::tail when (fst head) = dest -> let newArrivalCat = insertLidFromFlight lid (flight, dest) tail
                                               let tempList = lid::(snd head)
                                               ((fst head), tempList)::newArrivalCat
        | head::tail -> let newArrivalCat = insertLidFromFlight lid (flight, dest) tail
                        ((fst head), (snd head))::newArrivalCat;;

let rec insertLidFromRoute lid r ac = 
    match (r) with
        | [] -> []
        | head::tail -> let newArrivalCat = insertLidFromFlight lid (fst head, snd head) ac
                        newArrivalCat :: insertLidFromRoute lid tail ac
        //| head::tail -> let newArrivalCat = insertLidFromFlight lid (fst head, snd head) ac
        //                newArrivalCat @ insertLidFromRoute lid tail ac

arrCat;;

let newAC = insertLidFromFlight "SK 222-142" (flight1a ,"ATL") arrCat;;
let newAC = insertLidFromFlight "SK 222-142" (flight1a ,"BRU") arrCat;;
let newAC = insertLidFromFlight "SK 222-142" (flight1a ,"JFK") arrCat;;
let newAC = insertLidFromRoute "SK 222-142" route2 arrCat;;

let rec extend lid r ac = 
    match (r) with
        | [] -> ac
        | (_ , airport)::tail -> let newArrivalCat = insertLidFromRoute lid airport ac
                                 extend lid tail newArrivalCat
                                 ;;

let ac1: ArrivalCatalogue = [];;
extend lid1 route1 ac1;;

(*
5. Declare a function toArrivalCatalogue: LuggageCatalogue -> ArrivalCatalogue, that
creates an arrival catalogue from the information of a given luggage catalogue.
*)





(*
Task 3
You shall now solve questions 2. and 3. from the Airport-Luggage problem using higher-order
functions from the list library. Attach in a comment to the programs justications for your
choices (of library functions).
*)

//3.2
(*
Declare a function inRoute: Flight -> Route -> bool, that decides whether
a given flight occurs in a route.
*)
let inRoute2 flight route = List.fold (fun isContained (fli, dest) -> if flight = fli then true else isContained) false route;;
inRoute2 "SK 208" route1;;
inRoute2 "SK 208" route2;;
//3.3

//let route1 = [(flight1a, destAtl); (flight1b, destBru); (flight1c, destCph)];;
//let route2 = [(flight2a, destAtl); (flight2b, destBru); (flight2c, destJfk)];;
//let luggCat = [lid1, route1; lid2, route2]


let withFlight2 flight luggCat = 
    List.fold (fun lidsInFlight (lid, (flightDest)) -> 
    if inRoute2 flight flightDest then 
        lid::lidsInFlight 
    else lidsInFlight) [] luggCat;;

luggCat;;
withFlight2 flight1a luggCat ;;
withFlight2 flight1b luggCat ;;
withFlight2 flight1c luggCat ;;
withFlight2 flight2a luggCat ;;
withFlight2 "SK 208" luggCat;;

//3.4
(*
Declare a function extend: Lid*Route*ArrivalCatalogue -> ArrivalCalalogue so that
extend(lid, r, ac) is the arrival catalogue obtained by extending ac with the information
that lid will arrive at each airport contained in route r
*)

let extend = function 
    |
    |
extend(lid, r, ac)