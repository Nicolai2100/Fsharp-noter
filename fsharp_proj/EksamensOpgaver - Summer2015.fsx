//From the exam summer 2015

//Problem 1
//repeat: string -> int -> string
let rec repeat str n = 
    match n with
    | 0 -> ""
    | x when x > 0 -> str + (repeat str (x-1))
    | _ -> failwith("error in input");;
repeat "ab" 8

let rec f str1 str2 n = 
    match n with
    | 0 -> ""
    | x when x > 0 && x % 2 = 0 -> str2 + "\n" + (f str1 str2 (x-1))
    | x when x > 0 -> str1 + "\n" + (f str1 str2 (x-1))
    | _ -> failwith("error in input");;
f "XO" "OX" 1
f "ab" "cd" 3

let rec viz m n = 
    match n with
    | 0 -> ""
    | x when x > 0 && x % 2 = 0 -> repeat "OX" m + "\n" + (viz m (x-1))
    | x when x > 0 -> repeat "XO" m + "\n" + (viz m (x-1))
    | _ -> failwith("error in input");;
viz 4 5
printfn "%s" (viz 4 5)

//1. Make a tail-recursive variant of repeat using an accumulating parameter.
let rec repeatAccu str n akku = 
    match n with
    | 0 -> "" + akku
    | x when x > 0 -> repeatAccu str (x-1) (str + akku)
    | _ -> failwith("error in input");;
repeatAccu "ab" 8 ""

//2. Make a continuation-based tail-recursive variant of repeat.
let rec repeatCon str n k = 
    match n with
    | 0 -> k ""
    | x when x > 0 -> repeatCon str (x-1) (fun v -> k(v + str))
    | _ -> failwith("error in input");;
repeatCon "ab" 8 (fun x -> x)

//Problem 2
//mixMap
let rec mixMap f ls1 ls2 = 
    match (ls1, ls2) with
        | [], [] -> [] 
        | x::rextx, y::resty -> let list = mixMap f rextx resty
                                f(x, y)::list
        | _ -> failwith("error in input");;

mixMap (fun (x,y) -> (x*2,y*3)) [1;2;3;4] [1;2;3;4] 

let rec unMixMap f = 
    function
    | [] -> ([],[]) 
    | (x,y)::rest -> let (a, b) = unMixMap f rest
                     ((f x)::a, (f y)::b)
    | _ -> failwith("error in input");;

unMixMap (fun (x) -> (x * 2)) [(1,2);(3,4);(5,6);(7,8)]

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