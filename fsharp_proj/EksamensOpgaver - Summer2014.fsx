//From the exam Summer 2014

//Problem 2
//ordered


let rec ordered = 
    function
    | [] -> true
    | x1::x2::rest -> x1 >= x2 && ordered (x2::rest)
    | _::_ -> true
    ;;
ordered [1;2;3;4];;
ordered (List.rev [1;2;3;4]);;

let rec smallerThanAll x xs = List.fold (fun b y -> b && x < y) true xs;;
smallerThanAll 0 [1;2;3;4];;
smallerThanAll -1 [1;2;3;4];;
smallerThanAll 4 [1;2;3;4];;
smallerThanAll 5 [1;2;3;4];;

//insertBefore
let rec insertBefore p x = 
    function
    | [] -> []
    | y::rest when (p x y) -> x::y::rest
    | y::rest -> y :: insertBefore p x rest
    ;;
insertBefore (fun x y -> x > y ) 88 [1;2;3;4;99];;
insertBefore (fun x y -> x > y ) 88 [];;
insertBefore (fun x y -> x < y ) 88 [1;2;3;4;99];;



type Sex = | M // male
           | F // female

let sexToString sex = 
    match sex with 
    | M -> "Male"
    | F -> "Female";;

let rec replicate n str = 
    match n with 
    | 0 -> ""
    | n -> str + replicate (n-1) str;;

replicate 0 "abc"
replicate 3 "abc"

//Problem 3

type Name = string;;
type Sex = | M // male
           | F // female
type YearOfBirth = int;;
type FamilyTree = P of Name * Sex * YearOfBirth * Children
and Children = FamilyTree list;;

let fTree1: FamilyTree =  P("Mary", F, 1980, [P("Peter", M, 2005, []) ; P("Bob", M, 2008, []); P("Eve", F, 2010, []) ] );;
let fTree2: FamilyTree =  P("Mary", F, 1980, [P("Peter", M, 2005, []) ; P("Bob", M, 2008, [P("Peter", M, 2020, [])]); P("Eve", F, 2010, []) ] );;
let fTreeFalse1: FamilyTree =  P("Mary", F, 1980, [P("Peter", M, 2005, []) ; P("Bob", M, 1907, []); P("Eve", F, 2010, []) ] );;
let fTreeFalse2: FamilyTree =  P("Mary", F, 1980, [P("Peter", M, 2005, []) ; P("Bob", M, 2008, [P("Peter", M, 1901, [])]); P("Eve", F, 2010, []) ] );;

//isWF: FamilyTree -> bool
let rec isWF (P(n,s,yob, children)) = List.fold (fun wf (P(n',s',yob', children')) -> wf && (yob < yob' ) ) true children ;; 
let rec isWF (P(n,s,yob, children)) = List.fold (fun wf (P(n',s',yob', children')) -> wf && (yob < yob') && (isWF (P(n',s',yob', children')))) true children ;; 
isWF fTree1
isWF fTree2
isWF fTreeFalse1
isWF fTreeFalse2

//makePerson: Name*Sex*YearOfBith -> FamilyTree
let makePerson (name,sex,yob) = P(name,sex,yob,[]);;
makePerson ("Hansen", M, 1922);;

//insertChildOf: Name -> FamilyTree -> FamilyTree -> FamilyTree option
//insertChildOfInList: Name -> FamilyTree -> Children -> Children option
