//From the exam fall 2011

//Problem 1
type name = string;;
type phone = int;;
type level = int;;
type description = phone * level;;
type register = (name * description) list;;

//1
let reg1 = [("Joe",(10101010,4)); ("Sal",(11111111,2)); ("Sam",(12121212,7)); ("Jane",(13131313,1)); ];;

//2 getPhone: name -> register -> phone to extra
let rec getPhone name = function
    | (n, (number, _))::rest when n = name -> number 
    | (n, (number, _))::rest -> getPhone name rest
    | _ -> failwith("User not in the register") ;;
getPhone "Jane" reg1

//3. delete: name * register -> register to delete the entry for
let delete ((name:name), (reg:register)) = 
    if (not (Map.containsKey name (Map.ofList reg))) then
           failwith("User not registered")
    else
    List.foldBack (fun (nam,(num,lvl)) rs -> if name = nam then 
                                                rs
                                              else 
                                                (nam,(num,lvl))::rs) reg [];;
delete ("Joe", reg1);;
delete ("Jane", reg1);;

//4. getCandidates: level -> register -> (name*phone) list,
let getCandidates (level:int) (reg:register) = 
    List.fold (fun rs (nam,(num,lvl)) -> if ((System.Math.Abs (lvl - level)) < 3) then 
                                            (nam,num)::rs
                                         else 
                                            rs)  [] reg;;
getCandidates 3 reg1
getCandidates 5 reg1

//Problem 2
//type exp = | C of int
//           | BinOp of exp * string * exp;;

//1 
C 1
BinOp(C 1, "+", C 2)
BinOp(BinOp(C 1, "+", C 2), "+", C 2)

//2 toString: exp -> string
let rec toString = function
    | C x -> string x
    | BinOp(x,y,z) -> "(" + toString x + y + toString z + ")" ;;
toString (C 1)
toString (BinOp(C 1, "+", C 2))
toString (BinOp(BinOp(C 1, "+", C 2), "+", C 2))

//3 Declare a function to extract the set of operators from an expression
let rec extract = function
    | C x -> Set.empty
    | BinOp(x,y,z) -> (Set.union (extract x)   (Set.union (Set.empty.Add y) (extract z) ) ) ;;
    
extract (C 1)
extract (BinOp(C 1, "+", C 2))
extract (BinOp(BinOp(C 1, "*", C 2), "+", C 2))

//4 isDef: exp -> bool

type exp =  | C of int
            | BinOp of exp * string * exp
            | Id of string
            | Def of string * exp * exp;;

Def("x", C 5 , BinOp(Id "x", "+", Id "x"));;

let rec getDefs = function
    | C _ -> []
    | Id _ -> []
    | BinOp(x,y,z) -> getDefs x @ getDefs z
    | Def(x,y,z) -> x :: getDefs y @ getDefs z
    ;;
getDefs (Def("x", C 5 , BinOp(Id "x", "+", Id "x")));;
    
let isDef elem = 
let rec helperF e defs = match e with
    | C _ -> true
    | Id x -> List.contains x defs
    | BinOp(x,y,z) -> helperF x defs && helperF z defs
    | Def(x,y,z) -> helperF y defs && helperF z defs

let helpList = getDefs elem
helperF elem helpList
    ;;
isDef (Def("x", C 5 , BinOp(Id "x", "+", Id "x")));;
isDef (Def("x", C 5 , BinOp(Id "x", "+", Id "y")));;



//Resten er ikke vigtige