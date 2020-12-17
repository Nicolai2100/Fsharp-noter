open Microsoft.FSharp.Collections

type exp = | C of int
           | BinOp of exp * string * exp
           | Id of string
           | Def of string * exp * exp;;

//1. Give three different values of type exp.
C 5;;
(C 5, "+", C 2);; 
(C 5, "*", C 2);; 

//2. Declare a function toString: exp -> string, that gives a string representation
//for an expression. Put brackets around every subexpression with operators, e.g.
//(3+(5*2)) is a string representation of the above example.

let rec toString = function 
    | C n-> string n 
    | BinOp (a, b, c) -> (toString a) + " " + b + " " + (toString c);;

toString (BinOp(C 3, " + ", C 5))
toString (BinOp(C 5, "*", C 2))
toString (BinOp(C 3, " + ", (BinOp(C 5, "*", C 2))))
toString (C 3)
(3+(5*2))

//3. Declare a function to extract the set of operators from an expression

let rec extractOperators = function
    | (C _) -> Set.empty 
    | (BinOp (a,b,c)) -> Set.union (Set.union (Set.singleton b) (extractOperators a)) (extractOperators c) ;;

extractOperators (BinOp(C 3, "+", C 5))
extractOperators (BinOp(C 3, "+", (BinOp(C 5, "*", C 2))))

(*
4. The type for expressions is now extended to in
lude identiers (
onstrutor Id) and local denitions (
constructor Def): *)
// Hint: make use of an auxiliary function having an extra argument that takes care of dened identiers.

let rec curEnv exp env : Set<string> = 
    match exp with
      | (C x) -> env
      | (BinOp (a, b, c)) -> Set.union (curEnv a env) (curEnv c env) 
      | Def (a, b, c) -> curEnv c (Set.union (Set.singleton a) env)
      | Id (x) -> env;;

curEnv (C 2) Set.empty ;;
curEnv (BinOp(C 2, "+", C 3)) Set.empty ;;
curEnv (BinOp(Id "x", "+", C 3)) Set.empty ;;

curEnv (Def("x", C 5, C 2) ) Set.empty ;;
curEnv (Def("x", C 5, BinOp(C 2, "+", C 3)) ) Set.empty ;;
curEnv (Def("x", C 5, BinOp(Id "x", "+", Id "x"))) Set.empty ;;

let isDef exp =  
    let vars = curEnv exp Set.empty
    let rec helperFunc exp = 
        match exp with
             | C _ -> true
             | BinOp (a, b, c) -> helperFunc a && helperFunc c
             | Def (a, b, c) -> helperFunc c
             | Id (x) -> if Set.contains x vars then
                            true 
                         else 
                            false
    helperFunc exp;;

isDef (C 5) ;;
isDef (Def("x", C 5, C 2)) ;;
isDef (Def("x", C 5, Id "x")) ;;
isDef (BinOp(C 5, "+", C 2)) ;;

isDef (BinOp(Id "x", "+", C 2)) ;;
isDef (Def("x", C 5, BinOp(C 2, "+", C 1))) ;;
isDef (Def("x", C 5, BinOp(Id "x", "+", Id "x"))) ;;
isDef (Def("x", C 5, BinOp(Id "y", "+", Id "x"))) ;;

// BinOp (a, b, c) -> a og b kan indeholde en definition
// Def (a, b, c) -> b og c kan indeholde en definition
// Id (x) skal kontrolleres om den er defineret
// C _ vil altid være defineret - kræver ikke at blive defineret

(*
Make an interpreter for a simple imperative language (see exercises on the slides)
by completion of this program skeleton.


Complete the program skeleton for the interpreter, and try some examples.
 Extend the abstract syntax and the interpreter with if-then and
repeat-until statements.
 Suppose that an expression of the form inc(x) is added. It adds
one to the value of x in the current state, and the value of the
expression is this new value of x.
How would you refine the interpreter to cope with this construct?
 Analyse the problem and state the types for the refined interpretation functions
*)

type AExp = (* Arithmetical expressions *)
    | N of int (* numbers *)
    | V of string (* variables *)
    | Add of AExp * AExp (* addition *)
    | Mul of AExp * AExp (* multiplication *)
    | Sub of AExp * AExp;; (* subtraction *)

let rec A a s =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s;;

type BExp = (* Boolean expressions *)
    | TT (* true *)
    | FF (* false *)
    | Eq of AExp * AExp (* equality *)
    | Lt of AExp * AExp (* less than *)
    | Neg of BExp (* negation *)
    | Con of BExp * BExp ;; (* conjunction *)

let rec B b s =
    match b with
    | TT -> true
    | FF -> false
    | Eq (b1, b2) -> b1 = b2 
    | Lt (b1, b2) -> b1 < b2 
    | Neg (b1) -> not (B b1 s)
    | Con (b1, b2) -> (B b1 s) && (B b2 s) ;;

B TT [];; 
B FF [];; 
//B (Eq (TT,FF)) [];; 
//B (Eq (TT,TT)) [];; 
B (Eq (N 1, N 2)) [];; 
B (Eq (N 1, N 1)) [];; 
B (Lt (N 1, N 2)) [];; 
B (Lt (N 1, N 1)) [];; 
B (Lt (N 2, N 1)) [];; 
B (Lt (N 2, N 1)) [];; 

B (Con (TT, (Eq (N 1, N 1)))) [] ;; 
B (Con (TT, FF)) [];; 

B (Eq (N 2, N 0)) [] ;;
B (Neg ( Eq (N 2, N 0))) [] ;;

// y:=1 ; while not(x=0) do (y:= y*x ; x:=x-1)

type Stm = (* statements *)
    | Ass of string * AExp (* assignment *)
    | Skip
    | Seq of Stm * Stm (* sequential composition *)
    | ITE of BExp * Stm * Stm (* if-then-else *)
    | While of BExp * Stm;; (* while *) 

//y:=1 ; while not(x=0) do (y:= y*x ; x:=x-1)
Ass ("y", N 1)  ;;
While ( Neg(Eq(N 2, N 0)), Ass("y",Mul(V "y", V "x"))) ;;
Ass ("x", Sub (V "x", N -1 )) ;;

let update x v s = Map.add x v s;;

let y1 = "x";;
let v1 = 3;;
let s1 = Map.empty;;

(update y1 v1 s1).Item "x";;

let rec I stm s =
    match stm with
    | Ass(x, a) -> update x a s
    | Skip -> s
    | Seq(stm1, stm2) -> I stm2 (I stm1 s) 
    | ITE(b,stm1,stm2) -> if B b s then 
                            I stm1 s
                          else 
                            I stm2 s
    | While(b, stm) -> if B b s then (I (While(b, stm)) s)
                       else s
    ;;


let s0 = Map.ofList [("x", N 4)];;

I (Ass ("x", Sub (V "x", N -1 ))) Map.empty ;;
I (Ass ("x", Sub (V "x", N -1 ))) s0 ;;
