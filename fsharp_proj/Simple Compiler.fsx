open Microsoft.FSharp.Collections

//#I @"C:\Users\NicolaiLarsen\.nuget\packages\fscheck\2.14.3\lib\net452"
//#r @"FsCheck.dll"

//open FsCheck
//let commProp(x,y) = x+y = y+x;;
//let commPropTest = Check.Quick commProp;;
//let commPropTestVerbose = Check.Verbose commProp;;
//C:\Users\NicolaiLarsen\.nuget\packages\fscheck\2.14.3\lib\net452

// Part 1
type Instruction = | ADD | SUB | SIGN | ABS | PUSH of int;;

(* Declare a type Stack for representing the stack, and declare an F# function to interpret
the execution of a single instruction:
intpInstr: Stack -> Instruction -> Stack *)

type Stack = int list;; 

(*Declare a type Stack for representing the stack, and declare an F# function to interpret
the execution of a single instruction:
intpInstr: Stack -> Instruction -> Stack*)

let intpInstr (stack: Stack) (instruct: Instruction) : Stack = 
    if stack.Length < 2 && (instruct.Equals ADD || instruct.Equals SUB) then
        failwith "Stack length too short"
    else if stack.Length < 1 && instruct.Equals SIGN || stack.Length < 1 && instruct.Equals ABS then
        failwith "Stack length too short"
    else
    match instruct with
    | ADD -> [stack.[0] + stack.[1]] @ stack.[2..(stack.Length - 1)]
    | SUB -> [stack.[0] - stack.[1]] @ stack.[2..(stack.Length - 1)]
    | SIGN -> [-1 * stack.[0]] @ stack.[1..]
    | ABS -> if stack.[0] < 0  then
                    if stack.Length > 1 then
                        [-1 * stack.[0]] @ stack.[1..] 
                    else 
                        [-1 * stack.[0]]
                else stack
    | PUSH r -> [r] @ stack.[0..];;

let stack1 = [-1;2;3;4;5];;

intpInstr  [1;2;3;4;5] ABS;;
intpInstr stack1 ABS;;
   
intpInstr [1;2;3] (PUSH 5) ;;
intpInstr [1;2;3] (PUSH 5) ;;
intpInstr [1] (PUSH 5) ;;
intpInstr [1] (PUSH 5) ;;

intpInstr stack1 (PUSH 5) ;;
intpInstr stack1 SIGN;;
intpInstr stack1 ADD;;
intpInstr stack1 SUB;;

(*
A program for the stack machine is a list of instructions [i1; i2; : : : ; in]. A program is executed
by executing the instructions i1; i2; : : : ; in one after the other, in that order, starting with an
empty stack. The result of the execution is the top value of the stack when all instructions
have been executed.
Declare an F# function to interpret the execution of a program:
exec: Instruction list -> int
*)

let exec instList = 
    let listen = List.fold (fun tempList inst -> intpInstr tempList inst ) [] instList
    listen.[0];;
    //listen;;

exec [(PUSH 2) ; SUB ];;
exec [(PUSH 2) ; ADD ];;

exec [(PUSH 2) ; (PUSH 1) ; ABS ; (PUSH 2) ; (PUSH 1) ];;
exec [(PUSH 2) ; ABS ];;
exec [ABS ; (PUSH 2)  ];;
exec [(PUSH 2)];;
exec [(PUSH 1) ; (PUSH 2) ; ADD ];;
exec [(PUSH 1) ; (PUSH 2) ; SUB ];;
exec [(PUSH 1)];;
exec [(PUSH 3); (PUSH 1) ; (ADD)];;
exec [(PUSH 3); (PUSH 1) ; (SUB)];;
exec [(PUSH 3); (PUSH 1) ; (ADD) ; (PUSH 5) ];;
exec [(PUSH 3); (PUSH 1) ; (ADD) ];;
exec [(PUSH 3); (PUSH 1) ; (ADD) ; (SIGN) ; (PUSH 5) ];;
exec [(PUSH 3); (PUSH 1) ; (ADD) ; (PUSH 5) ; (SIGN) ; PUSH 6];;
exec [(PUSH 3); (PUSH 1) ; (ADD) ; (PUSH 5) ; (SIGN) ; PUSH 6 ; (SUB)];;
exec [(PUSH 1); (PUSH 2) ; (PUSH 3) ; (SUB) ; (PUSH 4) ; (PUSH 5) ];;

//Part 2
(*
Declare a type Exp for expressions so that
 X, C -2, C 7
 Abs X, Minus(C 7),
 Add(Abs(Minus(C 7)), Sub(X, Minus(Add(C 2, X))))
are six values of type Exp.
*)
type Exp = | X
           | C of int
           | Abs of Exp
           | Minus of Exp
           | Add of Exp * Exp
           | Sub of Exp * Exp;;
X
C -2
C 7
Abs X
Minus(C 7)
Add(Abs(Minus(C 7)), Sub(X, Minus(Add(C 2, X))))

(*
The semantics (or meaning) of expressions is given by a function:
sem : Exp ! int ! int
where the value of sem e x is the integer computed from e using that variable X has value
x and the constructors Add, Sub, Abs, Minus correspond in an obvious way to operations
on integers.*)

let rec sem (exp: Exp) (x: int) = 
    match exp with 
    | X -> x
    | C a -> a 
    | Abs a -> if (sem a x) < 0 then
                -1 * (sem a x)
               else (sem a x)
    | Minus a -> -1 * (sem a x)
    | Add(a,b) -> (sem a x) + (sem b x)
    | Sub(a,b) -> (sem a x) - (sem b x)
    ;;

sem (C 2) 4
sem X 4 ;;
sem (Minus (C 4)) 4 ;;
sem (Minus (C 4)) 3 ;;
sem (Add (C 2, C 4)) 5 ;;
sem (Abs(Sub (C 2, C 4))) 5 ;;
sem ((Sub (Minus (C 2), C 4))) 5 ;;
sem (Minus X) -4 ;;
sem (Abs X) -4 ;;
sem (Add (X, C 2)) -4 ;;

//Part 3
(*The compilation function is a curried function in order to have a simple setting for handling
the variable X:
compile: Exp -> int -> Instruction list
where the value of the expression compile e x is a list of instructions prg satisfying
exec prg = sem e x
for every expression e and integer x.

Observe that execution of [PUSH 3; PUSH 7; ADD; PUSH 4; PUSH 5; SUB; ADD] gives
the value of the expression (3 + 7) + (4 - 5).
*)

let rec compile (exp: Exp) (x: int) = 
    match exp with 
    | X -> [PUSH x]
    | C a -> [PUSH a] 
    | Abs a -> (compile a x) @ [ABS ] 
    | Minus a -> (compile a x) @ [ SIGN ]
    | Add(a,b) -> (compile a x) @ (compile b x) @ [ADD]
    | Sub(a,b) -> (compile a x) @ (compile b x) @  [SUB]
;;

compile (Abs (Add(C 3, C 7))) 4 ;;
compile (Add(C 3, C 7)) 0;;
compile (Add ((Add(C 3, C 7)), (Sub(C 4, C 5))))  0;;
compile (Sub(C 4, C 5))  0;;
compile (C 2) 4
compile X 4 ;;
compile (Abs (C 4)) 4 ;;
compile (Minus (C 4)) 4 ;;
compile (Add (C 4, C 2)) 4 ;;
compile (Sub (X, C 2)) 3 ;;
compile (Sub (X, Add(X, C 2))) 3 ;;

//Part 4
//red: Exp -> Exp

//| Add(a,b) -> match (red a), (red b) with 
//              | (C a, C b) -> C (a + b) 


let rec red (exp: Exp) = 
    match exp with 
    | X -> X
    | C a -> C a
    | Abs a -> match a with 
               | X -> Abs X 
               | C a -> Abs((red (C a))) 
               | Minus a -> Abs a
               | Abs a -> Abs a
               | a -> red (Abs (red a))
    | Minus a -> match a with 
                 | X -> Minus X 
                 | C a -> C (-1 * a) 
                 | Minus a -> red a
                 | a -> red ( Minus (red a) )
    | Add(a,b) -> match a,b with 
                  | (C a, C b) -> C (a + b) 
                  | (e, C 0) -> red e 
                  | (C 0, e) -> red e
                  | (a, b) -> red (Add( red a, red b))
    | Sub(a,b) -> match a,b with 
                  | (C a, C b) -> C (a - b) 
                  | (e, C 0) -> red e 
                  | (C 0, e) -> Minus (red e)
                  | (a, b) -> red (Sub( red a, red b))
;;

red (Abs(Abs(C 2))) ;;
red (Abs(Sub(C 2, C 7))) ;;
red (Abs(C 2)) ;;
red (Abs(X)) ;;

red (Minus(C 2)) ;;
red (Minus(Minus(C 2))) ;;
red (Minus(Add(C 2, C 1))) ;;

red (Add(C 2, C 3)) ;;
red (Add(C 0, C 3)) ;;
red (Add(C 3, C 0)) ;;

red (Add((Minus(C 3)), C 2)) ;;

red (Sub((Minus (C 3)), C 2)) ;;
red (Sub(C 3, C 0)) ;;
red (Sub(C 0, C 3)) ;;

red (Add(Minus(Add(C 2, (Add(C 2, C 1)))), C 0)) ;;

(*
Declare a function reducible: Exp -> bool that checks whether an expression can be
reduced, that is, some reduction is possible somewhere in an expression.
*)

let reducible (exp:Exp) = 
    let exp2 = red exp;
    not (exp2 = exp)
    ;;

reducible (Minus(C 2)) ;;
red (Minus(C 2)) ;;
reducible (C -2) ;;
