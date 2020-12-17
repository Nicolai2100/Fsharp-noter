//Ongoing exercise on propositional logic is expected to be available.


//Part 1
(*In this part you shall dene a type for the abstract syntax of propositions and a function
for the semantics of propositions.
1. Declare a type Prop<'a> for propositions so that
 A "a" : Prop<string> representing the atom a,
 A("a",3) : Prop<string*int> representing the atom (a; 3),
 Dis(A "a", A "b") : Prop<string> representing the proposition a v b,
 Con(A "a", A "b") : Prop<string> representing the proposition a ^ b, and
 Neg(A "a"): Prop<string> representing the proposition :a.
*)
type Prop<'a> = | A of 'a
                | Dis of Prop<'a> * Prop<'a>  
                | Con of Prop<'a> * Prop<'a>  
                | Neg of Prop<'a>
                ;;
A "b"
A ("b",3)
Dis (A "a", A "b")  // Dis betyder disjoint som er or
Con (A "a", A "b") // Con betyder and
Neg (A "a") 
          
//2. Declare a function sem: Prop<'a> -> Set<'a> -> bool so that
//          sem p asg = true iff asg |= p holds
let rec sem (p) (asg: Set<Prop<'a>>) = Set.contains p asg;;
let pSet: Set<Prop<string>> = Set.ofList [(A "a") ; (A "b") ] ;;
sem (A "b") pSet ;;
sem (A "a") pSet ;;
sem (A "c") pSet ;;

//Part 2
(*3. Declare function toNnf p transforming a proposition p into an equivalent proposition
in negation normal form, using the de Morgan laws:
!(P ^ Q) is equivalent to (:P) v (:Q)
!(P v Q) is equivalent to (:P) ^ (:Q)
and the law: :(:P) is equivalent to P.*)

let rec toNnf = function   
     | A n -> A n
     | Neg n -> match toNnf n with 
                | Neg n -> ( n)
                | Dis(a,b) -> Con( Neg (toNnf a), Neg (toNnf b))
                | Con(a,b) -> Dis( Neg (toNnf a), Neg (toNnf b))
                | n -> Neg (toNnf n)
     | Dis (a,b) -> match (a,b) with 
                    | (a,b) -> Dis(a,b)
     | Con (a,b) -> Con ((toNnf a), (toNnf b)) 
;;

toNnf (Neg (Neg (A "a")))
toNnf (Neg (A "a"))

toNnf (Neg (Dis (A "a", A "b")));;
toNnf (Neg (Con (A "a", A "b")));;

(*4. First, declare a function onNnf: Prop<'a> -> bool that can decide whether a propo-
sition is on negation normal form, and use property-based testing to validate that
toNnf p is on negation normal form for propositions p of type Prop<string>.*)

let rec onNnf = function   
     | A n -> true
     | Neg n -> match n with 
                | Neg n -> false
                | Dis(a,b) -> false
                | Con(a,b) -> false
                | n -> true
     | Dis (a,b) -> match (a,b) with 
                    | (a,b) -> true
     | Con (a,b) -> true 
;;

onNnf (Neg (A "a"));;
onNnf (Neg (Con (A "a", A "b")));;
onNnf (Dis (Neg (A "a"), Neg (A "b")));;
(*The second step should validate that p is equivalent to toNnf p, that is, sem p asg =
sem (toNnf p) asg, for every proposition p and assignments asg.*)

let asg: Set<Prop> = Set.ofList [(A "a") ; (A "b") ] ;;


//sem p asg = sem (toNnf p) asg
sem (A "b") asg ;;
sem (toNnf (Neg (Neg (A "a")))) asg

//Part 3: Disjunctive Normal Form
(*7 Declare a function dnf that transforms a proposition in negation normal form into
an equivalent proposition in disjunctive normal form using the distributive laws:
a ^ (b v c) is equivalent to (a ^ b) v (a ^ c)
(a v b) ^ c is equivalent to (a ^ c) v (b ^ c)*)

let rec dnf = function   
     | A n -> A n
     | Neg n -> match n with 
                | Neg n -> (dnf n)
                //| Dis(a,b) -> Con( Neg (toNnf a), Neg (toNnf b))
                //| Con(a,b) -> Dis( Neg (toNnf a), Neg (toNnf b))
                | n -> Neg (toNnf n)
     | Dis (a,b) -> match (a,b) with 
                    | (a,b) -> Dis(a,b)
                    | (a,b) -> Dis(a,b)
     | Con (a,b) -> match (a,b) with  
                    | (a, (Dis (b, c))) -> Dis ((Con (a,b)), (Con (a,c)))
                    | ((Dis (b, c)), a) -> Dis ((Con (a,b)), (Con (a,c)))
                    | (a,b) -> Con(a,b)
        ;;

//a ^ (b v c) -> Con (A "a") (Dis (A "b", A "c"))
dnf (Con ((A "a"), (Dis (A "b", A "c"))))

//Part 4
//15. Declare a type Inhabitant having three values K1, K2 and K3.
//type Inhabitant = K1, K2 and K3.

type Prop<'a> = | A of 'a
                | Dis of Prop<'a> * Prop<'a>  
                | Con of Prop<'a> * Prop<'a>  
                | Neg of Prop<'a>
                ;;

//type Inhabitant = Prop<string, int> * Prop<string, int> * Prop<string, int> ;;
type Inhabitant = Prop<string, int> * Prop<string, int> * Prop<string, int> ;;
(A ("K1",1))
(A ("K2",2))
(A ("K3",3))

//Part 5