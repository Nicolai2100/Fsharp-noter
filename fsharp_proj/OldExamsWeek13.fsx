//Problem 2 from May 2019 (Approx. 72 minutes)
type Part = string ;;
type Task = string  ;;
type Cost = int (* can be assumed to  be positive *)
type Duration = int (* can be assumed to be positive *)
type PartReg = Map<Part, Cost>  ;;
type TaskReg = Map<Task, Duration*Cost>  ;;
(* Part and task registers for balance bikes *)
type WorkStation = Task * (Part * int) list ;;
type AssemblyLine = WorkStation list ;;

let preg1 = Map.ofList [("wheel",50); ("saddle",10); ("handlebars",75); ("frame",100); ("screw bolt",5); ("nut",3)];;
let treg1 = Map.ofList [("addWheels",(10,2)); ("addSaddle",(5,2)); ("addHandlebars",(6,1))] ;;
let ws1 = ("addWheels",[("wheel",2);("frame",1);("screw bolt",2);("nut",2)]) ;;
let ws2 = ("addSaddle",[("saddle",1);("screw bolt",1);("nut",1)]) ;;
let ws3 = ("addHandlebars",[("handlebars",1);("screw bolt",1);("nut",1)]) ;;
let al1 = [ws1; ws2; ws3];;



// 1) Declare a function wellDefWS: PartReg -> TaskReg -> WorkStation -> bool that
//checks the well-definedness of a workstation for given part and task registers.

let rec wellDefWs pReg tReg ws = 
    match ws with
    | (t, pList) when Map.containsKey t tReg -> 
                                        match pList with
                                        | [] -> true
                                        | (part, _)::rest when Map.containsKey part pReg -> wellDefWs pReg tReg (t, rest)
                                        | _ -> false
    | _ -> false
    ;;

let ws3' = ("addHandlebars",[("handlebars",1);("screw olt",1);("nut",1)]) ;;   
let ws4 = ("addHandlebar",[("handlebars",1);("screw bolt",1);("nut",1)])
wellDefWs preg1 treg1 ws3
wellDefWs preg1 treg1 ws3'
wellDefWs preg1 treg1 ws4

//Declare a function wellDefAL: PartReg -> TaskReg -> AssemblyLine -> bool that
//checks the well-definedness of an assembly line for given part and task registers. This
//function should be declared using List.forall.
  
let rec wellDefAL pReg tReg al = 
    List.forall (wellDefWs pReg tReg) al
    ;;

wellDefAL preg1 treg1 al1
let al2 = [ws1; ws2; ws3'];;
wellDefAL preg1 treg1 al2
let al3 = [ws1; ws2; ws3; ws4];;
wellDefAL preg1 treg1 al3

//3: Declare a function longestDuration(al; treg), where al is an assembly line and treg a
//task register. The value of longestDuration(al; treg) is the longest duration of a task
//in al. What is the type of longestDuration?
//For example, the longest duration of a task in the assembly line for balanced bikes is 10
//(the duration of "addWheels").
let longestDuration al treg = List.fold (fun x (name, _) -> if (fst (Map.find name treg)) > x then 
                                                                (fst (Map.find name treg)) 
                                                            else x ) 0 al ;;
longestDuration al1 treg1
let al4 = [ws2; ws3];;
longestDuration al4 treg1

//4: Declare a function partCostAL: PartReg -> AssemblyLine -> Cost, that computes
//the accumulated cost of all parts needed for one nal product of an assembly line for a
//given part register. For example, the accumulated cost of all parts of a balanced bike
//is 317 - the cost of one frame, two wheels, one saddle, handlebars, 4 nuts and 4 screw
//bolts.
//Hint: You may introduce helper functions to deal with workstations and part lists
//[(p1; k1); : : : ; (pn; kn)].

let partCostWS ws preg = List.fold (fun accu (name, amount) -> accu + ((Map.find name preg) * amount) ) 0 (snd ws) ;;

let rec partCostAL preg = function 
    | [] -> 0
    | ws::rest -> (partCostWS ws preg ) + partCostAL preg rest
    ;;
partCostAL preg1 al1
partCostAL preg1 al4

//5: Declare a function prodDurCost: TaskReg -> AssemblyLine -> Duration*Cost, that
//for a given assembly line and task register, computes a pair (totalDuration; totalCost),
//where totalDuration is the accumulated duration of all durations of tasks in the assem-
//bly line and totalCost is the accumulated cost of the costs of all tasks in the assembly
//line (where the cost of parts is ignored). For the balanced bike example, the accumulated
//duration of the three tasks is 21 and the accumulated cost is 5.
let prodDurCost al treg = List.fold (fun (d,c) (name, _) -> (d + (fst (Map.find name treg)), c + (snd (Map.find name treg))   ))  (0,0) al ;;
prodDurCost al1 treg1

//A stock is mapping from parts to number of pieces:
//type Stock = Map<Part, int>
type Stock = Map<Part, int>;;

//6: Declare a function toStock: AssemblyLine -> Stock, that for a given assembly line,
//computes the stock needed to produce a single product.

let addStock2Map ws  = List.fold (fun map (name, amount) -> Map.add name amount map) Map.empty (snd ws) ;;
addStock2Map ws1
addStock2Map ws2
addStock2Map ws3

let stockPartsWS ws map = List.fold (fun map (name, amount) -> if (Map.containsKey name map) 
                                                                then 
                                                                    Map.add name (amount + (Map.find name map)) map
                                                                else 
                                                                    Map.add name amount map
                                                                ) map (snd ws) ;;

let rec toStock al = 
    List.fold (fun map ws -> stockPartsWS ws map ) Map.empty al;;
toStock al1


//Problem 3 from May 16 (approx 48 minutes)
type Container =
    | Tank of float * float * float // (length, width, height)
    | Cylinder of float * float // (radius, height)  ;;
    | Ball of float // radius ;;

//1. Declare two F# values of type Container for a tank and a ball, respectively.
let t1 = Tank(1.2,1.2,1.2);;
let b1 = Ball(1.3)
let c1 = Cylinder(1.2,1.2);;

//A tank is called well-formed when its length, width and height are all positive and a ball
//is well-formed when its radius is positive. Declare a function isWF : Container → bool
//that can test whether a container is well-formed.

let isWF = function
    | Tank(a,b,c) when a > 0.0 && b > 0.0 && c > 0.0 -> true
    | Tank(_) -> false
    | Cylinder(a,b) when a > 0.0 && b > 0.0 -> true
    | Cylinder(_) -> false
    | Ball(x) when x > 0.0 -> true
    | Ball(_) -> false;;
isWF t1
isWF b1
isWF c1
isWF (Ball(-1.0))

(*Declare a function volume c computing the volume of a container c. 
(Note that the volume of ball with radius r is 4/3 · π · r^3 .)
(Note that the volume of cylinder
//with radius r and height h is pi * r^2 * h
*)
let volume = function
    | Tank(a,b,c) when isWF (Tank(a,b,c)) ->  a * b * c
    | Ball(x) when isWF (Ball(x)) -> (4.0/3.0) * System.Math.PI * (float x)**3.0
    | Cylinder(a,b) when isWF (Cylinder(a,b)) -> System.Math.PI * (float a)**2.0 * b
    | _ -> 0.0;;
volume (Tank(1.2,1.2,1.2));;
volume (Ball(1.3)) ;;
volume (Cylinder(1.3, 1.2)) ;;

type Name = string;;
type Contents = string;;
type Storage = Map<Name, Contents*Container>;;


//5: Declare a value of type Storage, containing a tank with name "tank1" and contents
//"oil" and a ball with name "ball1" and contents "water".

let storage1 = Map.ofList [("tank1",( "oil",(Tank(1.1, 1.2, 1.3)) )) ; ("ball1",( "water",(Ball(1.1)) )) ]
storage1.Item "tank1"
storage1

//6: Declare a function find : Name -> Storage -> Contents * float, where find n stg
//should return the pair (cnt; vol) when cnt is the contents of a container with name n
//in storage stg, and vol is the volume of that container. A suitable exception must be
//raised when no container has name n in storage stg.

let rec find name map = 
    if (Map.containsKey name map) then
        let (elem, container) = Map.find name map
        (elem, volume container)
    else 
        failwith "No such container - Try another name!";;

find "tank1" storage1;;
find "ball1" storage1;;
find "" storage1;;







//Problem 4 from May 16 (approx. 48 minutes)
type T<'a> = L 
            | N of T<'a> * 'a * T<'a>;; 

let t = N(N(L, 1, N(N(L, 2, L), 1, L)), 3, L);;
t
//1. Give the type of t. Furthermore, provide three values of type T<bool list>.
let t1 = N(N(L, true, N(N(L, true, L), true, L)), false, L);;
let t2 = N(N(L, true, N(N(L, true, L), true, L)), false, L);;
let t3 = N(N(L, true, N(N(L, true, L), true, L)), false, L);;
t3

//3. . Declare a function count a t that can count the number of occurrences of a in the binary
//tree t. For example, the number of occurrences of 1 in the tree t is 2.
let rec count a = function
    | (L) -> 0
    | (N(x, y, z)) when y = a -> 1 + count a x + count a z
    | (N(x, _, z)) -> count a x + count a z ;;

count 1 t
count 2 t
count false t1
count true t1

//4. Declare a function replace, so that replace a b t is the tree obtained from t by 
//replacement of every occurrence of a by b. For example, replace 1 0 t gives the tree
//N(N(L, 0, N(N(L, 2, L), 0, L)), 3, L).

let rec replace a b = function
    | (L) -> (L)
    | (N(x, y, z)) when y = a -> (N( (replace a b x) , b , (replace a b z) ) )
    | (N(x, y, z)) ->  (N( (replace a b x) , y , (replace a b z) ) )

replace 1 0 t
replace true false t1
replace false true t1