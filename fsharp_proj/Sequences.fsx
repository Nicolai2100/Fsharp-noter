//Sequences - Overblik side 254




//11.1, 11.2. 11.3, 11.9
//ExerciseWeek11.pdf
//Summer exam 2014, Problem 1(4,5) and Fall exam 2014, Problem 2(1,2,3)
//-- available here


let nat = Seq.initInfinite (fun i -> i);;
Seq.nth 5 nat;;

let idWithPrint i = printfn "%d" i
                    i;;

let natWithPrint = Seq.initInfinite idWithPrint;;
let natWithPrintCached = Seq.cache natWithPrint;;
Seq.nth 5 natWithPrint;;

Seq.init 5 (fun i -> 2*i);;
let s1 = Seq.append (seq [1;2;3;4]) (seq [5;6]);;

let cons x sq = Seq.append (Seq.singleton x) sq;;
cons 5 (seq [6; 7; 8]);;

//11.1 Make a declaration for the sequence of odd numbers.
let odds = Seq.filter (fun n -> n % 2 <> 0) (Seq.initInfinite (fun i -> i));;
Seq.nth 0 odds;;
Seq.nth 1 odds;;
Seq.nth 2 odds;;

let odds2 = (Seq.initInfinite (fun i -> i % 2 <> 0));;
Seq.nth 0 odds2;;
Seq.nth 1 odds2;;
Seq.nth 2 odds2;;

//11.2 Make a declaration for the sequence of numbers 1, 1, 2, 6, . . . , n!, . . ..
let fact = Seq.filter (fun n -> n ) (Seq.initInfinite (fun i -> i + 1));;
Seq.item 0 fact;;
Seq.item 1 fact;;
Seq.item 2 fact;;

let sift a sq = Seq.filter (fun n -> n % a <> 0) sq;;
let rec sieve sq =
    seq { let p = Seq.head sq
          yield p
          yield! sieve(sift p (Seq.tail sq)) };;

let sift2 a sq = seq { for n in sq do
                           if n % a <> 0 then
                               yield n };;


let pos = Seq.initInfinite (fun i -> i+1) ;;
pos

let seq1 = seq {
                 yield (1)
                 for i in pos do
                 //yield (i)
                 yield (i * (i+1))

                 // yield (i) 
                 }
Seq.item 1 seq1
let val1 = Seq.take 5 seq1;;
val1
 
//11.3 
//Make a declaration for the sequence of seq [1; 1; 2; 6; . . . ; n!; . . .], where the i+1’st element is
//generated from the ith element by multiplication with i + 1.

//11.9
//Declare a sequence denoting the following enumeration of the integers:
//0,−1, 1,−2, 2,−3, 3, ...


//2014 - Sommer
let sq = Seq.initInfinite (fun i -> 3*i);;
let k j = seq {for i in sq do
               yield (i,i-j) };;
let xs = Seq.toList (Seq.take 4 sq);;
let ys = Seq.toList (Seq.take 4 (k 2));;

//2014 - Fall 
//1. Declare a function: multTable: int -> seq<int> so that multTable n gives the sequence of the first 10 numbers in the multiplication table for n. For example, multTable 3
//is the sequence of numbers 3, 6, 9, 12, . . . , 30.

let multTable n = 
 Seq.toList (Seq.take 10 ( seq {
               for i in (Seq.initInfinite (fun i -> i+1)) do
               yield (i * n) } ));;
multTable 3

//2. tableOf: int -> int -> (int -> int -> ’a) -> seq<int*int*’a>

let tableOf m n f = 
     Seq.toList (Seq.take n ( seq {
                   for i in (Seq.initInfinite (fun i -> i+1)) do
                   yield (i,i, (f i i)) 
                   yield (i,(f i i), (f (f i i) i)) 
                   }
                   ));;
tableOf 3 4 (+)

//3. Give a declaration for the infinite sequence of strings "a", "aa", "aaa", "aaaa", . . ..
let multTable n = 
 Seq.toList (Seq.take 10 ( seq {
               for i in (Seq.initInfinite (fun i -> i)) do
               yield (i * n) } ));;
multTable 3

let rec repeat items = 
  seq { yield! items  
        yield! repeat items }

repeat "a"


//2016 - Problem 3


let pos = Seq.initInfinite (fun i -> i+1) ;;
pos

let seq1 = seq { yield (0,0)
                 for i in pos do
                 yield (i,i)
                 yield (-i,-i) }
seq1

let val1 = Seq.take 5 seq1;;
val1



let f1 m n k = seq { for x in [0..m] do
                        for y in [0..n] do
                            if x+y < k then
                                yield (x,y) };;

let f2 f p sq = seq { for x in sq do
                        if p x then
                            yield f x };;

let f3 g sq = seq { for s in sq do
                        yield! g s };;
