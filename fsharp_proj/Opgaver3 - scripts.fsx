#I @"C:\Users\NicolaiLarsen\.nuget\packages\fscheck\2.14.3\lib\net452"
#r @"FsCheck.dll"

open FsCheck
let commProp(x,y) = x+y = y+x;;
let commPropTest = Check.Quick commProp;;
let commPropTestVerbose = Check.Verbose commProp;;
//C:\Users\NicolaiLarsen\.nuget\packages\fscheck\2.14.3\lib\net452

(* 
Further property-based testing
The main activity when doing property-based testing is to program correctness properties;
not to make test cases.
You shall now develop programs to address the second property of a sorting program.
By a counting for a given list xs we understand a list of the form [(x1; c1); (x2; c2); : : : ; (xk; ck)],
where xj is an integer (occurring in xs) and cj is a count (a positive integer) for the number
of occurrences of xj in xs. We require that x1 < x2 <    < xk, that is there is a unique
counting for every list xs.
For example, the counting for [3; 2; 6; 3; 2; 1] is [(1; 1); (2; 2); (3; 2); (6; 1)].
1. Declare a function increment(x; cnt). The value of increment(i; cnt) is the counting
obtained from cnt by incrementing the count for x by one.
2. Declare a function toCounting xs, that makes a counting for a given list xs.
3. Use property-based testing to test the property: ordered(toCounting xs).
4. Use property-based testing to test the property: toCounting xs = toCounting(sort xs).
*)