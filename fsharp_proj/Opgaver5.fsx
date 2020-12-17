#I @"C:\Users\NicolaiLarsen\.nuget\packages\fscheck\2.14.3\lib\net452"
#r @"FsCheck.dll"

open FsCheck
let commProp(x,y) = x+y = y+x;;
let commPropTest = Check.Quick commProp;;
let commPropTestVerbose = Check.Verbose commProp;;
//C:\Users\NicolaiLarsen\.nuget\packages\fscheck\2.14.3\lib\net452

// 1 - Revise your solution of the simple company club problem (Week 38) using sets and maps whenever that is appropriate.
type Name = string ;;
type no = string;;
type yb = int;;
type Themes = string list;;
type Description = (no * yb * Themes) ;;
type Member = Name * Description;;
type Register = Map<Name, Description> ;;
type Arrangement = Member -> bool ;;

let p1 (_,(_, yb, ths)) = yb > 1982 && List.contains "jazz" ths;;
let m1:Member = ("Karl Karlsen", ("22342016", 1991, ["ost";"jazz"]));;
let m2:Member = ("Karl2 Karlsen2", ("22342016", 1977, ["ost";"jazz"]));;
Map.add "Karl Karlsen" m1;;
Map.add "Karl2 Karlsen2" m2;;
// let memberList:Register = [m1;m2];;
p1 (m1);;

//3. A declaration of a function extractTargetGroup p r that gives a list with names and 
//phone numbers of the members in register r that may be interested in the arrangement p. 
//State the type of extractTargetGroup in a comment. Make use of the type names introduced
//under point 1. above, so that the type reflects the intention with the function. 

let extractTargetGroup (p : Arrangement, r : Register) = 
    match r with
    | [] -> []
    | (name, Description)::register when p (name, Description) -> let newMemberList = extractTargetGroup (p, register) 
                                                                  let number,_,_ = Description
                                                                  (name, number)::newMemberList
    | _::register -> extractTargetGroup (p, register)  ;;      

extractTargetGroup (p1, memberList);;


//let makeBill’ reg pur =
//let f (np,ac) (infos,billprice)
//= let (aname, aprice) = Map.find ac reg
//let tprice = np*aprice
//((np,aname,tprice)::infos, tprice+billprice)
//List.foldBack f pur ([],0);;



// Problem 1, questions 6 from Exam, Fall 2013.

//FAIL

// Solve Problem 4 from the exam summer 2015 (available here). Make a solution on paper first -- the final exam will be laptop-free. Try out your program.

type CourseNo = int;;
type Title = string;;
type ECTS = int;;
type CourseDesc = Title * ECTS;;
type CourseBase = Map<CourseNo, CourseDesc>;;
(*
1. Declare a function isValidCourseDesc: CourseDesc -> bool,
where isValidCourseDesc desc is true if the ECTS part of desc is valid.
*)

let title: Title = "Programmering 1";
let ects: ECTS = 15;
let desc1: CourseDesc = (title, ects);
let desc2: CourseDesc = (title, ects);
let desc3: CourseDesc = (title, ects);
let desc4: CourseDesc = (title, ects);
let desc5: CourseDesc = (title, ects);
let desc6: CourseDesc = (title, ects);
let desc7: CourseDesc = (title, ects);
let desc8: CourseDesc = (title, ects);
let desc9: CourseDesc = (title, ects);
let desc10: CourseDesc = (title, ects);
let desc11: CourseDesc = (title, ects);
let desc12: CourseDesc = (title, ects);
let desc13: CourseDesc = (title, ects);
let desc14: CourseDesc = (title, ects);
let desc15: CourseDesc = (title, ects);
let desc16: CourseDesc = (title, ects);
let desc17: CourseDesc = (title, ects);
let desc18: CourseDesc = (title, ects);
let desc19: CourseDesc = (title, ects);
let desc20: CourseDesc = (title, ects);
let desc21: CourseDesc = (title, ects);
let desc22: CourseDesc = (title, ects);
let desc23: CourseDesc = (title, ects);
let desc24: CourseDesc = (title, ects);
let desc25: CourseDesc = (title, ects);
let desc26: CourseDesc = (title, ects);
let desc27: CourseDesc = (title, ects);
let desc28: CourseDesc = (title, ects);
let desc29: CourseDesc = (title, ects);
let desc30: CourseDesc = (title, ects);

let courseBase: CourseBase = Map.ofList [1, desc1 ; 2, desc2 ; 3, desc3 ; 4, desc4 ; 5, desc5 ; 6, desc6 ; 7, desc7 ; 8, desc8 ; 9, desc9 ; 10, desc10 ;
11, desc11 ; 12, desc12 ; 13, desc13 ; 14, desc14 ; 15, desc15 ; 16, desc16 ; 17, desc17 ; 18, desc18 ; 19, desc19 ; 20, desc20 ;
21, desc21 ; 22, desc22 ; 23, desc23 ; 24, desc24 ; 25, desc25 ; 26, desc26 ; 27, desc27 ; 28, desc28 ; 29, desc29 ; 30, desc30] ;;

let isValidCourseDesc (courseDesc: CourseDesc) = snd courseDesc % 5 = 0 && (snd courseDesc) > 0;;
isValidCourseDesc desc3;;

(*
2. Declare a function isValidCourseBase: CourseBase -> bool,
where isValidCourseBase cb is true if every course description occurring the course
base cb is valid, that is, it satisfies the predicate isValidCourseDesc.
*)

let isValidCourseBase courseBase : bool = Map.forall (fun _ (_,p) -> p % 5 = 0) courseBase;;
let isValidCourseBase courseBase = Map.forall (fun _ (title, ects) -> isValidCourseDesc (title, ects)) courseBase;;
isValidCourseBase courseBase;;

(*
3. Declare a function disjoint: Set<’a> -> Set<’a> -> bool, where disjoint s1 s2
is true if the two sets s1 and s2 have no common element, that is, they are disjoint.
*)
type Mandatory = Set<CourseNo>;;
type Optional = Set<CourseNo>;;
type CourseGroup = Mandatory * Optional;;

let mandatory: Mandatory = Set.ofList [02131 ; 02141];
let mandatory2: Mandatory = Set.ofList [02131];
let optional: Optional = Set.ofList [02157 ; 02158];
let courseGroup: CourseGroup = (mandatory, optional);

let disjoint xs ys = 
    if (Set.toList xs).Length >= (Set.toList ys).Length 
    then let longList = xs 
         let shortList = ys
         Set.fold (fun isDisjoint value -> isDisjoint && not (longList.Contains value) ) true shortList
    else let longList = ys
         let shortList = xs
         Set.fold (fun isDisjoint value -> isDisjoint && not (longList.Contains value) ) true shortList ;;

mandatory
mandatory2
disjoint  mandatory2 mandatory;;
disjoint  mandatory mandatory2;;
disjoint  mandatory mandatory;;
disjoint  optional mandatory;;
disjoint  mandatory optional;;

(*
Declare a function sumECTS: Set<CourseNo> -> CourseBase -> int,
where sumECTS cs cb is the sum of all ECTS points of the courses with numbers in cs,
where the ECTS points are extracted from course descriptions in the course base cb.
*)

let courses = Set.ofList [1 ; 2 ; 3];

let sumECTS cs cb = 
     Set.fold (fun ectsSum courseNum -> ectsSum + snd( Option.get(Map.tryFind courseNum cb))) 0 cs ;;

sumECTS courses courseBase;;

(*
A course group (man, opt) for a bachelor programme is valid for a given course base cb if:
• man and opt are disjoint,
• the sum of all mandatory ECTS points (i.e. the ECTS sum for all courses in man) is less than or equal to 45,
• the set of optional courses opt is empty when the mandatory ECTS points add up to 45, and
• the total number of ECTS points of mandatory and optional courses should be at
least 45

Declare a function isValidCourseGroup: CourseGroup -> CourseBase -> bool that
can check whether a course group is valid for a given course base
*)


let isValidCourseGroup (mandatory, optional) courseBase = 
    let isDisjoint = disjoint mandatory optional
    let manEctsPoints = sumECTS mandatory courseBase
    let optEctsPoints = sumECTS optional courseBase
    let isEnoughEcts = manEctsPoints + optEctsPoints >= 45
    if  manEctsPoints >= 45  
        then if optEctsPoints = 0 
             then true && isDisjoint && isEnoughEcts
             else false
    else isDisjoint && isEnoughEcts
    ;;

let courseGroup2: CourseGroup = (Set [1], Set [2]);
    
isValidCourseGroup courseGroup2 courseBase;;

(*
A flag model (bns, tc, pps, ep) is valid if
• the three course groups bns, tc and pps are all valid,
• no course belongs to more than one of the course groups bns, tc and pps, and
• any course belonging to a course group bns, tc or pps must qualify as an elective
course, that is, it must satisfy the predicate ep.
6. Declare a function isValid: FlagModel -> CourseBase -> bool that can test whether
a flag model is valid for a given course base.
*)

type BasicNaturalScience = CourseGroup;;
type TechnologicalCore = CourseGroup;;
type ProjectProfessionalSkill = CourseGroup;;
type Elective = CourseNo -> bool;;
type FlagModel = BasicNaturalScience * TechnologicalCore * ProjectProfessionalSkill * Elective;;
type CoursePlan = Set<CourseNo>;;

// BasicNaturalScience
let BNSmandatory: Mandatory = Set.ofList [1 ; 2 ; 3];
let BNSOptional: Optional = Set.ofList [4 ; 5 ; 6];
let BNScourseGroup: BasicNaturalScience = (BNSmandatory, BNSOptional);
// TechnologicalCore
let TCmandatory: Mandatory = Set.ofList [7 ; 8 ; 9];
let TCOptional: Optional = Set.ofList [10 ; 11 ; 12];
let TCcourseGroup: TechnologicalCore = (TCmandatory, TCOptional);
// ProjectProfessionalSkill
let PPSmandatory: Mandatory = Set.ofList [13 ; 14 ; 15];
let PPSOptional: Optional = Set.ofList [16 ; 17 ; 18];
let PPScourseGroup: BasicNaturalScience = (PPSmandatory, PPSOptional);
// Elective 
let elective courseNum = courseNum < 22;

let flagModel: FlagModel = (BNScourseGroup, TCcourseGroup, PPScourseGroup, elective);
courseBase;;
flagModel

let isValid (BNScourseGroup, TCcourseGroup, PPScourseGroup, elective) courseBase =
    let BNSisValid = isValidCourseGroup BNScourseGroup courseBase
    let TCisValid = isValidCourseGroup TCcourseGroup courseBase
    let PPSisValid = isValidCourseGroup PPScourseGroup courseBase
    BNSisValid && TCisValid && PPSisValid;;

let isValid2 ((set1, set2, set3, elect): FlagModel) (courseBase: CourseBase) : bool = 
    let fullList = [set1 ; set2 ; set3 ] : CourseGroup list
    let rec matchFullList setList courseBase = 
        match setList with
        |(headMan,headOpt)::tail -> let isItValid = matchFullList tail courseBase
                                    let isCGValid1 = isValidCourseGroup headMan courseBase
                                    let isCGValid2 = isValidCourseGroup headOpt courseBase
                                    isItValid && isCGValid1 && isCGValid2
        | _ -> true 
    matchFullList fullList courseBase;;
    

isValid2 flagModel courseBase;;

let list1 = [ BNScourseGroup ; TCcourseGroup ; PPScourseGroup ] : CourseGroup list ;;
list1;;
isValidCourseGroup courseGroup2 courseBase;;



snd (Option.get(Map.tryFind 1 courseBase));;
not (mandatory.Contains 2131)
Set.isSubset mandatory2 mandatory;;

Set.forall (fun x -> x >= 2) mandatory;;
Map.forall (fun _ (_,p) -> p % 5 = 0) courseBase;;

// Part 5 of the exercise on polynimials.
