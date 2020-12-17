open Microsoft.FSharp.Collections

(*
The rst step is to revise the declaration for Element (and FileSys) so
that a le is characterized by a name and an extension, where both are strings. For example,
File("a2","fsx") should be a value of type Element denoting a le with name a2 having
the extension fsx, and the following should be a valid declaration
*)

type FileSys = Element list
and Element = | File of string * string
              | Dir of string * FileSys;;

let d1 = Dir("d1",[File("a1","java");
Dir("d2", [File("a2","fsx");
Dir("d3", [File("a3","fs")])]);
File("a4","fsx");
Dir("d3", [File("a5","pdf")])]);;

(*
ListOfNames: Revise the functions namesFileSys and namesElement so that they extract the
list of all le names (with extensions) and names of directories occurring in le systems
and elements, respectively. For example: the name of the le File("a2","fsx") is
the string "a2.fsx" and
namesElement d1;;
val it : string list = ["d1"; "a1.java"; "d2"; "a2.fsx"; "d3";
"a3.fs"; "a4.fsx"; "d3"; "a5.pdf"]
The order in which the strings occur in the list is of no importance.
*)

let rec namesFileSys = function
    | [] -> []
    | e::es -> (namesElement e) @ (namesFileSys es)
and namesElement = function
    | File (s, exs) -> [s + "." + exs ] 
    | Dir(s, fs) -> s :: (namesFileSys fs);;

namesElement d1;;

(*
search: Declare two functions searchFileSys ext lesys and searchElement ext elem in mu-
tual recursion that can extract the set of all le names having extension ext in a le
system or element, respectively. Just sets of le names without extensions are returned
by the two functions, e.g.:
searchElement "fsx" d1;;
val it : Set<string> = set ["a2"; "a4"]
*)
let rec searchFileSys (ext: string) = function
    | [] -> Set.empty
    | e::es -> Set.union (searchElement ext e) (searchFileSys ext es)
and searchElement ext = function
    | File (s, exs) when exs = ext -> Set.empty.Add s 
    | File (_, _) -> Set.empty
    | Dir (_, fs) -> searchFileSys ext fs;;

searchElement "fsx" d1;;
searchElement "java" d1;;

(*
longNames: Declare mutually recursive functions:
longNamesFileSys: FileSys -> Set<string>
longNamesElement: Element -> Set<string>
to extract the set of so-called long le names of all les occurring in le systems and
elements, respectively. A long le name is a string consisting of a path part and a le
name. It has the form dir 1ndir 2n    dirnnname:ext, when the le name with extension
ext is in the subdirectory named dirn and dir i+1 is a subdirectory of dir i, for 1  i < n.
If a le is an element of the top-most le system, then the corresponding path part is
the empty string. For example, the long name for the le a3:fs in the above element
d1 is the string "d1nd2nd3na3:fs". (Note that the backslash character is written using
the escape sequence "\\".)
*)
let addPath path = function 
    | Dir(s, fs) -> Dir(path + "\\" + s , fs)
    | File(s, ext) -> File(path + "\\" + s, ext);;
    
let rec longNamesFileSys es =
    Set.foldBack (fun e res -> Set.union (longNamesElement e) res) es Set.empty
and longNamesElement = function
    | Dir (s, fs) -> longNamesFileSys ((Set.ofList fs) |> Set.map (addPath s))
    | File (s, ext) -> Set.singleton (s + "." + ext) ;;
    
longNamesElement d1;;
    

