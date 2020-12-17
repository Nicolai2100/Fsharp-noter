type FileSys = Element list

and Element =
    | File of string * string
    | Dir of string * FileSys;;

File("a2","fsx");;

let d1 = Dir("d1",[File("a1","java"); Dir("d2", [File("a2","fsx"); Dir("d3", [File("a3","fs")])]); 
File("a4","fsx"); Dir("d3", [File("a5","pdf")])]);;

let rec namesFileSys = function
    | [] -> []
    | e::es -> (namesElement e) @ (namesFileSys es)
and namesElement = function
    | File(n,x) -> [n+"."+x]
    | Dir(s,fs) -> s :: (namesFileSys fs);;

namesElement d1

//searchFileSys ext filesys
//searchElement ext elem
let rec searchFileSys ext = function
    | [] -> Set.empty
    | e::fs -> Set.union (searchElement ext e) (searchFileSys ext fs)
and searchElement ext = function
    | File(n,x) when x = ext -> Set.empty.Add n
    | File(_) -> Set.empty
    | Dir(_,fs) -> searchFileSys ext fs;;

searchElement "java" d1
searchElement "fsx" d1
searchElement "fs" d1

//longNamesFileSys: FileSys -> Set<string>
//longNamesElement: Element -> Set<string>

let addPath path = function 
    | Dir(s, fs) -> Dir(path + "\\" + s , fs)
    | File(s, ext) -> File(path + "\\" + s, ext);;
addPath "" d1;;

let rec longNamesFileSys es =
    Set.foldBack (fun e res -> Set.union (longNamesElement e) res) es Set.empty
and longNamesElement = function
    | Dir (s, fs) -> longNamesFileSys ((Set.ofList fs) |> Set.map (addPath s))
    | File (s, ext) -> Set.singleton (s + "." + ext) ;;

longNamesElement d1;;
