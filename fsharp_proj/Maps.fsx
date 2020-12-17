

let msMap1 = Map.ofList [("b",3); ("a",5); ("c",1)];;
let msMap2 = Map.ofList [("b",3); ("a",5); ("c",1) ; ("c",1)];;
msMap1;;
msMap2;;
msMap1.Add ("a", 2)

let insertMap valIns num ms  = 
    if (Map.containsKey valIns ms) then
        let y = Map.find valIns ms
        Map.add valIns (y+num) ms 
    else 
        Map.add valIns num ms;;

insertMap "e" 2 msMap1;;  
insertMap "a" 2 msMap1;;  

let unionMap ms1 ms2 = 
    Map.foldBack (fun a b accu -> insertMap a b accu ) ms1 ms2;;
    

unionMap (Map.ofList [("b",3)])  (Map.ofList [("a",1)]) ;;
unionMap (Map.ofList [("b",3)])  (Map.ofList []) ;;
unionMap (Map.ofList [])  (Map.ofList [("a",1)]) ;;
unionMap (Map.ofList [("b",3); ("a",5); ("d",1)])  (Map.ofList [("b",3); ("a",5); ("d",1)]) ;;