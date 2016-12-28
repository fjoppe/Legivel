open System
open System.Text
open System.IO
open System.Text.RegularExpressions

#I "."
#r @"bin\Debug\YamlParser.dll"

open YamlParse


//  Tests
let engine = FlowCollectionStyles()

//type PrevType = EmptyFolded | Empty | Indented | Normal

//let foldlines n c str =
//    let ws2 = GRP(ZOM(engine.``s-white``)) + GRP(ZOM(engine.``ns-char``))
//    let rec doFold prev (res:string) (lst: string list) =
//        match lst with
//        |   []  -> res
//        |   curr :: tail ->
//            match curr with
//            |   Regex2(ws2) mt -> 
//                let (w, c) = mt.ge2
//                if c = "" then
//                    match prev with
//                    | Empty     ->
//                        if w.Length <= n then doFold Empty (res + "\n") tail
//                        else doFold Indented (res + "" + curr.Substring(n) ) tail
//                    | Indented  ->
//                        if w.Length <= n then doFold Empty (res + "\n\n") tail
//                        else doFold Indented (res + "\n\n" + curr.Substring(n)) tail
//                    | Normal    ->
//                        if w.Length <= n then doFold Empty (res + "\n") tail
//                        else doFold Indented (res + "\n" + curr.Substring(n)) tail
//                else
//                    match prev with
//                    | Empty     ->
//                        if w.Length = n then doFold Normal (res + curr.Substring(n)) tail
//                        else
//                        if w.Length > n then doFold Indented (res + "\n"+ curr.Substring(n)) tail
//                        else raise (ParseException(sprintf "Incorrect indentation at: '%s'" curr))
//                    | Indented  ->
//                        if w.Length = n then doFold Normal (res + "\n"+ curr.Substring(n)) tail
//                        else
//                        if w.Length > n then doFold Indented (res + "\n"+ curr.Substring(n)) tail
//                        else raise (ParseException(sprintf "Incorrect indentation at: '%s'" curr))
//                    | Normal    ->
//                        if w.Length = n then doFold Normal (res + " " + curr.Substring(n)) tail
//                        else
//                        if w.Length > n then doFold Indented (res + "\n"+ curr.Substring(n)) tail
//                        else raise (ParseException(sprintf "Incorrect indentation at: '%s'" curr))
//            |   _ -> raise (ParseException(sprintf "Incorrect pattern: '%s'" curr))
//    let stripAll lst = lst |> List.rev |> List.skipWhile(fun s -> String.IsNullOrWhiteSpace(s)) |> List.rev
//    match c with
//    | ``Strip`` -> str |> stripAll |> doFold Empty ""
//    | ``Clip``  -> if (String.IsNullOrWhiteSpace(List.last str)) then 
//                        List.append (str |> stripAll) [""] |> doFold Empty ""
//                   else 
//                        str |> doFold Empty ""
//    | ``Keep``  -> str |> doFold Empty ""


//let foldlines2 n c str =
//    let ws2 = GRP(ZOM(engine.``s-white``)) + GRP(ZOM(engine.``ns-char``))
//    let rec doFold foldChar (res:string) (lst: string list) =
//        match lst with
//        |   []  -> res
//        |   curr :: tail ->
//            match curr with
//            |   Regex2(ws2) mt -> 
//                let (w, c) = mt.ge2
//                if c = "" then
//                    let extranl = "" // if foldChar = "\n" then "\n" else ""
//                    if w.Length <= n then doFold "" (res + extranl + "\n") tail
//                    else doFold "\n" (res + "\n" + curr.Substring(n)) tail
//                else
//                        if w.Length = n then doFold " " (res + foldChar + curr.Substring(n)) tail
//                        else
//                        if w.Length > n then doFold "\n" (res + "\n"+ curr.Substring(n)) tail
//                        else raise (ParseException(sprintf "Incorrect indentation at: '%s'" curr))
//            |   _ -> raise (ParseException(sprintf "Incorrect pattern: '%s'" curr))
//    let stripAll lst = lst |> List.rev |> List.skipWhile(fun s -> String.IsNullOrWhiteSpace(s)) |> List.rev
//    match c with
//    | ``Strip`` -> str |> stripAll |> doFold "" ""
//    | ``Clip``  -> if (String.IsNullOrWhiteSpace(List.last str)) then 
//                        List.append (str |> stripAll) [""] |> doFold "" ""
//                   else 
//                        str |> doFold "" ""
//    | ``Keep``  -> str |> doFold "" ""

//
//let foldlines3 n c str =
//    let ws2 = GRP(ZOM(engine.``s-white``)) + GRP(ZOM(engine.``ns-char``))
//    let rec doFold prev (res:StringBuilder)  (lst: string list) =
//        match lst with
//        |   []  -> 
//            match prev with
//            | Empty    
//            | EmptyFolded     -> res.ToString()
//            | _ -> if res.ToString().EndsWith("\n") then res.Remove(res.Length-1,1).ToString() else res.ToString()
//        |   curr :: tail ->
//            match curr with
//            |   Regex2(ws2) mt -> 
//                printfn "%s" (res.ToString().Replace("\n","\\n"))
//                let (w, c) = mt.ge2
//                if c = "" then
//                    match prev with
//                    | EmptyFolded     ->
//                        if w.Length <= n then doFold Empty (res.Append("\n")) tail
//                        else doFold Indented (res.Append("\n").Append(curr.Substring(n)).Append("\n")) tail
//                    | Empty     ->
//                        if w.Length <= n then doFold Empty (res.Append("\n")) tail
//                        else doFold Indented (res.Append(curr.Substring(n)).Append("\n")) tail
//                    | Indented  ->
//                        if w.Length <= n then doFold Empty (res.Append("\n")) tail
//                        else doFold Indented (res.Append(curr.Substring(n)).Append("\n")) tail
//                    | Normal    ->
//                        if w.Length <= n then doFold EmptyFolded (res) tail
//                        else doFold Indented (res.Append(curr.Substring(n)).Append("\n")) tail
//                else
//                    match prev with
//                    | EmptyFolded   ->
//                        if w.Length = n then doFold Normal (res.Append(curr.Substring(n)).Append("\n")) tail
//                        else
//                        if w.Length > n then doFold Indented (res.Append("\n").Append(curr.Substring(n)).Append("\n")) tail
//                        else raise (ParseException(sprintf "Incorrect indentation at: '%s'" curr))
//                    | Empty         ->
//                        if w.Length = n then doFold Normal (res.Append(curr.Substring(n)).Append("\n")) tail
//                        else
//                        if w.Length > n then doFold Indented (res.Append(curr.Substring(n)).Append("\n")) tail
//                        else raise (ParseException(sprintf "Incorrect indentation at: '%s'" curr))
//                    | Indented  ->
//                        if w.Length = n then doFold Normal (res.Append(curr.Substring(n)).Append("\n")) tail
//                        else
//                        if w.Length > n then doFold Indented (res.Append(curr.Substring(n)).Append("\n")) tail
//                        else raise (ParseException(sprintf "Incorrect indentation at: '%s'" curr))
//                    | Normal    ->
//                        if w.Length = n then 
//                            let res = if res.ToString().EndsWith("\n") then res.Remove(res.Length-1,1).Append(" ") else res
//                            doFold Normal (res.Append(curr.Substring(n)).Append("\n") ) tail
//                        else
//                        if w.Length > n then doFold Indented (res.Append(curr.Substring(n)).Append("\n")) tail
//                        else raise (ParseException(sprintf "Incorrect indentation at: '%s'" curr))
//            |   _ -> raise (ParseException(sprintf "Incorrect pattern: '%s'" curr))
//    let stripAll lst = lst |> List.rev |> List.skipWhile(fun s -> String.IsNullOrWhiteSpace(s)) |> List.rev
//    match c with
//    | ``Strip`` -> str |> stripAll |> doFold Empty (new StringBuilder())
//    | ``Clip``  -> if (String.IsNullOrWhiteSpace(List.last str)) then 
//                        List.append (str |> stripAll) [""] |> doFold Empty (new StringBuilder())
//                   else 
//                        str |> doFold Empty (new StringBuilder())
//    | ``Keep``  -> str |> doFold Empty (new StringBuilder())

type PrevType2 = EmptyAfterFolded | Empty | Indented | TextLine

let foldlines4 n c str =
    let ws2 = GRP(ZOM(engine.``s-white``)) + GRP(ZOM(engine.``ns-char``))
    let rec doFold prev (res:StringBuilder)  (lst: string list) =
        match lst with
        |   []  -> 
            match prev with
            | Empty | EmptyAfterFolded   -> res.ToString()
            | _ -> if res.ToString().EndsWith("\n") then res.Remove(res.Length-1,1).ToString() else res.ToString()
        |   curr :: tail ->
            match curr with
            |   Regex2(ws2) mt -> 
                let standardContentAppedAndContinue mode (res:StringBuilder) = doFold mode (res.Append(curr.Substring(n)).Append("\n")) tail
                printfn "%s" (res.ToString().Replace("\n","\\n"))
                let (w, c) = mt.ge2
                if c = "" then
                    if w.Length > n then standardContentAppedAndContinue Indented res
                    else
                        match prev with
                        | Empty | Indented | EmptyAfterFolded  -> doFold Empty (res.Append("\n")) tail
                        | TextLine          -> doFold EmptyAfterFolded (res)   tail
                else
                    if w.Length < n then raise (ParseException(sprintf "Incorrect indentation at: '%s'" curr))
                    else 
                        if w.Length > n then
                            match prev with
                            | Empty | Indented | TextLine          -> standardContentAppedAndContinue Indented res
                            | EmptyAfterFolded  -> standardContentAppedAndContinue Indented (res.Append("\n"))
                        else    //  w.Length = n 
                            match prev with
                            | Empty | Indented | EmptyAfterFolded  -> standardContentAppedAndContinue TextLine res
                            | TextLine          ->
                                let res = if res.ToString().EndsWith("\n") then res.Remove(res.Length-1,1).Append(" ") else res
                                doFold TextLine (res.Append(curr.Substring(n)).Append("\n") ) tail
            |   _ -> raise (ParseException(sprintf "Incorrect pattern: '%s'" curr))
    let stripAll lst = lst |> List.rev |> List.skipWhile(fun s -> String.IsNullOrWhiteSpace(s)) |> List.rev
    match c with
    | ``Strip`` -> str |> stripAll |> doFold Empty (new StringBuilder())
    | ``Clip``  -> if (String.IsNullOrWhiteSpace(List.last str)) then 
                        List.append (str |> stripAll) [""] |> doFold Empty (new StringBuilder())
                   else 
                        str |> doFold Empty (new StringBuilder())
    | ``Keep``  -> str |> doFold Empty (new StringBuilder())

let fold n c ms =
    let split = ms |> engine.``split by linefeed`` 
    split |> foldlines4 n c


fold 2 ``Strip`` "  trimmed\n  \n \n\n  as\n  space" = "trimmed\n\n\nas space"
fold 2 ``Clip`` "  foo \n \n  \t bar\n\n  baz\n" = "foo \n\n\t bar\n\nbaz\n"
fold 1 ``Clip`` "\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n" = "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n"

