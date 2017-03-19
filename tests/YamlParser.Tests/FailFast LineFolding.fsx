open System
open System.Text
open System.IO
open System.Text.RegularExpressions

#I "."
#r @"bin\Debug\YamlParser.dll"

open RegexDSL
open YamlParse
open TagResolution

//  Tests
let engine = Yaml12Parser()

type PrevType = EmptyFolded | Empty | Indented | Normal

let ``dquote flow fold lines`` str =
    let ws2 = GRP(ZOM(engine.``s-white``)) + GRP(ZOM(engine.``ns-char``))
    let linefold (res:StringBuilder) = if res.ToString().EndsWith("\n") then res.Remove(res.Length-1,1).Append(" ") else res
    let rec doFold fst prev (res:StringBuilder) (lst: string list) =
        match lst with
        |   []  -> 
            let res = if res.ToString().EndsWith("\n") then res.Remove(res.Length-1,1) else res
            res.ToString()
        |   curr :: tail ->
            let currStr = 
                if fst then curr.TrimEnd()
                else if tail.Length = 0 then curr.TrimStart()
                else curr.Trim()
            let currType = 
                if (currStr.Length) = 0 then 
                    if fst then Normal
                    else Empty
                else Normal
            printf "%s\t\t" (res.ToString().Replace("\n","\\n"))
            printfn "'%s' - %d - (%A,%A)" currStr (currStr.Length) prev currType 
            match (prev, currType) with
            | (Empty, Empty)    -> doFold false currType (res.Append("\n")) tail
            | (Empty, Normal)   -> doFold false currType (res.Append(currStr).Append("\n")) tail
            | (Normal,Normal)   -> doFold false currType ((linefold res).Append(currStr).Append("\n")) tail
            | (Normal, Empty)   -> doFold false currType (res) tail
            |   _ -> raise (ParseException(sprintf "Incorrect pattern: '%s'" currStr))
    let stripAll lst = lst |> List.rev |> List.skipWhile(fun s -> String.IsNullOrWhiteSpace(s)) |> List.rev
    doFold true Empty (StringBuilder()) str


let ``dquote flow fold lines2`` ps str =
    let rec doFold fst prev (res : string) (lst: string list) =
        match lst with
        |   []  -> 
            let res = if res.EndsWith("\n") then res.Remove(res.Length-1) else res
            match (prev) with
            |   Empty  -> res + " "
            |   Normal -> res.ToString()
            |   _ -> raise (Exception("Bad"))
        |   curr :: tail ->
            let currStr = 
                (if fst then curr.TrimEnd() 
                else if tail.Length = 0 then curr.TrimStart()
                else curr.Trim()) + "\n"
            let currStr = 
                let SandR =
                    currStr.Split([|"\\\\"|], StringSplitOptions.RemoveEmptyEntries)
                    |>  List.ofArray
                    |>  List.map(fun s -> s.Replace("\\\n", ""))
                    |>  List.map(fun s -> s.Replace("\\ ", " "))
                    |>  List.map(fun s -> s.Replace("\\\t", "\t"))
                    |>  List.map(fun s -> s.Replace("\\\"", "\""))
                String.Join("\\", SandR)    // "\\" in dquote st is escaped, and here it is directly converted to "\"

            let currType = 
                if Regex.IsMatch(currStr, RGS(engine.``l-empty`` ps)) then 
                    if fst then Normal else Empty
                else Normal

            printfn "%s\t\t<-%s" (res.ToString().Replace("\n","\\n")) (currStr.Replace("\n","\\n"))

            match (prev) with
            |   Empty  -> doFold false currType (res+currStr) tail
            |   Normal -> 
                let res = 
                    if res.EndsWith("\n") then
                        match currType with
                        |   Empty -> res.Remove(res.Length-1)
                        |   Normal -> res.Remove(res.Length-1) + " "
                        |   _ -> raise (Exception("Bad"))
                    else res
                doFold false currType (res+currStr) tail
            |   _ -> raise (Exception("Bad"))
    let stripAll lst = lst |> List.rev |> List.skipWhile(fun s -> String.IsNullOrWhiteSpace(s)) |> List.rev
    doFold true Empty "" str


let ``flow fold`` ps ms =
    let split = ms |> engine.``split by linefeed`` 
    split |> ``dquote flow fold lines2`` ps


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

let ``bock foldlines 4`` n c str =
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

let ``block fold`` n c ms =
    let split = ms |> engine.``split by linefeed`` 
    split |> ``bock foldlines 4`` n c


``block fold`` 2 ``Strip`` "  trimmed\n  \n \n\n  as\n  space" = "trimmed\n\n\nas space"
``block fold`` 2 ``Clip`` "  foo \n \n  \t bar\n\n  baz\n" = "foo \n\n\t bar\n\nbaz\n"
``block fold`` 1 ``Clip`` "\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n" = "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n"

let getps =
    let ps = ParseState.Create "" YamlCoreSchema
    let ps = ps.SetIndent 0
    let ps = ps.SetSubIndent 0
    let ps = ps.SetStyleContext ``Block-in``
    ps 


``flow fold`` getps  " 1st non-empty\n\n 2nd non-empty \t3rd non-empty "
``flow fold`` getps "\n  foo \n \n  \t bar\n\n  baz\n"
``flow fold`` getps "folded \nto a space,\t\n \nto a line feed, or \t\\\n \\ \tnon-content"

