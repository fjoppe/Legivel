open System
open System.Text
open System.IO
open System.Text.RegularExpressions

#I "."
#I "../../packages"
#r @"bin\Debug\YamlParser.dll"
#r @"NUnit\lib\net45\nunit.framework.dll"
#r @"FsUnit\lib\net45\FsUnit.NUnit.dll"


open RegexDSL
open YamlParse
open TagResolution
open NUnit.Framework
open FsUnit

//  Tests
let engine = Yaml12Parser()


let blockFold s n =
    let ps = 
        ParseState.Create "" TagResolution.FailsafeSchema 
        |> ParseState.SetIndent n 
        |> ParseState.SetStyleContext ``Block-in``

    let lines = s |> engine.``split by linefeed``

    let IsTrimmable s = IsMatch(s, RGSF((engine.``s-line-prefix`` ps) ||| (engine.``s-indent(<n)`` ps)))
    let IsSpacedText s = IsMatch(s, RGSF(engine.``s-nb-spaced-text`` ps))

    let skipIndent s = 
        if IsMatch(s, RGS(engine.``s-indent(n)`` ps)) then s.Substring(n)
        else failwith (sprintf "Problem with indentation: %s" s)
    let unIndent s = if s <> "" then skipIndent s else s

    let rec trimLines inLines outLines noFold =
        let result nf = 
            if nf then 
                inLines, outLines
            else
                inLines, outLines |> List.tail
            
        match inLines with
        |   []          -> result noFold
        |   h :: rest   ->
            if IsTrimmable h then
                printfn "trim"
                trimLines rest (h.Trim() :: outLines) noFold
            else    // non empty
                result (noFold || IsSpacedText h)

    let rec foldEverything inLines outLines folded =
        match inLines with
        |   []          -> outLines |> List.rev
        |   h :: rest   ->
            if outLines.Length = 0 then
                printfn "initial"
                foldEverything rest (h :: outLines) false
            else
                if IsTrimmable h then
                    let (inl, outl) = trimLines inLines outLines (IsSpacedText outLines.Head || rest.Length = 0)
                    foldEverything inl outl true
                else    // non empty
                    printfn "nonempty: '%s'" h
                    let prev = List.head outLines
                    if not(IsSpacedText h) && not(folded) && prev <> "" then
                        let foldedout = prev + " " + (skipIndent h) :: List.tail outLines
                        foldEverything rest foldedout false
                    else
                       foldEverything rest (h :: outLines) false
                    
    let res = foldEverything lines [] false |> List.map(fun s -> unIndent s)
    String.Join("\n", res)


blockFold "  trimmed
  
 

  as
  space" 2 |> should equal "trimmed\n\n\nas space"

blockFold "  foo \n \n  \t bar\n\n  baz\n" 2 |> should equal "foo \n\n\t bar\n\nbaz\n"

blockFold "
 folded
 line

 next
 line
   * bullet

   * list
   * lines

 last
 line
" 1 |> should equal "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n"



// =======================================================

//  Literal style

let trimIndents s n =
    let pst = 
        ParseState.Create "" TagResolution.FailsafeSchema 
        |> ParseState.SetIndent n 
        |> ParseState.SetStyleContext ``Block-in``

    let slist = s |> engine.``split by linefeed``

    let skipIndent s = 
        if IsMatch(s, RGS(engine.``s-indent(n)`` pst)) then s.Substring(n)
        else failwith (sprintf "Problem with indentation: %s" s)
    let unIndent s = if s <> "" then skipIndent s else s

    let ``l-empty`` = RGSF((engine.``s-line-prefix`` (pst.SetStyleContext ``Block-in``)) ||| (engine.``s-indent(<n)`` pst))
    let ``l-literaltext`` = RGSF((engine.``s-indent(n)`` pst) + OOM(engine.``nb-char``))

    let trimTail sin sout =
        printfn "trimTail"
        match sin with
        |   []  -> sout |> List.rev
        |   h :: rest ->
            printfn "cl: '%s'" h
            let patt = engine.``l-chomped-empty`` pst + RGP("\\z")
            if (h="") || IsMatch(h, patt) then sout |> List.rev
            else raise (ParseException (sprintf "Unexpected characters"))

    let rec trimMain sin sout =
        printfn "trimMain"
        match sin with
        |   []  -> sout |> List.rev
        |   h :: rest ->
            printfn "cl: '%s'" h
            if (h="") then 
                trimMain rest (unIndent h :: sout)
            else
                match h with
                |   Regex(``l-empty``)       _ -> trimMain rest (unIndent h :: sout)
                |   Regex(``l-literaltext``) _ -> trimMain rest (unIndent h :: sout)
                |   _ -> trimTail sin sout

    let rec trimHead sin sout =
        printfn "trimHead"
        match sin with
        |   []  -> sout |> List.rev
        |   h :: rest ->
            printfn "cl: '%s'" h
            if (h="") then
                trimHead rest (unIndent h :: sout)
            else
                match h with
                |   Regex(``l-empty``)  _ -> trimHead rest (unIndent h :: sout)
                //  todo: more whitespace than indent -> raise
                |   _ -> trimMain sin sout
    
    trimHead slist []
    |> engine.``join lines``


trimIndents "  # text
  
 # Clip
  # comments:
" 2

trimIndents "    By four
      spaces" 4


trimIndents " literal
 \ttext
" 1





let ps = 
    ParseState.Create "" TagResolution.FailsafeSchema 
    |> ParseState.SetIndent 2 
    |> ParseState.SetStyleContext ``Block-in``

let p1 = RGSF(engine.``l-empty`` (ps.SetStyleContext ``Block-in``))
let p2 = RGSF((engine.``s-indent(n)`` ps) + OOM(engine.``nb-char``))
let p3 = RGSF(((engine.``s-line-prefix`` ps) ||| (engine.``s-indent(<n)`` ps)))
IsMatch("  ", p3)
