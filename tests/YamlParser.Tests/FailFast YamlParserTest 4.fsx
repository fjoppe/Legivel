open System
open System.Text
open System.IO
open System.Text.RegularExpressions

#I "."
#r @"bin\Debug\YamlParser.dll"

open RegexDSL
open YamlParse


//  Tests
let engine = Yaml12Parser()

let ``s-l+block-node`` s = 
    let ps = ParseState.Create s
    let ps = ps.SetIndent -1
    let ps = ps.SetSubIndent 0
    let ps = ps.SetStyleContext ``Block-in``
    let d = engine.``s-l+block-node`` ps 
    d


let ``s-l+block-collection`` s = 
    let ps = ParseState.Create s
    let ps = ps.SetIndent 0
    let ps = ps.SetSubIndent 0
    let ps = ps.SetStyleContext ``Block-in``
    let d = engine.``s-l+block-collection`` ps 
    d

#load "nlog.fsx"
NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let YamlParse s =
    try
        let rs = (``s-l+block-node`` s).Value |> fst
        printfn "%s" (rs.ToCanonical(0))
    with
    | e -> printfn "%A" e



YamlParse "{ first: Sammy, last: Sosa }:\n# Statistics:\n  hr:  # Home runs\n     65\n  avg: # Average\n   0.278"


``s-l+block-collection`` "\n# Statistics:\n  hr:  # Home runs\n     65\n  avg: # Average\n   0.278"

YamlParse "  hr:  # Home runs\n     65\n"

