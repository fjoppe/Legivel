open System
open System.Text
open System.IO
open System.Text.RegularExpressions
open TagResolution

#I "."
#r @"bin\Debug\YamlParser.dll"

open RegexDSL
open YamlParse


//  Tests
let engine = Yaml12Parser()

let ``s-l+block-node`` s = 
    let ps = ParseState.Create s YamlCoreSchema
    let ps = ps.SetIndent -1
    let ps = ps.SetSubIndent 0
    let ps = ps.SetStyleContext ``Block-in``
    let d = engine.``s-l+block-node`` ps 
    d


let ``s-l+block-collection`` s = 
    let ps = ParseState.Create s YamlCoreSchema
    let ps = ps.SetIndent 0
    let ps = ps.SetSubIndent 0
    let ps = ps.SetStyleContext ``Block-in``
    let d = engine.``s-l+block-collection`` ps 
    d


#load "nlog.fsx"
NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let YamlParse s =
    try
        let pr = (``s-l+block-node`` s).Value 
        let tr = pr |> snd
        let short (s:string) = if s.Length > 10 then s.Substring(0, 10) else s
        tr.TraceSuccess  |> List.iter(fun (s,ps) -> printfn "%s\t\"%s\"" s (short (ps.InputString.Replace("\n","\\n"))))
        let rs = pr |> fst
        printfn "%s" (rs.ToCanonical(0))
    with
    | e -> printfn "%A" e



//YamlParse "a:\n key: value\n"
//
//YamlParse "plain key: in-line value\n: # Both empty\n\"quoted key\":- entry"

//YamlParse ":"
//
//YamlParse "{ first: Sammy, last: Sosa }:\n# Statistics:\n  hr:  # Home runs\n     65\n  avg: # Average\n   0.278"
//
//YamlParse "  hr:  # Home runs\n     65\n  avg: # Average\n   0.278"
//
//YamlParse "\n# Statistics:\n  hr:  # Home runs\n     65\n  avg: # Average\n   0.278"

//  http://www.yaml.org/spec/1.2/spec.html#id2797382
//YamlParse "block sequence:\n  - one\n  - two : three\n"

//  http://www.yaml.org/spec/1.2/spec.html#id2787109
//YamlParse "\"implicit block key\" : [\n  \"implicit flow key\" : value,\n ]"

//  http://www.yaml.org/spec/1.2/spec.html#id2788496
//YamlParse "'implicit block key' : [\n  'implicit flow key' : value,\n ]"
//
//YamlParse "
//Mark McGwire: {hr: 65, avg: 0.278}
//Sammy Sosa: {
//    hr: 63,
//    avg: 0.288
//  }"



