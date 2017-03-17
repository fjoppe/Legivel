open System
open System.Text
open System.IO
open System.Text.RegularExpressions

#I "."
#r @"bin\Debug\YamlParser.dll"

open RegexDSL
open YamlParse
open YamlParser
open RepresentationGraph

let engine = Yaml12Parser()

#load "nlog.fsx"
NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let YamlParse s =
    let ``s-l+block-node`` s = 
        let ps = ParseState.Create s
        let ps = ps.SetIndent -1
        let ps = ps.SetSubIndent 0
        let ps = ps.SetStyleContext ``Block-in``
        let d = engine.``s-l+block-node`` ps 
        d
    try
        let rs = (``s-l+block-node`` s)
        let rsp = rs.Value |> fst
        printfn "%s" (rsp.ToCanonical(0))
        rsp
    with
    | e -> printfn "%A" e; raise e


let ToScalar n = 
    match n with
    |   Some([ScalarNode nd]) -> nd.Data
    |   _ -> raise (Exception "Is no scalar")

let pth = YamlPath.Create "//#'scalar'"

YamlParse "\"not found\"" |> pth.Select |> Option.isNone
YamlParse "\"scalar\""    |> pth.Select |> ToScalar
YamlParse "'scalar'"      |> pth.Select |> ToScalar
YamlParse "scalar"        |> pth.Select |> ToScalar


YamlParse "[ a, b ]"
YamlParse "{ a: b }"


YamlParse "- simple\n- text\n- [ testing, one, two, three ]"



