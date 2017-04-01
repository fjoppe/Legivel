module TestStuctures

open NUnit.Framework
open System
open YamlParser
open YamlParse
open FsUnit
open RepresentationGraph
open TagResolution
open Deserialization

let YamlParse s =
    let engine = Yaml12Parser()
    let ``s-l+block-node`` s = 
        let ps = ParseState.Create s YamlCoreSchema
        let ps = ps.SetIndent -1
        let ps = ps.SetSubIndent 0
        let ps = ps.SetStyleContext ``Block-in``
        let d = engine.``s-l+block-node`` ps 
        d
    try
        let rs = (``s-l+block-node`` s)
        let rsp = rs.Value |> fst
        let ps = rs.Value |> snd
        printfn "%s" (Deserialize rsp (ps.TagShorthands))
        rsp
    with
    | e -> printfn "%A" e; raise e


let ToScalar n = 
    match n with
    |   Some([ScalarNode nd]) -> nd.Data
    |   _ -> raise (Exception "Is no scalar")

let ToScalarTag n = 
    match n with
    |   Some([ScalarNode nd]) -> nd.Tag.Uri
    |   _ -> raise (Exception "Is no scalar")

[<Test>]
let ``Test Scalars at Root Level - Sunny Day Simple``() =
    let pth = YamlPath.Create "//#'scalar'"

    YamlParse "\"not found\"" |> pth.Select |> should equal None
    YamlParse "\"scalar\""    |> pth.Select |> ToScalar |> should equal "scalar"
    YamlParse "'scalar'"      |> pth.Select |> ToScalar |> should equal "scalar"
    YamlParse "scalar"        |> pth.Select |> ToScalar |> should equal "scalar"

[<Test>]
let ``Test Scalars at Root Level - Rainy Day Simple``() =
    let pth = YamlPath.Create "//#'scalar'"
    YamlParse "[a, b]" |> pth.Select |> should equal None 
    YamlParse "{a: b}" |> pth.Select |> should equal None 

[<Test>]
let ``Test Seq at Root Level - Sunny Day Simple``() =
    let pth = YamlPath.Create "//[]/#'a'"
    YamlParse "[ a, b ]" |> pth.Select |> ToScalar |> should equal "a"
    
[<Test>]
let ``Test Seq at Root Level - Rainy Day Simple``() =
    let pth = YamlPath.Create "//[]/#'a'"
    YamlParse "[ not, found ]" |> pth.Select |> should equal None 

[<Test>]
let ``Test Map key at Root Level - Sunny Day Simple``() =
    let pth = YamlPath.Create "//{#'a'}"
    YamlParse "{ a: b }" |> pth.Select |> ToScalar |> should equal "a"

[<Test>]
let ``Test Map value at Root Level - Sunny Day Simple``() =
    let pth = YamlPath.Create "//{#'a'}?"
    YamlParse "{ a: b }" |> pth.Select |> ToScalar |> should equal "b" 

[<Test>]
let ``Test Map key at Root Level - Rainy Day Simple``() =
    let pth = YamlPath.Create "//{#'a'}"
    YamlParse "[ a, b ]" |> pth.Select |> should equal None
    YamlParse "a" |> pth.Select |> should equal None

[<Test>]
let ``Test Hybrid Seq with Seq at Root Level - Sunnny Day Simple``() =
    let yml = YamlParse "- simple\n- text\n- [ testing, one, two, three ]"

    let pth1 = YamlPath.Create "//[]/#'simple'"
    yml |> pth1.Select |> ToScalar |> should equal "simple"

    let pth2 = YamlPath.Create "//[]/#'text'"
    yml |> pth2.Select |> ToScalar |> should equal "text"

    let pth3 = YamlPath.Create "//[]/[]/#'one'"
    yml |> pth3.Select |> ToScalar |> should equal "one"

[<Test>]
let ``Test Hybrid Seq with Map at Root Level - Sunnny Day Simple``() =
    let yml = YamlParse "- simple\n- text\n- { testing: 0, one: 1, two: 2, three : 3 }"

    let pth1 = YamlPath.Create "//[]/#'simple'"
    yml |> pth1.Select |> ToScalar |> should equal "simple"

    let pth2 = YamlPath.Create "//[]/#'text'"
    yml |> pth2.Select |> ToScalar |> should equal "text"

    let pth3 = YamlPath.Create "//[]/{#'one'}"
    yml |> pth3.Select |> ToScalar |> should equal "one"

    let pth4 = YamlPath.Create "//[]/{#'three'}?"
    yml |> pth4.Select |> ToScalar |>  should equal "3"

[<Test>]
let ``Test Map Hybrid Notation - Sunnny Day Simple``() =
    let yml = YamlParse "{\"adjacent\":value1, \"readable\": value2,  \"empty\":}"

    let pth = YamlPath.Create "//{#'adjacent'}?"
    yml |> pth.Select |> ToScalarTag  |>  should equal TagResolution.Failsafe.StringGlobalTag.Uri
    yml |> pth.Select |> ToScalar |>  should equal "value1"

    let pth = YamlPath.Create "//{#'readable'}?"
    yml |> pth.Select |> ToScalarTag |> should equal TagResolution.Failsafe.StringGlobalTag.Uri
    yml |> pth.Select |> ToScalar |> should equal "value2"

    let pth = YamlPath.Create "//{#'empty'}?"
    yml |> pth.Select |> ToScalarTag |> should equal TagResolution.JSON.NullGlobalTag.Uri

[<Test>]
let ``Test Map with inline Comments - Sunnny Day Simple``() =
    let pth = YamlPath.Create "//{#'key'}?"
    YamlParse "key:    # Comment\n  value" |> pth.Select |> ToScalar |> should equal "value"
    YamlParse "key:    # Comment\n        # lines\n  value\n\n" |> pth.Select |> ToScalar |> should equal "value"

[<Test>]
let ``Test Map Null : Null - Sunnny Day Simple``() =
    let ptk = YamlPath.Create "//{#''}"
    let ptv = YamlPath.Create "//{#''}?"
    let yml = YamlParse ":"
    yml |> ptk.Select |> ToScalarTag |> should equal TagResolution.JSON.NullGlobalTag.Uri
    yml |> ptv.Select |> ToScalarTag |> should equal TagResolution.JSON.NullGlobalTag.Uri

[<Test>]
let ``Test Map key : Map - Sunnny Day Simple``() =
    let pt1 = YamlPath.Create "//{#'mainkey'}"
    let pt2 = YamlPath.Create "//{#'mainkey'}?/{#'key'}?"
    let yml = YamlParse "mainkey:\n key: value\n"

    yml |> pt1.Select |> ToScalar |> should equal "mainkey"
    yml |> pt2.Select |> ToScalar |> should equal "value"

[<Test>]    //  http://www.yaml.org/spec/1.2/spec.html#id2780810
let ``Test Map with inline seperation lines - Sunnny Day Simple``() =
    let yml = YamlParse "{ first: Sammy, last: Sosa }:\n# Statistics:\n  hr:  # Home runs\n     65\n  avg: # Average\n   0.278"

    let pt1 = YamlPath.Create "//{}/{#'first'}?"
    yml |> pt1.Select |> ToScalar  |> should equal "Sammy"

    let pt2 = YamlPath.Create "//{}/{#'last'}?"
    yml |> pt2.Select |> ToScalar |> should equal "Sosa"

    let pt3 = YamlPath.Create "//{}?/{#'hr'}?"
    yml |> pt3.Select |> ToScalar |> should equal "65"

    let pt4 = YamlPath.Create "//{}?/{#'avg'}?"
    yml |> pt4.Select |> ToScalar |> should equal "0.278"

[<Test>]
let ``Test Map indented implicit entries - Sunnny Day Simple``() =
    let yml = YamlParse "  hr:  # Home runs\n     65\n  avg: # Average\n   0.278"
    let pt3 = YamlPath.Create "//{#'hr'}?"
    yml |> pt3.Select |> ToScalar |> should equal "65"

    let pt4 = YamlPath.Create "//{#'avg'}?"
    yml |> pt4.Select |> ToScalar |> should equal "0.278"

[<Test>]
let ``Test Map indented implicit entries and comments - Sunnny Day Simple``() =
    let yml = YamlParse "\n# Statistics:\n  hr:  # Home runs\n     65\n  avg: # Average\n   0.278"
    let pt3 = YamlPath.Create "//{#'hr'}?"
    yml |> pt3.Select |> ToScalar |> should equal "65"

    let pt4 = YamlPath.Create "//{#'avg'}?"
    yml |> pt4.Select |> ToScalar |> should equal "0.278"

[<Test>]    //  http://www.yaml.org/spec/1.2/spec.html#id2797382
let ``Test Map implicit entries with indented seq value - Sunnny Day Simple``() =
    let yml = YamlParse "block sequence:\n  - one\n  - two : three\n"
    let pt1 = YamlPath.Create "//{#'block sequence'}?/[]/#'one'"
    yml |> pt1.Select |> ToScalar |> should equal "one"

    let pt2 = YamlPath.Create "//{#'block sequence'}?/[]/{#'two'}?"
    yml |> pt2.Select |> ToScalar |> should equal "three"

[<Test>]    //  http://www.yaml.org/spec/1.2/spec.html#id2787109
let ``Test Map Double Quoted style - Sunnny Day Simple``() =
    let yml = YamlParse "\"implicit block key\" : [\n  \"implicit flow key\" : value,\n ]"
    let pt = YamlPath.Create "//{#'implicit block key'}?/[]/{#'implicit flow key'}?"
    yml |> pt.Select |> ToScalar |> should equal "value"

[<Test>]    //  http://www.yaml.org/spec/1.2/spec.html#id2788496
let ``Test Map Single Quoted style - Sunnny Day Simple``() =
    let yml = YamlParse "'implicit block key' : [\n  'implicit flow key' : value,\n ]"
    let pt = YamlPath.Create "//{#'implicit block key'}?/[]/{#'implicit flow key'}?"
    yml |> pt.Select |> ToScalar |> should equal "value"


    