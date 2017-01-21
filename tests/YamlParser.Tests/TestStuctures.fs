module TestStuctures

open NUnit.Framework
open System
open System.Diagnostics
open RegexDSL
open YamlParser
open YamlParse
open RepresentationGraph


let engine = Yaml12Parser()


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

let ToScalarTag n = 
    match n with
    |   Some([ScalarNode nd]) -> nd.Tag.Short
    |   _ -> raise (Exception "Is no scalar")

[<Test>]
let ``Test Scalars at Root Level - Sunny Day Simple``() =
    let pth = YamlPath.Create "//#'scalar'"
    Assert.AreEqual(None, YamlParse "\"not found\"" |> pth.Select)
    Assert.AreEqual("scalar", YamlParse "\"scalar\""    |> pth.Select |> ToScalar)
    Assert.AreEqual("scalar", YamlParse "'scalar'"      |> pth.Select |> ToScalar)
    Assert.AreEqual("scalar", YamlParse "scalar"        |> pth.Select |> ToScalar)


[<Test>]
let ``Test Scalars at Root Level - Rainy Day Simple``() =
    let pth = YamlPath.Create "//#'scalar'"
    Assert.AreEqual(None, YamlParse "[a, b]"        |> pth.Select)
    Assert.AreEqual(None, YamlParse "{a: b}"        |> pth.Select)


[<Test>]
let ``Test Seq at Root Level - Sunny Day Simple``() =
    let pth = YamlPath.Create "//[]/#'a'"
    Assert.AreEqual("a", YamlParse "[ a, b ]" |> pth.Select |> ToScalar)

    
[<Test>]
let ``Test Seq at Root Level - Rainy Day Simple``() =
    let pth = YamlPath.Create "//[]/#'a'"
    Assert.AreEqual(None, YamlParse "[ not, found ]" |> pth.Select)


[<Test>]
let ``Test Map key at Root Level - Sunny Day Simple``() =
    let pth = YamlPath.Create "//{#'a'}"
    Assert.AreEqual("a", YamlParse "{ a: b }" |> pth.Select |> ToScalar)


[<Test>]
let ``Test Map value at Root Level - Sunny Day Simple``() =
    let pth = YamlPath.Create "//{#'a'}?"
    Assert.AreEqual("b", YamlParse "{ a: b }" |> pth.Select |> ToScalar)


[<Test>]
let ``Test Map key at Root Level - Rainy Day Simple``() =
    let pth = YamlPath.Create "//{#'a'}"
    Assert.AreEqual(None, YamlParse "[ a, b ]" |> pth.Select)
    Assert.AreEqual(None, YamlParse "a" |> pth.Select)


[<Test>]
let ``Test Hybrid Seq with Seq at Root Level - Sunnny Day Simple``() =
    let yml = YamlParse "- simple\n- text\n- [ testing, one, two, three ]"

    let pth1 = YamlPath.Create "//[]/#'simple'"
    Assert.AreEqual("simple", yml |> pth1.Select |> ToScalar)

    let pth2 = YamlPath.Create "//[]/#'text'"
    Assert.AreEqual("text", yml |> pth2.Select |> ToScalar)

    let pth3 = YamlPath.Create "//[]/[]/#'one'"
    Assert.AreEqual("one", yml |> pth3.Select |> ToScalar)


[<Test>]
let ``Test Hybrid Seq with Map at Root Level - Sunnny Day Simple``() =
    let yml = YamlParse "- simple\n- text\n- { testing: 0, one: 1, two: 2, three : 3 }"

    let pth1 = YamlPath.Create "//[]/#'simple'"
    Assert.AreEqual("simple", yml |> pth1.Select |> ToScalar)

    let pth2 = YamlPath.Create "//[]/#'text'"
    Assert.AreEqual("text", yml |> pth2.Select |> ToScalar)

    let pth3 = YamlPath.Create "//[]/{#'one'}"
    Assert.AreEqual("one", yml |> pth3.Select |> ToScalar)

    let pth4 = YamlPath.Create "//[]/{#'three'}?"
    Assert.AreEqual("3", yml |> pth4.Select |> ToScalar)


[<Test>]
let ``Test Map Hybrid Notation - Sunnny Day Simple``() =
    let yml = YamlParse "{\"adjacent\":value1, \"readable\": value2,  \"empty\":}"

    let pth = YamlPath.Create "//{#'adjacent'}?"
    Assert.AreEqual("!!str", yml |> pth.Select |> ToScalarTag)
    Assert.AreEqual("value1", yml |> pth.Select |> ToScalar)

    let pth = YamlPath.Create "//{#'readable'}?"
    Assert.AreEqual("!!str", yml |> pth.Select |> ToScalarTag)
    Assert.AreEqual("value2", yml |> pth.Select |> ToScalar)

    let pth = YamlPath.Create "//{#'empty'}?"
    Assert.AreEqual("!!null", yml |> pth.Select |> ToScalarTag)

[<Test>]
let ``Test Map with inline Comments - Sunnny Day Simple``() =
    let pth = YamlPath.Create "//{#'key'}?"

    Assert.AreEqual("value", YamlParse "key:    # Comment\n  value" |> pth.Select |> ToScalar)
    Assert.AreEqual("value", YamlParse "key:    # Comment\n        # lines\n  value\n\n" |> pth.Select |> ToScalar)

//[<Test>]
//let ``Test Map with inline seperation lines - Sunnny Day Simple``() =
//    let pth = YamlPath.Create "//{#'key'}?"
//    YamlParse "{ first: Sammy, last: Sosa }:\n# Statistics:\n  hr:  # Home runs\n     65\n  avg: # Average\n   0.278"

