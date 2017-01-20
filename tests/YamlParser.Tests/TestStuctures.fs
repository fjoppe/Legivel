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
let ``Test Map at Root Level - Sunny Day Simple``() =
    let pth = YamlPath.Create "//{#'a'}"
    Assert.AreEqual("a", YamlParse "{ a: b }" |> pth.Select |> ToScalar)


