module TestStuctures

open NUnit.Framework
open TestUtils
open YamlParser
open FsUnit
open RepresentationGraph


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
    yml |> pth.Select |> ExtractTag  |>  should equal TagResolution.Failsafe.StringGlobalTag.Uri
    yml |> pth.Select |> ToScalar |>  should equal "value1"

    let pth = YamlPath.Create "//{#'readable'}?"
    yml |> pth.Select |> ExtractTag |> should equal TagResolution.Failsafe.StringGlobalTag.Uri
    yml |> pth.Select |> ToScalar |> should equal "value2"

    let pth = YamlPath.Create "//{#'empty'}?"
    yml |> pth.Select |> ExtractTag |> should equal TagResolution.JSON.NullGlobalTag.Uri

[<Test>]
let ``Test Map Null : Null - Sunnny Day Simple``() =
    let ptk = YamlPath.Create "//{#''}"
    let ptv = YamlPath.Create "//{#''}?"
    let yml = YamlParse ":"
    yml |> ptk.Select |> ExtractTag |> should equal TagResolution.JSON.NullGlobalTag.Uri
    yml |> ptv.Select |> ExtractTag |> should equal TagResolution.JSON.NullGlobalTag.Uri

[<Test>]
let ``Test Map key : Map - Sunnny Day Simple``() =
    let pt1 = YamlPath.Create "//{#'mainkey'}"
    let pt2 = YamlPath.Create "//{#'mainkey'}?/{#'key'}?"
    let yml = YamlParse "mainkey:\n key: value\n"

    yml |> pt1.Select |> ToScalar |> should equal "mainkey"
    yml |> pt2.Select |> ToScalar |> should equal "value"

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

// duplicate key tests


[<Test>]
let ``Test Map duplicate key - Simple``() =
    let err = YamlParseWithErrors " { a : b, a : c } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 1

[<Test>]
let ``Test Map triple key - Simple``() =
    let err = YamlParseWithErrors " { a : b, a : c, a : d } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 2

[<Test>]
let ``Test Map duplicate key - Adjacent``() =
    let err = YamlParseWithErrors " { a : b, b : d, a : c } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 1

[<Test>]
let ``Test Map duplicate key - Seq keys identical``() =
    let err = YamlParseWithErrors " { [ 1 , 2 ] : b, [ 1 , 2 ] : c } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 1

[<Test>]
let ``Test Map duplicate key - Seq keys unordered``() =
    let err = YamlParseWithErrors " { [ 1 , 2 ] : b, [ 2 , 1 ] : c } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 1

    
[<Test>]
let ``Test Map duplicate key - Map keys identical``() =
    let err = YamlParseWithErrors " { { 1 : 2 } : b, { 1 : 2 } : c } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 1


[<Test>]
let ``Test Map duplicate key - Map keys unordered``() =
    let err = YamlParseWithErrors " { {1 : 2, 2 : 1} : b, {2 : 1, 1 : 2} : c } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 1

