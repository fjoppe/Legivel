module Chapter2Tests

(*
    Testing examples from chapte 2: http://www.yaml.org/spec/1.2/spec.html#Preview
*)

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
    try
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Value
        let (nodes, ps) = pr
        let node = nodes.Head
        printfn "%s" (Deserialize node (ps.TagShorthands))
        node
    with
    | e -> printfn "%A" e; raise e

let YamlParseList s =
    let engine = Yaml12Parser()
    try
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Value
        let (nodes, ps) = pr
        nodes |> List.iter(fun node -> printfn "%s" (Deserialize node (ps.TagShorthands)))
        nodes
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


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760118")>]
let ``Example 2.1.  Sequence of Scalars``() =
    let yml = YamlParse "
- Mark McGwire
- Sammy Sosa
- Ken Griffey"

    ["Mark McGwire";"Sammy Sosa"; "Ken Griffey"]
    |> List.iter(fun s-> 
        let pth = YamlPath.Create (sprintf "//[]/#'%s'" s)
        yml |> pth.Select |> ToScalar |> should equal s
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760142")>]
let ``Example 2.2.  Mapping Scalars to Scalars``() =
    let yml = YamlParse "
hr:  65    # Home runs
avg: 0.278 # Batting average
rbi: 147   # Runs Batted In"

    [("hr", "65");("avg","0.278"); ("rbi","147")]
    |> List.iter(fun (k,v) -> 
        let pth = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> pth.Select |> ToScalar |> should equal v
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760167")>]
let ``Example 2.3.  Mapping Scalars to Sequences``() =
    let yml = YamlParse "
american:
  - Boston Red Sox
  - Detroit Tigers
  - New York Yankees
national:
  - New York Mets
  - Chicago Cubs
  - Atlanta Braves"

    [
    ("american","Boston Red Sox");("american","Detroit Tigers"); ("american","New York Yankees")
    ("national","New York Mets");("national","Chicago Cubs"); ("national","Atlanta Braves")
    ]
    |> List.iter(fun (k,v) -> 
        let pth = YamlPath.Create (sprintf "//{#'%s'}?/[]/#'%s'" k v)
        yml |> pth.Select |> ToScalar |> should equal v
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760193")>]
let ``Example 2.4.  Sequence of Mappings``() =
    let yml = YamlParse "
-
  name: Mark McGwire
  hr:   65
  avg:  0.278
-
  name: Sammy Sosa
  hr:   63
  avg:  0.288"

    [
    ("name","Mark McGwire"); ("hr","65"); ("avg","0.278")
    ("name","Sammy Sosa");("hr","63"); ("avg","0.288")
    ]
    |> List.iter(fun (k,v) -> 
        let ypath = (sprintf "//[]/{#'%s'}?/#'%s'" k v)
        let pth = YamlPath.Create ypath
        yml |> pth.Select |> ToScalar |> should equal v
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760351")>]
let ``Example 2.5. Sequence of Sequences``() =
    let yml = YamlParse "
- [name        , hr, avg  ]
- [Mark McGwire, 65, 0.278]
- [Sammy Sosa  , 63, 0.288]"

    [
    "name"; "hr"; "avg"
    "Mark McGwire"; "65"; "0.278"
    "Sammy Sosa"; "63"; "0.288"
    ]
    |> List.iter(fun s -> 
        let ypath = (sprintf "//[]/[]/#'%s'" s)
        let pth = YamlPath.Create ypath
        yml |> pth.Select |> ToScalar |> should equal s
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760372")>]
let ``Example 2.6. Mapping of Mappings``() =
    let yml = YamlParse "
Mark McGwire: {hr: 65, avg: 0.278}
Sammy Sosa: {
    hr: 63,
    avg: 0.288
  }"

    [
    ("Mark McGwire", "hr", "65");   ("Mark McGwire", "avg", "0.278")
    ("Sammy Sosa", "hr", "63");     ("Sammy Sosa", "avg", "0.288")
    ]
    |> List.iter(fun (n,p,v) -> 
        let ypath = (sprintf "//{#'%s'}?/{#'%s'}?" n p)
        let pth = YamlPath.Create ypath
        yml |> pth.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760493")>]
let ``Example 2.7. Two Documents in a Stream (each with a leading comment)``() =
    let yml = YamlParseList "
# Ranking of 1998 home runs
---
- Mark McGwire
- Sammy Sosa
- Ken Griffey

# Team ranking
---
- Chicago Cubs
- St Louis Cardinals
"
    yml.Length |> should equal 2

    let [yml1; yml2] = yml 

    ["Mark McGwire"; "Sammy Sosa"; "Ken Griffey"]
    |> List.iter(fun e -> 
        let ypath = (sprintf "//[]/#'%s'" e)
        let pth = YamlPath.Create ypath
        yml1 |> pth.Select |> ToScalar |> should equal e
    )
    
    ["Chicago Cubs"; "St Louis Cardinals"]
    |> List.iter(fun e -> 
        let ypath = (sprintf "//[]/#'%s'" e)
        let pth = YamlPath.Create ypath
        yml2 |> pth.Select |> ToScalar |> should equal e
    )



