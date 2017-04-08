module Chapter2Tests

(*
    Testing examples from chapter 2: http://www.yaml.org/spec/1.2/spec.html#Preview
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

let ToSequence n =
    match n with
    |   Some([SeqNode nd]) -> nd.Data
    |   _ -> raise (Exception "Is no seq")
    

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

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760519")>]
let ``Example 2.8.  Play by Play Feed from a Game``() =
    let yml = YamlParseList "
---
time: 20:03:20
player: Sammy Sosa
action: strike (miss)
...
---
time: 20:03:47
player: Sammy Sosa
action: grand slam
...
"
    yml.Length |> should equal 2

    let [yml1; yml2] = yml 

    [("time", "20:03:20"); ("player", "Sammy Sosa");("action", "strike (miss)")]
    |> List.iter(fun (k,v) -> 
        let ypath = (sprintf "//{#'%s'}?" k)
        let pth = YamlPath.Create ypath
        yml1 |> pth.Select |> ToScalar |> should equal v
    )

    [("time", "20:03:47"); ("player", "Sammy Sosa");("action", "grand slam")]
    |> List.iter(fun (k,v) -> 
        let ypath = (sprintf "//{#'%s'}?" k)
        let pth = YamlPath.Create ypath
        yml2 |> pth.Select |> ToScalar |> should equal v
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760633")>]
let ``Example 2.9.  Single Document with Two Comments``() =
    let yml = YamlParseList "
---
hr: # 1998 hr ranking
  - Mark McGwire
  - Sammy Sosa
rbi:
  # 1998 rbi ranking
  - Sammy Sosa
  - Ken Griffey
"
    yml.Length |> should equal 1

    [("hr", "Mark McGwire", 2); ("rbi", "Sammy Sosa", 2)]
    |> List.iter(fun (k,v,c) -> 
        let p1 = YamlPath.Create (sprintf "//{#'%s'}?" k)
        let p2 = YamlPath.Create (sprintf "//{#'%s'}?/[]/#'%s'" k v)

        yml.Head |> p1.Select |> ToSequence |> fun s -> s.Length |> should equal c
        yml.Head |> p2.Select |> ToScalar |> should equal v
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760658")>]
let ``Example 2.10.  Node for “Sammy Sosa” appears twice in this document``() =
    let yml = YamlParseList "
---
hr:
  - Mark McGwire
  # Following node labeled SS
  - &SS Sammy Sosa
rbi:
  - *SS # Subsequent occurrence
  - Ken Griffey
"
    yml.Length |> should equal 1

    [("hr", "Sammy Sosa", 2); ("rbi", "Sammy Sosa", 2)]
    |> List.iter(fun (k,v,c) -> 
        let p1 = YamlPath.Create (sprintf "//{#'%s'}?" k)
        let p2 = YamlPath.Create (sprintf "//{#'%s'}?/[]/#'%s'" k v)

        yml.Head |> p1.Select |> ToSequence |> fun s -> s.Length |> should equal c
        yml.Head |> p2.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760799")>]
let ``Example 2.11. Mapping between Sequences``() =
    let yml = YamlParseList "
? - Detroit Tigers
  - Chicago cubs
:
  - 2001-07-23

? [ New York Yankees,
    Atlanta Braves ]
: [ 2001-07-02, 2001-08-12,
    2001-08-14 ]
"
    yml.Length |> should equal 1

    //  TODO: extend YamlPath to query above structure
//    let p1 = YamlPath.Create "//{}/[]/#'Detroit Tigers'"
//    let p2 = YamlPath.Create "//{}?/[]/#'2001-07-02'"

//    yml.Head |> p1.Select |> ToScalar |> should equal "Detroit Tigers"
//    yml.Head |> p2.Select |> ToScalar |> should equal "2001-07-02"

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761008")>]
let ``Example 2.13.  In literals, newlines are preserved``() =
    let yml = YamlParse "
# ASCII Art
--- |
  \//||\/||
  // ||  ||__"

    Some([yml]) |> ToScalarTag |> should equal TagResolution.Failsafe.StringGlobalTag.Uri

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761032")>]
let ``Example 2.14.  In the folded scalars, newlines become spaces``() =
    let yml = YamlParse "
--- >
  Mark McGwire's
  year was crippled
  by a knee injury."

    Some([yml]) |> ToScalarTag |> should equal TagResolution.Failsafe.StringGlobalTag.Uri
    Some([yml]) |> ToScalar |> should equal "Mark McGwire's year was crippled by a knee injury."


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761056")>]
let ``Example 2.15.  Folded newlines are preserved for "more indented" and blank lines``() =
    let yml = YamlParse "
>
 Sammy Sosa completed another
 fine season with great stats.

   63 Home Runs
   0.288 Batting Average

 What a year!"

    Some([yml]) |> ToScalarTag |> should equal TagResolution.Failsafe.StringGlobalTag.Uri
    Some([yml]) |> ToScalar |> should equal "Sammy Sosa completed another fine season with great stats.\n\n  63 Home Runs\n  0.288 Batting Average\n\nWhat a year!"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761056")>]
let ``Example 2.16.  Indentation determines scope``() =
    let yml = YamlParse "
name: Mark McGwire
accomplishment: >
  Mark set a major league
  home run record in 1998.
stats: |
  65 Home Runs
  0.278 Batting Average"

    [
        ("name", "Mark McGwire")
        ("accomplishment", "Mark set a major league home run record in 1998.\n") // TODO: not sure about this last \n
        ("stats", "65 Home Runs\n0.278 Batting Average")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761245")>]
let ``Example 2.17. Quoted Scalars``() =
    let yml = YamlParse "
unicode: \"Sosa did fine.\\u263A\"
control: \"\\b1998\\t1999\\t2000\\n\"
hex esc: \"\\x0d\\x0a is \\r\\n\"

single: '\"Howdy!\" he cried.'
quoted: ' # Not a ''comment''.'
tie-fighter: '|\\-*-/|'
"
    [
        ("unicode", "Sosa did fine.\u263A")
        ("control", "\b1998\t1999\t2000\n") 
        ("hex esc", "\r\n is \r\n")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )