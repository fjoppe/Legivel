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


//  Test !!seq
[<Test>]
let ``Test Seq full - Sunny Day Simple``() =
    let yml = YamlParse "
# Ordered sequence of nodes
Block style: !!seq
- Mercury   # Rotates - no light/dark sides.
- Venus     # Deadliest. Aptly named.
- Earth     # Mostly dirt.
- Mars      # Seems empty.
- Jupiter   # The king.
- Saturn    # Pretty.
- Uranus    # Where the sun hardly shines.
- Neptune   # Boring. No rings.
- Pluto     # You call this a planet?
Flow style: !!seq [ Mercury, Venus, Earth, Mars,      # Rocks
                    Jupiter, Saturn, Uranus, Neptune, # Gas
                    Pluto ]                           # Overrated" 

    let btpth = YamlPath.Create "//{#'Block style'}?"
    let ftpth = YamlPath.Create "//{#'Flow style'}?"
    [btpth;ftpth]
    |> List.iter(fun pth -> 
        yml |> pth.Select |> ExtractTag |> should equal TagResolution.Failsafe.SequenceGlobalTag.Uri
    )

    

// Duplicate key tests Yaml Core schema


[<Test>]
let ``Test YamlCore Map duplicate key - Simple``() =
    let err = YamlParseWithErrors " { a : b, a : c } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 1

[<Test>]
let ``Test YamlCore Map triple key - Simple``() =
    let err = YamlParseWithErrors " { a : b, a : c, a : d } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 2

[<Test>]
let ``Test YamlCore Map duplicate key - Adjacent``() =
    let err = YamlParseWithErrors " { a : b, b : d, a : c } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 1

[<Test>]
let ``Test YamlCore Map duplicate key - Seq keys identical``() =
    let err = YamlParseWithErrors " { [ 1 , 2 ] : b, [ 1 , 2 ] : c } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 1

[<Test>]
let ``Test YamlCore Map duplicate key - Seq keys unordered``() =
    let err = YamlParseWithErrors " { [ 1 , 2 ] : b, [ 2 , 1 ] : c } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 1

    
[<Test>]
let ``Test YamlCore Map duplicate key - Map keys identical``() =
    let err = YamlParseWithErrors " { { 1 : 2 } : b, { 1 : 2 } : c } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 1


[<Test>]
let ``Test YamlCore Map duplicate key - Map keys unordered``() =
    let err = YamlParseWithErrors " { {1 : 2, 2 : 1} : b, {2 : 1, 1 : 2} : c } "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should equal 1

//  Ill formed
[<Test>]
let ``Test YamlCore Map ill formed - missing comma``() =
    let err = YamlParseWithErrors "
    { 
        ? b
        : value # missing comma
        ? a
        : value        
    }
"
    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message ="Incorrect mapping syntax, are you missing a comma, or }?") |> List.length |> should equal 1


//  YamlExtended - !!omap

[<Test>]
let ``Test YamlExtended omap sunny day - omap assigned``() =
    let yml = YamlParseForSchema TagResolution.YamlExtendedSchema "
# Ordered maps are represented as
# A sequence of mappings, with
# each mapping having one key
--- !!omap
- Mark McGwire: 65
- Sammy Sosa: 63
- Ken Griffy: 58
"
    [
        ("Mark McGwire", "65")
        ("Sammy Sosa", "63")
        ("Ken Griffy", "58")
    ]
    |> List.iter(fun (k,v) -> 
        let ypath = (sprintf "//[]/{#'%s'}?" k)
        let pth = YamlPath.Create ypath
        yml |> pth.Select |> ToScalar |> should equal v
    )

    Some([yml]) |> ExtractTag |> should equal TagResolution.YamlExtended.OrderedMappingGlobalTag.Uri

[<Test>]
let ``Test YamlExtended omap sunny day - omap detected``() =
    let yml = YamlParseForSchema TagResolution.YamlExtendedSchema "
# Ordered maps are represented as
# A sequence of mappings, with
# each mapping having one key
---
- Mark McGwire: 65
- Sammy Sosa: 63
- Ken Griffy: 58
"
    [
        ("Mark McGwire", "65")
        ("Sammy Sosa", "63")
        ("Ken Griffy", "58")
    ]
    |> List.iter(fun (k,v) -> 
        let ypath = (sprintf "//[]/{#'%s'}?" k)
        let pth = YamlPath.Create ypath
        yml |> pth.Select |> ToScalar |> should equal v
    )

    Some([yml]) |> ExtractTag |> should equal TagResolution.YamlExtended.OrderedMappingGlobalTag.Uri

[<Test>]
let ``Test YamlExtended omap sunny day - pairs detected because of duplicate key``() =
    let yml = YamlParseForSchema TagResolution.YamlExtendedSchema "
# Ordered maps are represented as
# A sequence of mappings, with
# each mapping having one key
---
- Mark McGwire: 65
- Sammy Sosa: 63
- Ken Griffy: 58
- Ken Griffy: 58    # duplicate
"
    [
        ("Mark McGwire", "65")
        ("Sammy Sosa", "63")
        ("Ken Griffy", "58")
    ]
    |> List.iter(fun (k,v) -> 
        let ypath = (sprintf "//[]/{#'%s'}?" k)
        let pth = YamlPath.Create ypath
        yml |> pth.Select |> Option.get |> List.length |> should be (greaterThan 0)
    )

    Some([yml]) |> ExtractTag |> should equal TagResolution.YamlExtended.OrderedPairsGlobalTag.Uri

[<Test>]
let ``Test YamlExtended omap rainy day - omap assinged - voilating duplicate key``() =
    let err = YamlParseForSchemaWithErrors TagResolution.YamlExtendedSchema "
# Ordered maps are represented as
# A sequence of mappings, with
# each mapping having one key
--- !!omap
- Mark McGwire: 65
- Sammy Sosa: 63
- Ken Griffy: 58
- Ken Griffy: 58    # duplicate
"

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should be (greaterThan 0)

[<Test>]
let ``Test YamlExtended omap rainy day - omap assigned - equality``() =
    let err = YamlParseForSchemaWithErrors TagResolution.YamlExtendedSchema "

    {
        ? !!omap [ a : 1, b : 1] 
        : value,
        ? !!omap [ a : 1, b : 1] 
        : value
    }
"
    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should be (greaterThan 0)


[<Test>]
let ``Test YamlExtended omap sunny day - omap assigned - equality - reordered``() =
    let yml = YamlParseForSchema TagResolution.YamlExtendedSchema "
    {
        ? !!omap [ a : 1, b : 1] 
        : value,
        ? !!omap [b : 1,  a : 1] 
        : value
    }
"
    Some([yml]) |> ExtractTag |> should equal TagResolution.Failsafe.MappingGlobalTag.Uri

    let pth = YamlPath.Create "//{}"

    yml 
    |> pth.Select 
    |> Option.get
    |> List.iter(fun n -> 
            [n]
            |> Some
            |> ExtractTag 
            |> should equal TagResolution.YamlExtended.OrderedMappingGlobalTag.Uri)
    


//  YamlExtended - !!pairs

[<Test>]
let ``Test YamlExtended pairs sunny day - pairs assigned``() =
    let yml = YamlParseForSchema TagResolution.YamlExtendedSchema "
# Explicitly typed pairs.
Block tasks: !!pairs
  - meeting: with team.
  - meeting: with boss.
  - break: lunch.
  - meeting: with client.
Flow tasks: !!pairs [ meeting: with team, meeting: with boss ]
"
    [("meeting",3);("break",1)]
    |> List.iter(fun (k,l) -> 
        let ypath = (sprintf "//{#'Block tasks'}?/[]/{#'%s'}" k)
        let pth = YamlPath.Create ypath
        yml |> pth.Select |> Option.get |> List.length |> should equal l
    )

    let btpth = YamlPath.Create "//{#'Block tasks'}?"
    let ftpth = YamlPath.Create "//{#'Flow tasks'}?"
    [btpth;ftpth]
    |> List.iter(fun pth -> 
        yml |> pth.Select |> ExtractTag |> should equal TagResolution.YamlExtended.OrderedPairsGlobalTag.Uri
    )

[<Test>]
let ``Test YamlExtended pairs sunny day - pairs detected``() =
    let yml = YamlParseForSchema TagResolution.YamlExtendedSchema "
# Explicitly typed pairs.
Block tasks:    # no pairs tag
  - meeting: with team.
  - meeting: with boss.
  - break: lunch.
  - meeting: with client.
Flow tasks: !!pairs [ meeting: with team, meeting: with boss ]
"
    [("meeting",3);("break",1)]
    |> List.iter(fun (k,l) -> 
        let ypath = (sprintf "//{#'Block tasks'}?/[]/{#'%s'}" k)
        let pth = YamlPath.Create ypath
        yml |> pth.Select |> Option.get |> List.length |> should equal l
    )

    let btpth = YamlPath.Create "//{#'Block tasks'}?"
    let ftpth = YamlPath.Create "//{#'Flow tasks'}?"
    [btpth;ftpth]
    |> List.iter(fun pth -> 
        yml |> pth.Select |> ExtractTag |> should equal TagResolution.YamlExtended.OrderedPairsGlobalTag.Uri
    )

[<Test>]
let ``Test YamlExtended pairs rainy day - pairs assinged - voilating pair constraint``() =
    let err = YamlParseForSchemaWithErrors TagResolution.YamlExtendedSchema "
--- !!pairs
- meeting: with team.
- 1     # not a pair
"

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Construct has incorrect syntax for tag tag:yaml.org,2002:pairs")) |> List.length |> should be (greaterThan 0)


[<Test>]
let ``Test YamlExtended pairs rainy day - pairs assigned - equality``() =
    let err = YamlParseForSchemaWithErrors TagResolution.YamlExtendedSchema "

    {
        ? !!pairs [ a : 1, b : 1] 
        : value,
        ? !!pairs [ a : 1, b : 1] 
        : value
    }
"
    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should be (greaterThan 0)

[<Test>]
let ``Test YamlExtended pairs sunny day - pairs assigned - equality - reordered``() =
    let yml = YamlParseForSchema TagResolution.YamlExtendedSchema "

    {
        ? !!pairs [ a : 1, b : 1] 
        : value,
        ? !!pairs [ b : 1, a : 1] 
        : value
    }
"
    Some([yml]) |> ExtractTag |> should equal TagResolution.Failsafe.MappingGlobalTag.Uri

    let pth = YamlPath.Create "//{}"

    yml 
    |> pth.Select 
    |> Option.get
    |> List.iter(fun n -> 
            [n]
            |> Some
            |> ExtractTag 
            |> should equal TagResolution.YamlExtended.OrderedPairsGlobalTag.Uri)


//  YamlExtended - !!set

[<Test>]
let ``Test YamlExtended set sunny day - set assigned``() =
    let yml = YamlParseForSchema TagResolution.YamlExtendedSchema "
# Explicitly typed set.
baseball players: !!set
  ? Mark McGwire
  ? Sammy Sosa
  ? Ken Griffey
# Flow style
baseball teams: !!set { Boston Red Sox, Detroit Tigers, New York Yankees }
"
    ["Mark McGwire";"Sammy Sosa";"Ken Griffey"]
    |> List.iter(fun k -> 
        let ypath = (sprintf "//{#'baseball players'}?/{#'%s'}" k)
        let pth = YamlPath.Create ypath
        yml |> pth.Select |> Option.get |> List.length |> should equal 1
    )

    let btpth = YamlPath.Create "//{#'baseball players'}?"
    let ftpth = YamlPath.Create "//{#'baseball teams'}?"
    [btpth;ftpth]
    |> List.iter(fun pth -> 
        yml |> pth.Select |> ExtractTag |> should equal TagResolution.YamlExtended.UnOrderedSetGlobalTag.Uri
    )


[<Test>]
let ``Test YamlExtended set sunny day - set detected``() =
    let yml = YamlParseForSchema TagResolution.YamlExtendedSchema "
# Explicitly typed set.
baseball players:
  ? Mark McGwire
  ? Sammy Sosa
  ? Ken Griffey
# Flow style
baseball teams: !!set { Boston Red Sox, Detroit Tigers, New York Yankees }
"
    ["Mark McGwire";"Sammy Sosa";"Ken Griffey"]
    |> List.iter(fun k -> 
        let ypath = (sprintf "//{#'baseball players'}?/{#'%s'}" k)
        let pth = YamlPath.Create ypath
        yml |> pth.Select |> Option.get |> List.length |> should equal 1
    )

    let btpth = YamlPath.Create "//{#'baseball players'}?"
    let ftpth = YamlPath.Create "//{#'baseball teams'}?"
    [btpth;ftpth]
    |> List.iter(fun pth -> 
        yml |> pth.Select |> ExtractTag |> should equal TagResolution.YamlExtended.UnOrderedSetGlobalTag.Uri
    )

[<Test>]
let ``Test YamlExtended set rainy day - set assinged - voilating set unique constraint``() =
    let err = YamlParseForSchemaWithErrors TagResolution.YamlExtendedSchema "
--- !!set
    ?   duplicate
    ?   duplicate
"
    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should be (greaterThan 0)


[<Test>]
let ``Test YamlExtended set rainy day - set assinged - voilating set no-value constraint``() =
    let err = YamlParseForSchemaWithErrors TagResolution.YamlExtendedSchema "
--- !!set
    ?   some value
    ?   has illegal
    :   value
"
    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Construct has incorrect syntax for tag")) |> List.length |> should be (greaterThan 0)


[<Test>]
let ``Test YamlExtended set rainy day - set assigned - equality``() =
    let err = YamlParseForSchemaWithErrors TagResolution.YamlExtendedSchema "
    {
        ? !!set { a, b }
        : value,
        ? !!set { a, b }
        : value
    }
"
    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should be (greaterThan 0)

[<Test>]
let ``Test YamlExtended set rainy day - set assigned - equality - reordered``() =
    let err = YamlParseForSchemaWithErrors TagResolution.YamlExtendedSchema "
    {
        ? !!set { a, b }
        : value,
        ? !!set { b, a }
        : value
    }
"
    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Duplicate key for node")) |> List.length |> should be (greaterThan 0)


[<Test>]
let ``Test reference non existend anchor - Rainy Day``() =
    let err = YamlParseWithErrors "
    -   first
    -   *invalidanchor
    "

    err.Error.Length |> should be (greaterThan 0)
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Referenced anchor 'invalidanchor'")) |> List.length |> should be (greaterThan 0)


