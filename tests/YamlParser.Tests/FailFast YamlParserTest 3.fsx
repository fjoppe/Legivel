open System
open System.Text
open System.IO
open System.Text.RegularExpressions

#I "."
#r @"bin\Debug\YamlParser.dll"

open RegexDSL
open YamlParse


//  Tests
let engine = FlowCollectionStyles()


let ``c-flow-json-node`` s =
    let ps = ParseState.Create s
    let ps = ps.SetStyleContext ``Flow-key``
    let d1 = engine.``c-flow-json-node`` ps
    let n1 = fst(d1.Value)
    printf "%s" (n1.ToCanonical(0))

``c-flow-json-node`` "[ a, b ]"


let ``ns-flow-node`` s =
    let ps = ParseState.Create s
    let ps = ps.SetStyleContext ``Flow-key``
    let d2 = engine.``ns-flow-node`` ps
    let n2 = fst(d2.Value)
    printf "%s" (n2.ToCanonical(0))

``ns-flow-node`` "{ a: b }"
``ns-flow-node`` "[ a, b ]"


let ``c-ns-flow-map-json-key-entry`` s =
    let ps = ParseState.Create s
    let ps = ps.SetStyleContext ``Flow-key``
    let d3 = engine.``c-ns-flow-map-json-key-entry`` ps
    d3.Value

``c-ns-flow-map-json-key-entry`` "\"adjacent\":value"
``c-ns-flow-map-json-key-entry`` "\"readable\": value"
``c-ns-flow-map-json-key-entry`` "\"empty\":"


let ``ns-flow-pair-entry`` s = 
    let ps = ParseState.Create s
    let ps = ps.SetStyleContext ``Flow-key``
    let d3 = engine.``ns-flow-pair-entry`` ps
    d3.Value
    
``ns-flow-pair-entry`` "YAML : separate"
``ns-flow-pair-entry`` ": empty key entry"
``ns-flow-pair-entry`` "{JSON: like}:adjacent"


let ``ns-flow-content`` s = 
    let ps = ParseState.Create s
    let ps = ps.SetStyleContext ``Flow-key``
    let d3 = engine.``ns-flow-content`` ps
//    d3.Value
    let n2 = fst(d3.Value)
    printf "%s" (n2.ToCanonical(0))

``ns-flow-content`` "[ a, b ]"
``ns-flow-content`` "{ a: b }"
``ns-flow-content`` "\"a\""
``ns-flow-content`` "'b'"
``ns-flow-content`` "c"


//HasMatches("\" 1st non-empty\n\n 2nd non-empty \n 3rd non-empty \"", RGS(``nb-double-multi-line`` 0))
//
//HasMatches("'here''s to \"quotes\"'", ``nb-single-one-line``)
//
//let ps = ((((ParseState.Create "").SetChomping ``Clip``).SetContext ``Flow-key``).SetIndent 1)
//let ``l-literal-content`` = (engine.``l-literal-content`` ps)
//let (b,m,r) =  HasMatches(" \n  \n  literal\n   \n  \n  text\n\n # Comment\nblabla", ``l-literal-content``)
//let split = m |> engine.``split by linefeed``
//let aut = split |> engine.``auto detect indent in block`` ps.n

let ``c-l+literal`` s = 
    let ps = ParseState.Create s
    let ps = ps.SetIndent 1
    let ps = ps.SetSubIndent 0
    let d = engine.``c-l+literal`` ps
    d

try
    let rs = (``c-l+literal`` "|\n \n  \n  literal\n   \n  \n  text\n\n # Comment\nmore stuff").Value |> fst 
    printfn "%s" rs
with
| e -> printfn "%A" e


let ``c-l+folded`` s =
    let ps = ParseState.Create s
    let ps = ps.SetIndent 1
    let ps = ps.SetSubIndent 0
    let d = engine.``c-l+folded`` ps
    d

try
    let rs = (``c-l+folded`` ">\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment\nmore stuff").Value |> fst
    printfn "%s" rs
with
| e -> printfn "%A" e


let cfolded = (``c-l+folded`` ">\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment\nmore stuff").Value |> fst


let ``s-l+block-node`` s = 
    let ps = ParseState.Create s
    let ps = ps.SetIndent -1
    let ps = ps.SetSubIndent 0
    let ps = ps.SetStyleContext ``Block-in``
    let d = engine.``s-l+block-node`` ps 
    d


#load "nlog.fsx"
NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let YamlParse s =
    try
        let rs = (``s-l+block-node`` s).Value |> fst
        printfn "%s" (rs.ToCanonical(0))
    with
    | e -> printfn "%A" e

YamlParse "- simple\n- text\n- [ testing, one, two, three ]"
YamlParse "- [ testing, one, two, three ]"
YamlParse "- { testing: 0, one: 1, two: 2, three : 3 }"

//let str = "{\n\"adjacent\":value,\n  \"readable\": value,\n  \"empty\":\n}"
//let ps2 = ((((ParseState.Create "").SetChomping ``Clip``).SetStyleContext ``Block-key``).SetIndent 0)
//let mt =  (|Regex2|_|) ((RGP "\\{") + ZOM(engine.``s-separate`` ps2)) str

//let rm = System.Text.RegularExpressions.Regex.Match("\n\"adjacent\"", "[ ]+|.^", System.Text.RegularExpressions.RegexOptions.Multiline)
//rm.Value

//
//let str2 = "\"adjacent\":value,\n\"readable\": value,\n\"empty\":\n}"
//let (b3,m3,r3) =  (HasMatches(str2, RGS((RGP "\"") + GRP(engine.``nb-double-text`` ps2) + (RGP "\""))))


YamlParse "{\"adjacent\":value, \"readable\": value,  \"empty\":}"
YamlParse "{\n\"adjacent\":value,\n\"readable\": value,\n\"empty\":\n}"
YamlParse "{\nadjacent: value,\n readable : value,\n empty :\n}"
//YamlParse "{\n  \"adjacent\":value,\n  \"readable\": value,\n\"empty\":\n}"


//  Line folding
YamlParse ">-\n  trimmed\n  \n \n\n  as\n  space"
YamlParse ">\n  foo \n \n  \t bar\n\n  baz\n"
YamlParse "\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n" 
YamlParse "folded \nto a space,\t\n \nto a line feed, or \t\\\n \\ \tnon-content"


//  Comments
YamlParse "key:    # Comment\n  value"
YamlParse "key:    # Comment\n        # lines\n  value\n\n"


//  Seperation lines
YamlParse "{ first: Sammy, last: Sosa }:\n# Statistics:\n  hr:  # Home runs\n     65\n  avg: # Average\n   0.278"

//let ps = ((((ParseState.Create "").SetChomping ``Clip``).SetContext ``Flow-out``).SetIndent 1)
//let ``tag anchor`` = GRP(engine.``c-ns-tag-property``) + OPT((engine.``s-separate`` ps) + GRP(engine.``c-ns-anchor-property``))
//let ``anchor tag`` = GRP(engine.``c-ns-anchor-property``) + OPT((engine.``s-separate`` ps) + GRP(engine.``c-ns-tag-property``))
//let pattern = ``anchor tag``
//let (b,m,r) =  HasMatches("&anchor !foo Foo", RGS(pattern))
//
//
//(|Regex2|_|) ``anchor tag`` "&anchor !foo Foo"
//(|Regex2|_|) ``tag anchor`` "!foo &anchor Foo"

YamlParse "First occurrence: &anchor Foo
Second occurrence: *anchor"

//Override anchor: &anchor Bar
//Reuse anchor: *anchor"


YamlParse "  # Leading comment line spaces are
   # neither content nor indentation.
    
Not indented:
 By one space: |
    By four
      spaces
 Flow style: [    # Leading spaces
   By two,        # in flow style
  Also by two,    # are neither
    Still by two   # content nor
    ]             # indentation."



