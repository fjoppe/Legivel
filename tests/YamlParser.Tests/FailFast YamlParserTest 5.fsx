#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#r @"bin/Debug/YamlParser.dll"
#r @"NLog/lib/net45/NLog.dll"

open YamlParse
open TagResolution
open Deserialization
open NLog

#load "nlog.fsx"
NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let logger = LogManager.GetLogger("*")

let engine = Yaml12Parser(fun s -> logger.Trace(s))


let YamlParse s =
    try
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Value
        let (nodes, ps) = pr
        let node = nodes.Head
        printfn "%s" (Deserialize node (ps.TagShorthands))
    with
    | e -> printfn "%A" e

let YamlParseList s =
    try
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Value
        let (nodes, ps) = pr
        nodes |> List.iter(fun node -> printfn "%s\n---" (Deserialize node (ps.TagShorthands)))
        nodes
    with
    | e -> printfn "%A" e; raise e


YamlParse "a:\n key: value\n"

YamlParse "plain key: in-line value\n: # Both empty\n\"quoted key\":- entry"

YamlParseList "
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


YamlParse "
---
hr:
  - Mark McGwire
  # Following node labeled SS
  - &SS Sammy Sosa
rbi:
  - *SS # Subsequent occurrence
  - Ken Griffey
"

YamlParse  "block sequence:\n  - one\n  - two : three\n"

YamlParse "
? - Detroit Tigers
  - Chicago cubs
:
  - 2001-07-23
? [ New York Yankees,
    Atlanta Braves ]
: [ 2001-07-02, 2001-08-12,
    2001-08-14 ]
"

YamlParse "
# ASCII Art
--- |
  \//||\/||
  // ||  ||__"

