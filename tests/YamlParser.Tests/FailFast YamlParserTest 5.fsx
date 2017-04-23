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
        printfn "Total lines: %d" ps.LineNumber
        printfn "%s" (Deserialize node (ps.TagShorthands))
        ps.Messages |> List.iter(fun i -> 
            match i with
            |   Error t -> printfn "ERROR: %s" t
            |   Warn  t -> printfn "Warning: %s" t
        )
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


YamlParse "
canonical: 2001-12-15T02:59:43.1Z
iso8601: 2001-12-14t21:59:43.10-05:00
spaced: 2001-12-14 21:59:43.10 -5
date: 2002-12-14
"

YamlParse "
null:
booleans: [ true, false ]
string: '012345'
"

YamlParse "
!!str &a1 \"foo\":
  !!str bar
&a2 baz : *a1
"

