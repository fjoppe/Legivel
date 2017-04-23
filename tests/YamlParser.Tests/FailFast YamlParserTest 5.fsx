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
!!str &a1 \"foo\":
  !!str bar
&a2 baz : *a1
"

