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
    | DocumentException e -> 
        e.Messages  |> List.iter(fun m -> 
            match m with
            |   Error s -> printfn "Error: %s" s
            |   Warn  s -> printfn "Warning: %s" s
        )
        raise (DocumentException e)
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e

let YamlParseList s =
    try
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Value
        let (nodes, ps) = pr
        nodes |> List.iter(fun node -> printfn "%s\n---" (Deserialize node (ps.TagShorthands)))
        nodes
    with
    | DocumentException e -> 
        e.Messages  |> List.iter(fun m -> 
            match m with
            |   Error s -> printfn "Error: %s" s
            |   Warn  s -> printfn "Warning: %s" s
        )
        raise (DocumentException e)
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e


YamlParse "
plain key: in-line value
  :  # Both empty
\"quoted key\":
- entry
"

