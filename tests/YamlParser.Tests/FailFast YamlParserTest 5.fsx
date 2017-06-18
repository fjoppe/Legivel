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
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Data
        let (nodes, ps) = pr
        let node = nodes.Head
        printfn "Total lines: %d" ps.Location.Line
        printfn "%s" (Deserialize node (ps.TagShorthands))
        
        ps.Messages.Warn  |> List.iter(fun s -> printfn "Warn: %d %d: %s" (s.Location.Line) (s.Location.Column) (s.Message))
        ps.Messages.Error |> List.iter(fun s -> printfn "ERROR: %d %d:%s" (s.Location.Line) (s.Location.Column)  (s.Message))
        if ps.Messages.Error.Length > 0 then printfn "Cannot parse: \"%s\"" ps.InputString
    with
    | DocumentException e -> 
        e.Messages.Warn  |> List.iter(fun s -> printfn "Warn: %d %d: %s" (s.Location.Line) (s.Location.Column) (s.Message))
        e.Messages.Error |> List.iter(fun s -> printfn "ERROR: %d %d:%s" (s.Location.Line) (s.Location.Column)  (s.Message))
        raise (DocumentException e)
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e

let YamlParseList s =
    try
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Data
        let (nodes, ps) = pr
        printfn "Total lines: %d" ps.Location.Line
        ps.Messages.Warn  |> List.iter(fun s -> printfn "Warn: %d %d: %s" (s.Location.Line) (s.Location.Column) (s.Message))
        ps.Messages.Error |> List.iter(fun s -> printfn "ERROR: %d %d:%s" (s.Location.Line) (s.Location.Column)  (s.Message))
        if ps.Messages.Error.Length > 0 then printfn "Cannot parse: \"%s\"" ps.InputString
        nodes |> List.iter(fun node -> printfn "%s\n---" (Deserialize node (ps.TagShorthands)))
    with
    | DocumentException e -> 
        e.Messages.Warn  |> List.iter(fun s -> printfn "Warn: %d %d: %s" (s.Location.Line) (s.Location.Column) (s.Message))
        e.Messages.Error |> List.iter(fun s -> printfn "ERROR: %d %d:%s" (s.Location.Line) (s.Location.Column)  (s.Message))
        raise (DocumentException e)
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e


YamlParseList "
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



