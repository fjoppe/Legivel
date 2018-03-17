#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#r @"bin/Debug/Legivel.Common.dll"
#r @"bin/Debug/Legivel.RepresentationGraph.dll"
#r @"bin/Debug/Legivel.Parser.dll"
#r @"test/NLog/lib/net45/NLog.dll"
open System
open System.Globalization
open Legivel.Parser
open Legivel.TagResolution
open Legivel.Serialization
open Legivel.RepresentationGraph
open Legivel.Common
open NLog


#load "nlog.fsx"

open System
open System.Globalization

NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let logger = LogManager.GetLogger("*")

let engine = Yaml12Parser(fun s -> logger.Trace(s))

let WarnMsg (sl:ParseMessageAtLine list) = sl |> List.iter(fun s -> printfn "Warn: %d %d: %s" (s.Location.Line) (s.Location.Column) (s.Message))
let ErrMsg  (sl:ParseMessageAtLine list) = sl |> List.iter(fun s -> printfn "ERROR: %d %d:%s" (s.Location.Line) (s.Location.Column) (s.Message))
let TotLns (ps:DocumentLocation) = printfn "Total lines: %d" ps.Line

let PrintNode crr =
    match crr with
    |   NoRepresentation rr ->
        //printfn "Cannot parse: \"%s\"" rr.RestString
        rr.StopLocation |>  TotLns
        rr.Error |> ErrMsg
        rr.Warn |> WarnMsg
    |   PartialRepresentaton rr ->
        rr.StopLocation |>  TotLns
        rr.Warn |> WarnMsg
        printfn "%s" (SerializeToCanonical rr.Document (rr.TagShorthands))
    |   CompleteRepresentaton rr ->
        rr.StopLocation |>  TotLns
        rr.Warn |> WarnMsg
        printfn "%s" (SerializeToCanonical rr.Document (rr.TagShorthands))
    |   EmptyRepresentation rr ->
        printfn "Document was empty"
        rr.StopLocation |>  TotLns
        rr.Warn |> WarnMsg


let YamlParse s =
    try
        let repr = (engine.``l-yaml-stream`` YamlCore.Schema s)
        let crr = repr.Head
        PrintNode crr
    with
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e

let YamlParseList s =
    try
        let repr = (engine.``l-yaml-stream`` YamlCore.Schema s)
        printfn "Total Documents: %d" (repr.Length)
        repr |> List.iter(fun crr ->
            PrintNode crr
            printfn "..."
        )
    with
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e


// 7.12
YamlParse "
1st non-empty

 2nd non-empty 
\t3rd non-empty
"




DateTime.Parse("2014-09-12T00:00:00.0000000Z").ToUniversalTime()
DateTime(2014, 09, 12, 0, 0, 0)

YamlParse "2014-09-12"

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
..." 
