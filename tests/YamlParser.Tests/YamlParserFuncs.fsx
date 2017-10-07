module Yaml

#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#r @"bin/Debug/FsYamlParser.dll"
#r @"NLog/lib/net45/NLog.dll"

open YamlParse
open TagResolution
open Deserialization
open RepresentationGraph
open YamlParser.Common
open NLog


#load "nlog.fsx"
NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let logger = LogManager.GetLogger("*")

let engine = Yaml12Parser()

let WarnMsg (sl:ParseMessageAtLine list) = sl |> List.iter(fun s -> printfn "Warn: %d %d: %s" (s.Location.Line) (s.Location.Column) (s.Message))
let ErrMsg  (sl:ParseMessageAtLine list) = sl |> List.iter(fun s -> printfn "ERROR: %d %d:%s" (s.Location.Line) (s.Location.Column) (s.Message))
let TotLns (ps:DocumentLocation) = printfn "Total lines: %d" ps.Line

let PrintNode crr =
    match crr with
    |   NoRepresentation rr ->
        printfn "Cannot parse: \"%s\"" rr.RestString
        rr.StopLocation |>  TotLns
        rr.Error |> ErrMsg
        rr.Warn |> WarnMsg
    |   PartialRepresentaton rr ->
        rr.StopLocation |>  TotLns
        rr.Warn |> WarnMsg
        printfn "%s" (Deserialize rr.Document (rr.TagShorthands))
    |   CompleteRepresentaton rr ->
        rr.StopLocation |>  TotLns
        rr.Warn |> WarnMsg
        printfn "%s" (Deserialize rr.Document (rr.TagShorthands))
    |   EmptyRepresentation rr ->
        printfn "Document was empty"
        rr.StopLocation |>  TotLns
        rr.Warn |> WarnMsg

let YamlParse s =
    let repr = (engine.``l-yaml-stream`` YamlExtended.Schema s)
    let crr = repr.Head
    PrintNode crr

let YamlParseList s =
    try
        let repr = (engine.``l-yaml-stream`` YamlExtended.Schema s)
        printfn "Total Documents: %d" (repr.Length)
        repr |> List.iter(fun crr ->
            PrintNode crr
            printfn "..."
        )
    with
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e

