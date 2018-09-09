#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#time

#r @"bin/Debug/Legivel.Common.dll"
#r @"bin/Debug/Legivel.RepresentationGraph.dll"
#r @"bin/Debug/Legivel.Parser.dll"

//#r @"bin/Release/Legivel.Common.dll"
//#r @"bin/Release/Legivel.RepresentationGraph.dll"
//#r @"bin/Release/Legivel.Parser.dll"

#r @"test/NLog/lib/net45/NLog.dll"
open System
open System.Globalization
open Legivel.Parser
open Legivel.TagResolution
open Legivel.Serialization
open Legivel.RepresentationGraph
open Legivel.Common
open NLog
open System.IO

#load "nlog.fsx"

open System
open System.Globalization

NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let logger = LogManager.GetLogger("*")

let engine = Yaml12Parser(JSON.Schema, fun s -> logger.Trace(s))

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
        let repr = (engine.``l-yaml-stream`` s)
        let crr = repr.Head
        PrintNode crr
    with
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e

let YamlParseList s =
    try
        let repr = (engine.``l-yaml-stream`` s)
        printfn "Total Documents: %d" (repr.Length)
        repr |> List.iter(fun crr ->
            PrintNode crr
            printfn "..."
        )
    with
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e

let YamlParseWithErrors s =
    try
        let repr = (engine.``l-yaml-stream`` s)
        let crrp = repr.Head
        match crrp with
        |   NoRepresentation nr -> 
            nr
        |   _ -> failwith "Unexpected return type"

    with
    | e -> printfn "%A" e; raise e

//let s = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "ec2-swagger.yaml"))

//YamlParse s

//  ``Example 8.3. Invalid Block Scalar Indentation Indicators``
YamlParseWithErrors "
- >
  text
 text" 








