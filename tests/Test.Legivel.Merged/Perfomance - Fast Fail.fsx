#I __SOURCE_DIRECTORY__ 

#time

#r @"bin/Debug/net45/FSharp.Core.dll"
#r @"bin/Debug/net45/Legivel.Parser.dll"
#r @"bin/Debug/net45/NLog.dll"

open System.Text

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

let engine = Yaml12Parser(Failsafe.Schema)
engine.SetLogFunc (logger.Trace)

let WarnMsg (sl:ParseMessageAtLine list) = sl |> List.iter(fun s -> printfn "Warn: %d %d: %s" (s.Location.Line) (s.Location.Column) (s.Message))
let ErrMsg  (sl:ParseMessageAtLine list) = sl |> List.iter(fun s -> printfn "ERROR: %d %d:%s" (s.Location.Line) (s.Location.Column) (s.Message))
let TotLns (ps:DocumentLocation) = printfn "Total lines: %d" ps.Line

let PrintNode crr =
    match crr with
    |   NoRepresentation rr ->
        printfn "Cannot parse:" 
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
        //PrintNode crr
        ()
    with
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e


let sb = StringBuilder()

sb.Append("TestKey:\n")

//  a few phrases from: https://www.gutenberg.org/files/1184/1184-0.txt
let randomPhrases = [
    "- 'When the young man on board saw this person approach, he left his'\n"
    "- 'much disliked by the crew as Edmond Dantès was beloved by them.'\n"
    "- 'course she had taken, and what was her cargo. I believe, if she had not'\n"
    "- 'three months after that; only be back again in three months, for the'\n"
]

let cartesionProduct sl ll =
    let rec innerLoop ll csl acc =
        match (ll,csl) with
        |   ([], _) -> acc
        |   (_, []) -> innerLoop ll sl acc
        |   (hl::rl, hs::rs) -> innerLoop rl rs ((hl,hs)::acc)
    innerLoop ll sl []

    
[0..23000]
|>  cartesionProduct randomPhrases
|>  List.map(fun (_, s) -> s)
|>  List.fold(fun (s:StringBuilder) (i:string)  -> s.Append(i)) sb
|>  ignore


YamlParse (sb.ToString())


