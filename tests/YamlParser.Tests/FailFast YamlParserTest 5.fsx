#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#r @"bin/Debug/YamlParser.dll"
#r @"NLog/lib/net45/NLog.dll"

open YamlParse
open TagResolution
open Deserialization
open RepresentationGraph
open YamlParser.Internals
open NLog


#load "nlog.fsx"
NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let logger = LogManager.GetLogger("*")

let engine = Yaml12Parser(fun s -> logger.Trace(s))

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
    try
        let repr = (engine.``l-yaml-stream`` YamlExtendedSchema s)
        let crr = repr.Head
        PrintNode crr
    with
    | DocumentException e -> 
        e.Messages.Warn  |> List.iter(fun s -> printfn "Warn: %d %d: %s" (s.Location.Line) (s.Location.Column) (s.Message))
        e.Messages.Error |> List.iter(fun s -> printfn "ERROR: %d %d:%s" (s.Location.Line) (s.Location.Column)  (s.Message))
        raise (DocumentException e)
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e

let YamlParseList s =
    try
        let repr = (engine.``l-yaml-stream`` YamlExtendedSchema s)
        printfn "Total Documents: %d" (repr.Length)
        repr |> List.iter(fun crr ->
            PrintNode crr
            printfn "..."
        )
    with
    | DocumentException e -> 
        e.Messages.Warn  |> List.iter(fun s -> printfn "Warn: %d %d: %s" (s.Location.Line) (s.Location.Column) (s.Message))
        e.Messages.Error |> List.iter(fun s -> printfn "ERROR: %d %d:%s" (s.Location.Line) (s.Location.Column)  (s.Message))
        raise (DocumentException e)
    | e -> printfn "%A:%A\n%A" (e.GetType()) (e.Message) (e.StackTrace); raise e



YamlParse "
    {
        ? !!omap [ a : 1, b : 1] 
        : value,
        ? !!omap [b : 1,  a : 1] 
        : value
    }
"



YamlParse "# Explicitly typed pairs.
Block tasks: !!pairs
  - meeting: with team.
  - meeting: with boss.
  - break: lunch.
  - meeting: with client.
  - meeting
Flow tasks: !!pairs [ meeting: with team, meeting: with boss ]"


YamlParse "# Explicitly typed set.
baseball players: !!set
  ? Mark McGwire
  ? Sammy Sosa
  ? Ken Griffey
# Flow style
baseball teams: !!set { Boston Red Sox, Detroit Tigers, New York Yankees }"

YamlParse "# Explicitly typed set.
baseball players: 
  ? Mark McGwire
  ? Sammy Sosa
  ? Ken Griffey
# Flow style
baseball teams: { Boston Red Sox, Detroit Tigers, New York Yankees }"

YamlParse "# Explicitly typed set.
baseball players: !!set
  ? Mark McGwire
  ? Sammy Sosa
  ? Ken Griffey
  ? Ken Griffey
# Flow style
baseball teams: !!set { Boston Red Sox, Detroit Tigers, New York Yankees }"

YamlParse "# Explicitly typed set.
baseball players: !!set
  ? Mark McGwire
  ? Sammy Sosa
  ? Ken Griffey
  : 1
# Flow style
baseball teams: !!set { Boston Red Sox, Detroit Tigers, New York Yankees }"

YamlParseList "# A document may be null.
---
---
# This mapping has four keys,
# one has a value.
empty:
canonical: ~
english: null
~: null key
---
# This sequence has five
# entries, two have values.
sparse:
  - ~
  - 2nd entry
  -
  - 4th entry
  - Null"


open RegexDSL

let patt = (RGP "\[")
let str = "[\r\n: [ 2001-07-02, 2001-08-12,\r\n    2001-08-14 ]\r\n"

IsMatch(str, patt)


type    DocumentLocation = {
        Line    : int
        Column  : int
    }


let a1 = {Line = 1; Column = 5}
let a2 = {Line = 1; Column = 4}

a1 < a2
