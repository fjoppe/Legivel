module TestUtils

open System
open YamlParse
open RepresentationGraph
open TagResolution
open FsUnit

let YamlParse s =
    let engine = Yaml12Parser()
    try
        let repr = (engine.``l-yaml-stream`` YamlCoreSchema s)
        let crrp = repr.Head
        match crrp with
        |   CompleteRepresentaton cr -> cr.Document
        |   _ -> failwith "Unexpected return type"
    with
    | e -> printfn "%A" e; raise e

let YamlParseWithErrors s =
    let engine = Yaml12Parser()
    try
        let repr = (engine.``l-yaml-stream`` YamlCoreSchema s)
        let crrp = repr.Head
        match crrp with
        |   NoRepresentation nr -> 
            nr.Error.Length |> should greaterThan 0
            nr
        |   _ -> failwith "Unexpected return type"

    with
    | e -> printfn "%A" e; raise e

let YamlParseWithWarning s =
    let engine = Yaml12Parser()
    try
        let repr = (engine.``l-yaml-stream`` YamlCoreSchema s)
        let crrp = repr.Head
        match crrp with
            |   NoRepresentation _ -> failwith "Unexpected error"
            |   CompleteRepresentaton cr -> cr
            |   PartialRepresentaton pr -> pr
            |   EmptyRepresentation er -> failwith "Unexpected empty"
    with
    | e -> printfn "%A" e; raise e


let YamlParseEmpty s =
    let engine = Yaml12Parser()
    try
        let repr = (engine.``l-yaml-stream`` YamlCoreSchema s)
        let crrp = repr.Head
        match crrp with
        |   EmptyRepresentation _ -> true
        |   _ -> failwith "Unexpected return type"
    with
    | e -> printfn "%A" e; raise e

let YamlParseList s =
    let engine = Yaml12Parser()
    try
        let repr = (engine.``l-yaml-stream`` YamlCoreSchema s)
        repr |> List.map(fun e ->
            match e with
            |   NoRepresentation _ -> failwith "Unexpected error"
            |   CompleteRepresentaton cr -> cr.Document
            |   PartialRepresentaton pr -> pr.Document
            |   EmptyRepresentation er -> failwith "Unexpected empty"
        )
    with
    | e -> printfn "%A" e; raise e


let YamlParseForSchema sch s =
    let engine = Yaml12Parser()
    try
        let repr = (engine.``l-yaml-stream`` sch s)
        let crrp = repr.Head
        match crrp with
        |   CompleteRepresentaton cr -> cr.Document
        |   _ -> failwith "Unexpected return type"
    with
    | e -> printfn "%A" e; raise e


let ToScalar n = 
    match n with
    |   Some([ScalarNode nd]) -> nd.Data
    |   _ -> raise (Exception "Is no scalar")

let ToSequence n =
    match n with
    |   Some([SeqNode nd]) -> nd.Data
    |   _ -> raise (Exception "Is no seq")
    

let ExtractTag n = 
    let extract nd =
        match nd.Tag with
        |   Global gt   -> gt.Uri
        |   Unrecognized gt -> gt.Uri
        |   Local  s    -> s.Handle
        |   NonSpecific s -> s.Handle
    match n with
    |   Some([ScalarNode nd]) -> extract nd
    |   Some([SeqNode nd]) -> extract nd
    |   Some([MapNode nd]) -> extract nd
    |   _ -> raise (Exception "Not recognized as single node")


