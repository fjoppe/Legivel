module TestUtils

open System
open YamlParse
open RepresentationGraph
open TagResolution
open Deserialization

let YamlParse s =
    let engine = Yaml12Parser()
    try
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Value
        let (nodes, ps) = pr
        let node = nodes.Head
        printfn "%s" (Deserialize node (ps.TagShorthands))
        node
    with
    | e -> printfn "%A" e; raise e

let YamlParseList s =
    let engine = Yaml12Parser()
    try
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Value
        let (nodes, ps) = pr
        nodes |> List.iter(fun node -> printfn "%s" (Deserialize node (ps.TagShorthands)))
        nodes
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
        |   Local  s    -> s
        |   NonSpecific s -> s
    match n with
    |   Some([ScalarNode nd]) -> extract nd
    |   Some([SeqNode nd]) -> extract nd
    |   Some([MapNode nd]) -> extract nd
    |   _ -> raise (Exception "Not recognized as single node")


