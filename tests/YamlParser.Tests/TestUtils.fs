module TestUtils

open System
open YamlParse
open RepresentationGraph
open TagResolution
open FsUnit

let YamlParse s =
    let engine = Yaml12Parser()
    try
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Data
        let (nodes, ps) = pr
        let node = nodes.Head
        ps.Errors |> should equal 0
        node
    with
    | e -> printfn "%A" e; raise e

let YamlParseWithErrors s =
    let engine = Yaml12Parser()
    try
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Data
        let (nodes, ps) = pr
        let node = nodes.Head
        ps.Errors |> should greaterThan 0
        node
    with
    | e -> printfn "%A" e; raise e


let YamlParseList s =
    let engine = Yaml12Parser()
    try
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Data
        let (nodes, ps) = pr
        ps.Errors |> should equal 0
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


