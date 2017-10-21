module Legivel.Mapper

open Legivel.Parser
open Legivel.Common
open Legivel.TagResolution
open Legivel.RepresentationGraph
open Legivel.Customization.Utilities
open Legivel.Customization.Mapping

//let NoDocumentLocation = (DocumentLocation.Create 0 0)


type ConstructionSettings = {
        UsePrimitiveDefaultWhenMissing : bool   // use default value for missing source data (primitive types only)
    }




//module Legivel=


type SuccessInfo<'tp> = {
        Data : 'tp
        Warn : ParseMessageAtLine list
        StopLocation : DocumentLocation
    }
    with
        static member Create d w sl =  {Data = d; Warn = w; StopLocation = sl}


type ErrorInfo = {
        Warn  : ParseMessageAtLine list 
        Error : ParseMessageAtLine list
        StopLocation : DocumentLocation
    }
    with
        static member Create e w sl = { Warn = w; Error = e ; StopLocation = sl}

type DeserializeResult<'tp> =
    |   Succes of SuccessInfo<'tp>
    |   Error of ErrorInfo


let DeserializeWithMappers<'tp> (tryFindMappers:TryFindIdiomaticMapperForType list) yml : DeserializeResult<'tp> list =
    let yamlParser = Yaml12Parser()
    CreateTypeMappings<'tp> tryFindMappers
    |>  function
        |   NoResult -> [ErrorInfo.Create ([ParseMessageAtLine.Create (DocumentLocation.Create 0 0) "Cannot find yaml to type mappers"]) [] NoDocumentLocation |> Error]
        |   ErrorResult e -> [ErrorInfo.Create (e) [] NoDocumentLocation |> Error]
        |   Value mappings ->
            (yamlParser.``l-yaml-stream`` YamlExtended.Schema yml) 
            |> List.map(fun ymlpl ->
                match ymlpl with
                |   NoRepresentation err -> ErrorInfo.Create (err.Error) (err.Warn) (err.StopLocation) |> Error
                |   EmptyRepresentation mt -> ErrorInfo.Create ([ParseMessageAtLine.Create NoDocumentLocation  "Document was empty"]) (mt.Warn) (mt.StopLocation) |> Error
                |   PartialRepresentaton pdr
                |   CompleteRepresentaton pdr -> 
                        mappings.map (pdr.Document)
                        |>  function
                            |   NoResult -> ErrorInfo.Create ([ParseMessageAtLine.Create NoDocumentLocation  "Document cannot be mapped"]) (pdr.Warn) (pdr.StopLocation) |> Error
                            |   ErrorResult e -> ErrorInfo.Create (e) (pdr.Warn) (pdr.StopLocation) |> Error
                            |   Value v -> 
                                let d = unbox<'tp> v
                                SuccessInfo<'tp>.Create d (pdr.Warn) (pdr.StopLocation) |> Succes
            )

let Deserialize<'tp> yml : DeserializeResult<'tp> list =
    DeserializeWithMappers<'tp> BuildInTryFindMappers yml

