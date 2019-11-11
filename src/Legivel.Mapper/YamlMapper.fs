/// Provides functions to deserialize yaml
module Legivel.Serialization

open Legivel.Common
open Legivel.TagResolution
open Legivel.RepresentationGraph
open Legivel.Customization.Mapping


type MapYaml =
    |   ToModelOnly = 0
    |   WithCrossCheck = 1
    |   AndRequireFullProjection = 2

type ProcessingOption =
    |   MappingMode of MapYaml


let ConvertMapYamlToInternal v =
    match v with
    |   MapYaml.ToModelOnly     -> CrossMatch.None
    |   MapYaml.WithCrossCheck  -> CrossMatch.Warn
    |   MapYaml.AndRequireFullProjection -> CrossMatch.Error
    |   _ -> failwith "Unrecognized value"


let ParseOptions pl =
    let ``default`` = {
        CrossMatch = CrossMatch.None
    }
    let ``parse to settings object`` s po =
        match po with
        |   MappingMode(v) -> { s with CrossMatch = ConvertMapYamlToInternal v }
    pl
    |>  List.fold(``parse to settings object``) ``default``


type SuccessInfo<'tp> = {
        Data : 'tp
        Warn : ParseMessageAtLine list
    }
    with
        static member Create d w =  {Data = d; Warn = w}


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


let DeserializeWithOptions<'tp> (options : ProcessingOption list) yml : DeserializeResult<'tp> list =
    CustomDeserializeYaml (BuildInTryFindMappers (ParseOptions options) YamlScalarToNativeMappings) MapYamlDocumentToNative ParseYamlToNative (Legivel.Customization.Mapping.YamlMapped.Schema) (YamlExtended.NullGlobalTag) (YamlExtended.StringGlobalTag) yml
    |>  List.map(fun r ->
        match r with
        |   Processed d -> Succes (SuccessInfo<'tp>.Create d.Data d.Warn)
        |   WithErrors d -> Error  (ErrorInfo.Create d.Error d.Warn d.StopLocation)
    )

let Deserialize<'tp> yml : DeserializeResult<'tp> list = DeserializeWithOptions [] yml

