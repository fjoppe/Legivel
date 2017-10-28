/// Provides functions to deserialize yaml
module Legivel.Serialization

open Legivel.Common
open Legivel.TagResolution
open Legivel.RepresentationGraph
open Legivel.Customization.Mapping


//type ProcessingOptions = {
//        UsePrimitiveDefaultWhenMissing : bool   // use default value for missing source data (primitive types only)
//    }


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


let Deserialize<'tp> yml : DeserializeResult<'tp> list =
    CustomDeserializeYaml BuildInTryFindMappers MapYamlDocumentToNative ParseYamlToNative (YamlCore.Schema) (YamlCore.NullGlobalTag.Uri) yml
    |>  List.map(fun r ->
        match r with
        |   Processed d -> Succes (SuccessInfo<'tp>.Create d.Data d.Warn)
        |   WithErrors d -> Error  (ErrorInfo.Create d.Error d.Warn d.StopLocation)
    )


