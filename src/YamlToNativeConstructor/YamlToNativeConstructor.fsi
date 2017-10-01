module YamlToNativeConstructor

open RepresentationGraph
open YamlParser.Common

type YamlFieldAttribute  = 
    new : Name:string -> YamlFieldAttribute
    inherit System.Attribute

type SuccessInfo<'tp> = {
        Data : 'tp
        Warn : ParseMessageAtLine list
        StopLocation : DocumentLocation
    }


type ErrorInfo = {
        Warn  : ParseMessageAtLine list 
        Error : ParseMessageAtLine list
        StopLocation : DocumentLocation
    }


type DeserializeResult<'tp> =
    |   Succes of SuccessInfo<'tp>
    |   Error of ErrorInfo

val Deserialize<'tp> : yaml:string -> DeserializeResult<'tp>

