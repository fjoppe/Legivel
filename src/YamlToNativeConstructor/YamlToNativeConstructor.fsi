module YamlToNativeConstructor

open System
open RepresentationGraph
open YamlParser.Common

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple=false, Inherited = false)>]
type YamlFieldAttribute  = 
    new : Name:string -> YamlFieldAttribute
    inherit System.Attribute

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property ||| AttributeTargets.Field, AllowMultiple=false, Inherited = false)>]
type YamlValueAttribute = 
    new : Id:string -> YamlValueAttribute
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

