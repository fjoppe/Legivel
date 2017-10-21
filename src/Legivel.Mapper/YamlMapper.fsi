module Legivel.Mapper

open System
open Legivel.RepresentationGraph
open Legivel.Common
open Legivel.Internals
open Legivel.Customization.Mapping

/// Used in generating error messages, when there is no document location available
//val NoDocumentLocation : DocumentLocation


///// Used to map a yaml value to a field-name; ie when a yaml value is different than the record fieldname it maps to
//[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple=false, Inherited = false)>]
//type YamlFieldAttribute  = 
//    new : Name:string -> YamlFieldAttribute
//    inherit System.Attribute


///// Used to map a yaml value to a union-case; ie when the yaml value is different than the union-case name, it maps to
//[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property ||| AttributeTargets.Field, AllowMultiple=false, Inherited = false)>]
//type YamlValueAttribute = 
//    new : Id:string -> YamlValueAttribute
//    inherit System.Attribute



///// Base type to construct a yaml to native mapper
//type IYamlToNativeMapping =
//    abstract member map : n:Node -> FallibleOption<obj, ParseMessageAtLine list>
//    abstract member Default : FallibleOption<obj, ParseMessageAtLine list> with get


///// Return type signature of a TryFindMapper function
//type internal TryFindMapperReturnType = FallibleOption<IYamlToNativeMapping,ParseMessageAtLine list>


///// a function which tries to find an Idiomatic mapper for a specific type, or type group (ie all primitives)
//type TryFindIdiomaticMapperForType = (AllTryFindIdiomaticMappers -> Type -> FallibleOption<IYamlToNativeMapping,ParseMessageAtLine list>)


///// a container which stores all functions, which try to find an idiomatic mapper; this is used to setup the mappers-tree for nested types
//and AllTryFindIdiomaticMappers  
//and AllTryFindIdiomaticMappers  with
//    /// Ty to find an idiomatic mapper for a given type
//    member TryFindMapper : t:Type -> TryFindMapperReturnType


///// The build-in set of TryFindIdiomaticMapperForType functions
//val BuildInTryFindMappers : TryFindIdiomaticMapperForType list


//module Legivel=
/// Returned when deserialization in succesful
type SuccessInfo<'tp> = {
        Data : 'tp
        Warn : ParseMessageAtLine list
        StopLocation : DocumentLocation
    }

/// Returned when deserialization contained errors
type ErrorInfo = {
        Warn  : ParseMessageAtLine list 
        Error : ParseMessageAtLine list
        StopLocation : DocumentLocation
    }

/// Deserialization return type
type DeserializeResult<'tp> =
    |   Succes of SuccessInfo<'tp>
    |   Error of ErrorInfo

/// Deserialize yaml with the given set of yaml-to-native mappers
val DeserializeWithMappers<'tp> : tryFindMappers : TryFindIdiomaticMapperForType list -> yaml:string -> DeserializeResult<'tp> list


/// Deserialize yaml with the build-in yaml-to-native mappers
val Deserialize<'tp> : yaml:string -> DeserializeResult<'tp> list

