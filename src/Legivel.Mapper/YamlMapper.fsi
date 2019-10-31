module Legivel.Serialization

open Legivel.RepresentationGraph
open Legivel.Common

type MapYaml =
    /// The model is leading and defines the minimal yaml required for succesful deserialization. Any unrecognized yaml is ignored.
    |   ToModelOnly = 0
    /// The model and yaml are desired to be eachothers projection. Significant unrecognized yaml is reported as warning.
    |   WithCrossCheck = 1
    /// The model and yaml must be eachothers exact projection. Significant difference is reported as an error.
    |   AndRequireFullProjection = 2

type ProcessingOption =
    ///  How strict should the yaml be mapped to the target model.
    |   MappingMode of MapYaml


/// Returned when deserialization was succesful
type SuccessInfo<'tp> = {
        Data : 'tp
        Warn : ParseMessageAtLine list
    }
    with
        static member Create<'a> : 'a -> ParseMessageAtLine list -> SuccessInfo<'a>


/// Returned when deserialization contained errors
type ErrorInfo = {
        Warn  : ParseMessageAtLine list 
        Error : ParseMessageAtLine list
        StopLocation : DocumentLocation
    }
    with
        static member Create : ParseMessageAtLine list -> ParseMessageAtLine list -> DocumentLocation -> ErrorInfo


/// Deserialization result
type DeserializeResult<'tp> =
    |   Success of SuccessInfo<'tp>
    |   Error of ErrorInfo

val DeserializeWithOptions<'tp> : options:ProcessingOption list -> yaml:string  -> DeserializeResult<'tp> list

/// Deserialize yaml-string to the provided type
val Deserialize<'tp> : yaml:string -> DeserializeResult<'tp> list

