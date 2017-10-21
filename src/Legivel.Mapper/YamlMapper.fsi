module Legivel.Serialization

open Legivel.RepresentationGraph
open Legivel.Common


/// Returned when deserialization was succesful
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

/// Deserialization result
type DeserializeResult<'tp> =
    |   Succes of SuccessInfo<'tp>
    |   Error of ErrorInfo

/// Deserialize yaml-string to the provided type
val Deserialize<'tp> : yaml:string -> DeserializeResult<'tp> list

