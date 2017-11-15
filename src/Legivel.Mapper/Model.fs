module Legivel.Mapper.Model

open Legivel.Common
open Legivel.RepresentationGraph

/// Base type for any yaml to native mapping, for simple and compex types.
type IYamlToNativeMapping =
    /// Map a Node to the target type-instance (boxed into type obj)
    abstract member map : n:Node -> FallibleOption<obj, ParseMessageAtLine list>

    /// Return a default value for the target type
    abstract member Default : FallibleOption<obj, ParseMessageAtLine list> with get

type ReferenceGenerator = private {
        id : int
    }
    with
        static member Create() = { id = 1 }
        member this.CreateReference() = (this.id,{ this with id = this.id + 1})

type MappedTypes = private {
        TypeToRef   : Map<string, int>
        RefToMapper : Map<int,IYamlToNativeMapping>
        RefGen      : ReferenceGenerator
    }
    with
        static member Create() = { TypeToRef = Map.empty; RefToMapper = Map.empty ; RefGen = ReferenceGenerator.Create() }
        //member this.RegisterType t = 
