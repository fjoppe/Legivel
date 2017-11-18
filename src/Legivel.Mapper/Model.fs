module Legivel.Mapper.Model

open System
open Legivel.Common
open Legivel.RepresentationGraph


/// Base type for any yaml to native mapping, for simple and compex types.
type IYamlToNativeMapping =
    /// Map a Node to the target type-instance (boxed into type obj)
    abstract member map : n:Node -> FallibleOption<obj, ParseMessageAtLine list>

    /// Return a default value for the target type
    abstract member Default : FallibleOption<obj, ParseMessageAtLine list> with get


type MappedTypes = private {
        TypeToMapper: Map<string, IYamlToNativeMapping>
    }
    with
        static member Create() = { TypeToMapper = Map.empty}

        member private this.TypeString (t:Type) = sprintf "%s%s" t.Namespace t.Name

        member this.RegisterTypeMapping t m = 
            let typeId = this.TypeString t
            { this with TypeToMapper = this.TypeToMapper.Add(typeId, m)}  

        member this.HasMapper t = 
            let typeId = this.TypeString t
            this.TypeToMapper.ContainsKey typeId

        member this.GetMapper t =
            let typeId = this.TypeString t
            this.TypeToMapper.[typeId]

