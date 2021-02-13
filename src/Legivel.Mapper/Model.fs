module Legivel.Mapper.Model

open System
open Legivel.Common
open Legivel.RepresentationGraph
open Legivel.Customization.Utilities

type NodePathType = string list


type NodeTypeTree =
    |   ExpectedTag of TagKind
    |   InStructure of YTMRef

/// Base type for any yaml to native mapping, for simple and compex types.
and IYamlToNativeMapping =
    abstract member GetTagFor : Node -> EventNodeKind -> string list -> (NodeTypeTree * (string list)) option

    /// Map a Node to the target type-instance (boxed into type obj)
    abstract member map : msgList:ProcessMessages -> am:AllTryFindIdiomaticMappers -> n:Node -> FallibleOption<obj> * ProcessMessages

    /// Return a default value for the target type
    abstract member Default : FallibleOption<obj> with get

and YTMRef = int

and MappedTypes = private {
        TypeToRef: Map<string, YTMRef>
        RefToMapper: Map<YTMRef, IYamlToNativeMapping>
        CurrentRef : YTMRef
    }
    with
        static member Create() = { TypeToRef = Map.empty; RefToMapper = Map.empty; CurrentRef = 0}

        member private this.TypeString (t:Type) = t.FullName

        member this.RegisterType t = 
            let typeId = this.TypeString t
            let ref = this.CurrentRef + 1
            ref, { this with TypeToRef = this.TypeToRef.Add(typeId, ref); CurrentRef = ref}  

        member this.RegisterMapper r m = 
            { this with RefToMapper = this.RefToMapper.Add(r, m)}  

        member this.HasType t = 
            let typeId = this.TypeString t
            this.TypeToRef.ContainsKey typeId

        member this.HasMapper r = 
            this.RefToMapper.ContainsKey r

        member this.GetRef t =
            let typeId = this.TypeString t
            this.TypeToRef.[typeId]

        member this.GetMapper r = this.RefToMapper.[r]

and FoundMappers = {
    Ref     : YTMRef
    Mappers : AllTryFindIdiomaticMappers
}
with
    static member Create r m = { Ref = r; Mappers = m}

and FindMapperParams = {
    MessageList : ProcessMessages 
    Mappers     : AllTryFindIdiomaticMappers 
    CurrentType : Type
}
with
    static member Create ml mp t = { MessageList = ml; Mappers = mp; CurrentType = t }

    member this.GetExistingType() =
        if this.Mappers.HasType this.CurrentType then
            let ref = this.Mappers.GetRef this.CurrentType
            (FallibleOption.Value (FoundMappers.Create ref this.Mappers), this.MessageList) |>  Some
        else
            None


/// The return type of a TryFindMapper function
and TryFindMapperReturnType = FallibleOption<FoundMappers>*ProcessMessages

/// signature of a TryFindMapper function, which may return a mapping construct for the given native type
and TryFindIdiomaticMapperForType = (FindMapperParams -> TryFindMapperReturnType)

/// Structure containing all TryFindMapper functions available
and AllTryFindIdiomaticMappers = private {
        PotentialMappers : TryFindIdiomaticMapperForType list
        NullTagUri'      : GlobalTag
        StringTagUri'    : GlobalTag
        KnownTypes       : MappedTypes
    }
    with
        static member Create ml nt st = {PotentialMappers = ml; NullTagUri' = nt; StringTagUri' = st; KnownTypes=MappedTypes.Create()}

        /// Try to find a mapper for the given type, look in all potential mappers
        member this.TryFindMapper (msgList:ProcessMessages) (t:Type) : TryFindMapperReturnType =
            let findParams = FindMapperParams.Create msgList this t
            this.PotentialMappers
            |>  List.tryFindFo msgList (fun pmf -> 
                match findParams.GetExistingType() with
                |   Some v -> v
                |   None -> pmf findParams
            )
            |>  fun (foundMapper, pm) ->
                match foundMapper.Result with
                |   FallibleOptionValue.NoResult -> AddError msgList (ParseMessageAtLine.Create NoDocumentLocation (sprintf "Unsupported: no conversion for: %s.%s" (t.MemberType.GetType().FullName) (t.FullName)))
                |   FallibleOptionValue.Value    -> FallibleOption.Value (foundMapper.Data), msgList
                |   _ -> failwith (sprintf "Error finding parser for type: %s.%s" (t.MemberType.GetType().FullName) (t.FullName))

        member this.GetMapper r = this.KnownTypes.GetMapper r

        member this.map e r = (this.GetMapper r).map e this

        member this.RegisterType t =
            let (r,kt) = this.KnownTypes.RegisterType t
            r, { this with KnownTypes = kt}

        member this.RegisterMapper r m =
            let kt = this.KnownTypes.RegisterMapper r m
            { this with KnownTypes = kt}

        member this.HasType t = this.KnownTypes.HasType t

        member this.HasMapper r = this.KnownTypes.HasMapper r

        member this.GetRef t = this.KnownTypes.GetRef t

        member this.StringTagUri with get() = this.StringTagUri'

        member this.NullTagUri with get() = this.NullTagUri'


