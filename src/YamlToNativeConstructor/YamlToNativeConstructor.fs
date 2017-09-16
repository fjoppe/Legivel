module YamlToNativeConstructor

open System
open YamlParse
open TagResolution
open RepresentationGraph
open Microsoft.FSharp.Reflection
open System.Reflection


type ConstructionSettings = {
        UsePrimitiveDefaultWhenMissing : bool   // use default value for missing source data (primitive types only)
    }


type YamlMapping = {
        YamlTag     : GlobalTag
        TargetType  : Type
        ToNative    : (string -> obj)
    }
    with
        static member Create (yt, tt, tn) = { YamlTag = yt; TargetType = tt; ToNative = tn}


let YamlTypeMappings = [
    YamlMapping.Create (YamlExtended.StringGlobalTag, typeof<string>, fun (s:string) -> box s)
    YamlMapping.Create (YamlExtended.IntegerGlobalTag, typeof<int>, fun (s:string) -> YamlExtended.IntegerGlobalTag.ToCanonical s |> Option.get |> Int32.Parse |> box)
    YamlMapping.Create (YamlExtended.FloatGlobalTag, typeof<float>, fun (s:string) -> YamlExtended.FloatGlobalTag.ToCanonical s |> Option.get |> Double.Parse |> box)
    YamlMapping.Create (YamlExtended.BooleanGlobalTag, typeof<bool>, fun (s:string) -> YamlExtended.BooleanGlobalTag.ToCanonical s |> Option.get |> Boolean.Parse |> box)
    ]


//  https://stackoverflow.com/questions/44804767/f-create-custom-attribute-to-expression
type YamlFieldAttribute(Name : string)  = 
    inherit System.Attribute()
    member this.Name' = Name

type RefTypeInfo = {
    Constructor     : ConstructorInfo
}


let AreTypesEqual (t1:Type) (t2:Type) =
    let c1 = sprintf "%s%s" t1.Namespace t1.Name
    let c2 = sprintf "%s%s" t2.Namespace t2.Name
    c1 = c2

let getMapNode (n:Node) =
    match n with
    |   MapNode n ->  n 
    |   _    -> failwith "Expecting a mapping node"

let getSeqNode (n:Node) =
    match n with
    |   SeqNode n ->  n 
    |   _    -> failwith "Expecting a Sequence Node"

let getScalarNode (n:Node) =
    match n with
    |   ScalarNode n ->  n 
    |   _    -> failwith "Expecting a Scalar Node"

let GetCustomAttribute<'T when 'T :> Attribute> (st:MemberInfo) =
    let at = Attribute.GetCustomAttributes(st, typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> None
    |   1   -> Some (at.Head :?> 'T)
    |   _   -> failwith (sprintf "'%s.%s' has more than one attributes of type '%s'" (st.MemberType.ToString()) (st.Name) (typeof<'T>.FullName))



type NodeKindToIdiomaticMapper = (NodeKindToIdiomaticMappers -> Type -> YamlToNativeMapping option)
and NodeKindToIdiomaticMappers = private {
        PotentialMappers : NodeKindToIdiomaticMapper list
    }
    with
        static member Create ml = {PotentialMappers = ml}
        member this.TryFindMapper (t:Type) =
            this.PotentialMappers
            |>  List.choose(fun pmf -> pmf this t)
            |>  function
                |   []  -> failwith (sprintf "Unsupported: no conversion for: %s.%s" (t.MemberType.GetType().FullName) (t.FullName))
                |   [e] -> e
                |   _ -> failwith(sprintf "Ambigous: too many converters found for: %s.%s" (t.MemberType.GetType().FullName) (t.FullName))


and PrimitiveMappingInfo = {
        ScalarMapping   : YamlMapping
    }
    with
        static member TryFindMapper (mappers:NodeKindToIdiomaticMappers) (t:Type) =
            YamlTypeMappings
            |> List.tryFind(fun yt -> AreTypesEqual t yt.TargetType) 
            |> Option.map(fun yt -> PrimitiveMapping {ScalarMapping = yt })

        member this.map (n:Node) =
            if n.NodeTag.Uri = this.ScalarMapping.YamlTag.Uri then
                let scalar = getScalarNode n
                this.ScalarMapping.ToNative scalar.Data
            else
                failwith (sprintf "Incompatible type '%s' for tag: %s" n.NodeTag.Uri this.ScalarMapping.TargetType.Name)

        member this.Default
            with get() = Activator.CreateInstance(this.ScalarMapping.TargetType) |> box


and FieldMapping = {
        YamlName        : string
        PropertyName    : string
        PropertyMapping : YamlToNativeMapping
    }

and RecordMappingInfo = {
        Target       : Type
        FieldMapping : FieldMapping list
    }
    with
        static member TryFindMapper (mappers:NodeKindToIdiomaticMappers) (t:Type) =
            if FSharpType.IsRecord t then
                let mappings = 
                    FSharpType.GetRecordFields t
                    |>  List.ofArray
                    |>  List.map(fun fld ->
                        let yamlName = 
                            GetCustomAttribute<YamlFieldAttribute> fld
                            |>  function
                                |   None    -> fld.Name
                                |   Some ya -> ya.Name'
                        { YamlName = yamlName; PropertyName = fld.Name; PropertyMapping = mappers.TryFindMapper fld.PropertyType } )
                RecordMapping { Target = t; FieldMapping = mappings } |> Some
            else
                None

        member this.map (n:Node) =
            let mn = getMapNode n
            let dataToFieldMapping =
                mn.Data
                |>  List.choose(fun (k,v) ->
                    if k.NodeTag.Uri = YamlExtended.StringGlobalTag.Uri then
                        let kf = getScalarNode k
                        this.FieldMapping
                        |>  List.tryFind(fun fm -> fm.YamlName = kf.Data)
                        |>  Option.map(fun fm -> kf.Data, fm.PropertyMapping.map v)
                    else
                        None
                )
                |>  Map.ofList
            let values = 
                this.FieldMapping
                |>  List.map(fun fm ->
                    if dataToFieldMapping.ContainsKey fm.YamlName then
                        dataToFieldMapping.[fm.YamlName]
                    else
                        fm.PropertyMapping.Default
                )
            FSharpValue.MakeRecord(this.Target,values |> List.toArray)

        member this.Default with get() = failwith "Unsupported: default record value"


and OptionalMappingInfo = {
        OptionType    : Type
        OptionMapping : YamlToNativeMapping
    }
    with
        static member TryFindMapper (mappers:NodeKindToIdiomaticMappers) (t:Type) =
            let IsOptional (t:Type) = AreTypesEqual typeof<FSharp.Core.Option<obj>> t
            if IsOptional t then
                let mapping = mappers.TryFindMapper t.GenericTypeArguments.[0]
                OptionMapping {OptionType=t; OptionMapping = mapping} |> Some
            else
                None

        member this.map (n:Node) = 
            let ctor = this.OptionType.GetConstructors() |> Array.head
            ctor.Invoke([| this.OptionMapping.map n |]) |> box

        member this.Default with get() = None |> box

and ListMappingInfo = {
        ListType    : Type
        ListMapping : YamlToNativeMapping
    }
    with
        static member TryFindMapper (mappers:NodeKindToIdiomaticMappers) (t:Type) =
            let IsList (t:Type) = AreTypesEqual typeof<FSharp.Collections.List<obj>> t
            if IsList t then
                let mapping = mappers.TryFindMapper t.GenericTypeArguments.[0]
                ListMapping {ListType = t; ListMapping = mapping} |> Some
            else
                None

        member this.map (n:Node) = 
            let sn = getSeqNode n
            sn.Data
            |>  List.rev
            |>  List.fold(fun s e -> s.GetType().GetMethod("Cons").Invoke(null, [|this.ListMapping.map e;s|])) this.Default
            |>  box

        member this.Default
            with get() :obj = 
                this.ListType.GetProperty("Empty", BindingFlags.Static ||| BindingFlags.Public).GetGetMethod().Invoke(null, [||]) 
                |> box


and YamlToNativeMapping =
    |   PrimitiveMapping of PrimitiveMappingInfo
    |   RecordMapping of RecordMappingInfo
    |   OptionMapping of OptionalMappingInfo
    |   ListMapping of ListMappingInfo
    with
        member this.map (n:Node) =
            match this with
            |   PrimitiveMapping pm -> pm.map n
            |   RecordMapping rm    -> rm.map n
            |   OptionMapping om    -> om.map n
            |   ListMapping lm      -> lm.map n

        member this.Default 
            with get() =
                match this with
                |   PrimitiveMapping pm -> pm.Default
                |   RecordMapping rm    -> rm.Default
                |   OptionMapping om    -> om.Default
                |   ListMapping lm      -> lm.Default

let TryFindMappers =
    [
        PrimitiveMappingInfo.TryFindMapper
        RecordMappingInfo.TryFindMapper
        OptionalMappingInfo.TryFindMapper
        ListMappingInfo.TryFindMapper
    ]



let CreateTypeMappings<'tp>() =
    let tryFindMapper = NodeKindToIdiomaticMappers.Create TryFindMappers
    tryFindMapper.TryFindMapper typeof<'tp>


let Deserialize<'tp> yml : 'tp option =
    let yamlParser = Yaml12Parser()
    let mappings = CreateTypeMappings<'tp>()

    let ymlpl = (yamlParser.``l-yaml-stream`` YamlExtended.Schema yml) |> List.head
    match ymlpl with
    |   NoRepresentation err -> failwith "Exited with parse errors"
    |   EmptyRepresentation mt -> None
    |   PartialRepresentaton pdr 
    |   CompleteRepresentaton pdr -> 
            mappings.map (pdr.Document) :?> 'tp
            |> Some

