module YamlToNativeConstructor

open System
open YamlParse
open YamlParser.Internals
open YamlParser.Common
open TagResolution
open RepresentationGraph
open Microsoft.FSharp.Reflection
open System.Reflection

let GetErrors l = l |> List.choose(fun pmf -> match pmf with | ErrorResult e -> Some e | _ -> None) |> List.collect(id)


type internal FallibleOption<'a,'b>
with
    member internal this.IsErrorResult =
        match this with
        |   ErrorResult _ -> true
        |   _ -> false

    member this.IsNoResult =
        match this with
        |   NoResult -> true
        |   _ -> false

module internal FallibleOption =
    let forCollection f =
        function
        |   NoResult -> [NoResult]
        |   ErrorResult e ->  [ErrorResult e]
        |   Value dt -> f dt

    let errorsOrValues f l =
        let errors = l |> GetErrors
        if errors.Length > 0 then
            ErrorResult errors
        else
            l
            |>  List.filter(fun mr -> not(mr.IsNoResult))  // this may hide errors
            |>  f

type SerieBuilder() =
    member this.Bind(mx: FallibleOption<'a,'c>, f: 'a -> FallibleOption<'b,'c>) : FallibleOption<'b,'c> =
        match mx with
        |   Value v -> f v
        |   NoResult -> NoResult
        |   ErrorResult e -> ErrorResult e

    member this.Return (x: 'a): FallibleOption<'a,'c> = Value x

let faillableSequence = new SerieBuilder()

module List =
    let choosefo<'a,'b, 'c> (f:'c->FallibleOption<'a,'b>) l = l |> List.map(f) |> List.filter(fun (i:FallibleOption<'a,'b>) -> not(i.IsNoResult))



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
    |   MapNode n ->  Value n
    |   _    -> ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a mapping node")]


let getSeqNode (n:Node) =
    match n with
    |   SeqNode n ->  Value n 
    |   _    -> ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a Sequence Node")]


let getScalarNode (n:Node) =
    match n with
    |   ScalarNode n ->  Value n
    |   _    -> ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a Scalar Node")]


let GetCustomAttribute<'T when 'T :> Attribute> (st:MemberInfo) =
    let at = Attribute.GetCustomAttributes(st, typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> NoResult
    |   1   -> Value (at.Head :?> 'T)
    |   _   -> ErrorResult [(ParseMessageAtLine.Create (DocumentLocation.Create 0 0) (sprintf "'%s.%s' has more too many attributes of type '%s'" (st.MemberType.ToString()) (st.Name) (typeof<'T>.FullName)))]


type internal MapperReturnType = FallibleOption<YamlToNativeMapping,ParseMessageAtLine list>
and internal NodeKindToIdiomaticMapper = (NodeKindToIdiomaticMappers -> Type -> FallibleOption<YamlToNativeMapping,ParseMessageAtLine list>)
and internal NodeKindToIdiomaticMappers = private {
        PotentialMappers : NodeKindToIdiomaticMapper list
    }
    with
        static member Create ml = {PotentialMappers = ml}
        member this.TryFindMapper (t:Type) : MapperReturnType =
            this.PotentialMappers
            |>  List.map(fun pmf -> pmf this t)
            |>  FallibleOption.errorsOrValues(fun ml ->
                ml
                |>  function
                    |   []  -> ErrorResult [(ParseMessageAtLine.Create (DocumentLocation.Create 0 0) (sprintf "Unsupported: no conversion for: %s.%s" (t.MemberType.GetType().FullName) (t.FullName)))]
                    |   [Value e] -> Value e
                    |   _ -> ErrorResult [(ParseMessageAtLine.Create (DocumentLocation.Create 0 0) (sprintf "Ambigous: too many converters found for: %s.%s" (t.MemberType.GetType().FullName) (t.FullName)))]
                )


and PrimitiveMappingInfo = {
        ScalarMapping   : YamlMapping
    }
    with
        static member TryFindMapper (mappers:NodeKindToIdiomaticMappers) (t:Type) : MapperReturnType =
            YamlTypeMappings
            |> List.tryFind(fun yt -> AreTypesEqual t yt.TargetType) 
            |> Option.map(fun yt -> PrimitiveMapping {ScalarMapping = yt })
            |>  function
                |   Some d  -> Value d
                |   None    -> NoResult

        member this.map (n:Node) =
            if n.NodeTag.Uri = this.ScalarMapping.YamlTag.Uri then
                faillableSequence {
                    let! scalar = getScalarNode n
                    return this.ScalarMapping.ToNative scalar.Data
                }
            else
                ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) (sprintf "Incompatible type '%s' for tag: %s" n.NodeTag.Uri this.ScalarMapping.TargetType.Name))]

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
        static member TryFindMapper (mappers:NodeKindToIdiomaticMappers) (t:Type) : MapperReturnType  =
            if FSharpType.IsRecord t then
                FSharpType.GetRecordFields t
                |>  List.ofArray
                |>  List.map(fun fld ->
                    faillableSequence {
                        let! propMapping = mappers.TryFindMapper fld.PropertyType
                        let! yamlName =
                            GetCustomAttribute<YamlFieldAttribute> fld
                            |>  function
                                |   NoResult -> Value fld.Name
                                |   Value ya -> Value ya.Name'
                                |   ErrorResult e -> ErrorResult e
                        return { YamlName = yamlName; PropertyName = fld.Name; PropertyMapping = propMapping }
                    }
                )
                |>  FallibleOption.errorsOrValues(fun possibleMappings ->
                        let m = possibleMappings |>  List.map(fun pm -> pm.Data)
                        Value (RecordMapping { Target = t; FieldMapping = m })
                    )
            else
                NoResult

        member this.map (n:Node) =
            let mapKeyValue kn v = 
                this.FieldMapping
                |>  List.tryFind(fun fm -> fm.YamlName = kn)
                |>  Option.map(fun fm -> kn, fm.PropertyMapping.map v)
                |>  function
                    |   Some (k,ov) -> ov |> FallibleOption.map(fun v -> k,v)
                    |   None -> NoResult

            let possibleDataToFieldMapping =
                getMapNode n
                |>  FallibleOption.forCollection(fun dt ->
                    dt.Data
                    |>  List.map(fun (k,v) -> 
                        if k.NodeTag.Uri = YamlExtended.StringGlobalTag.Uri then
                            getScalarNode k
                            |>  FallibleOption.bind(fun kf -> mapKeyValue (kf.Data) v)
                        else
                            NoResult
                ))
                |>  List.choosefo(id)
            possibleDataToFieldMapping 
            |>  FallibleOption.errorsOrValues(fun possibleDataToFieldMapping  ->
                let dataToFieldMapping =
                    possibleDataToFieldMapping
                    |>  List.map(fun e -> e.Data)
                    |>  Map.ofList
                let values = 
                    this.FieldMapping
                    |>  List.map(fun fm ->
                        if dataToFieldMapping.ContainsKey fm.YamlName then
                            dataToFieldMapping.[fm.YamlName]
                        else
                            fm.PropertyMapping.Default
                    )
                FSharpValue.MakeRecord(this.Target,values |> List.toArray) |> Value 
            )

        member this.Default with get() = failwith "Unsupported: default record value"


and OptionalMappingInfo = {
        OptionType    : Type
        OptionMapping : YamlToNativeMapping
    }
    with
        static member TryFindMapper (mappers:NodeKindToIdiomaticMappers) (t:Type) =
            let IsOptional (t:Type) = AreTypesEqual typeof<FSharp.Core.Option<obj>> t
            if IsOptional t then
                mappers.TryFindMapper t.GenericTypeArguments.[0]
                |>  FallibleOption.map(fun mapping -> OptionMapping {OptionType=t; OptionMapping = mapping})
            else
                NoResult

        member this.map (n:Node) = 
            let ctor = this.OptionType.GetConstructors() |> Array.head
            faillableSequence{
                let! v = this.OptionMapping.map n
                return ctor.Invoke([| v |]) |> box
            }

        member this.Default with get() = None |> box

and ListMappingInfo = {
        ListType    : Type
        ListMapping : YamlToNativeMapping
    }
    with
        static member TryFindMapper (mappers:NodeKindToIdiomaticMappers) (t:Type) =
            let IsList (t:Type) = AreTypesEqual typeof<FSharp.Collections.List<obj>> t
            if IsList t then
                mappers.TryFindMapper t.GenericTypeArguments.[0]
                |>  FallibleOption.map(fun mapping -> ListMapping {ListType = t; ListMapping = mapping})
            else
                NoResult

        member this.map (n:Node) = 
            getSeqNode n
            |>  FallibleOption.forCollection(fun dt ->
                dt.Data
                |>  List.rev
                |>  List.map(this.ListMapping.map)
            )
            |>  FallibleOption.errorsOrValues(fun possibleData ->
                possibleData
                |>  List.map(fun pd -> pd.Data)
                |>  List.fold(fun (s:obj) e -> s.GetType().GetMethod("Cons").Invoke(null, [|e;s|])) this.Default
                |>  box
                |>  Value
            )

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
        member this.map (n:Node) : FallibleOption<obj, ParseMessageAtLine list> =
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

let private TryFindMappers =
    [
        PrimitiveMappingInfo.TryFindMapper
        RecordMappingInfo.TryFindMapper
        OptionalMappingInfo.TryFindMapper
        ListMappingInfo.TryFindMapper
    ]



let CreateTypeMappings<'tp>() =
    let tryFindMapper = NodeKindToIdiomaticMappers.Create TryFindMappers
    tryFindMapper.TryFindMapper typeof<'tp>


type SuccessInfo<'tp> = {
        Data : 'tp
        Warn : ParseMessageAtLine list
        StopLocation : DocumentLocation
    }
    with
        static member Create d w sl =  {Data = d; Warn = w; StopLocation = sl}


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


let Deserialize<'tp> yml : DeserializeResult<'tp> =
    let NoDocumentLocation = (DocumentLocation.Create 0 0)
    let yamlParser = Yaml12Parser()
    CreateTypeMappings<'tp>()
    |>  function
        |   NoResult -> ErrorInfo.Create ([ParseMessageAtLine.Create (DocumentLocation.Create 0 0) "Cannot find yaml to type mappers"]) [] NoDocumentLocation |> Error
        |   ErrorResult e -> ErrorInfo.Create (e) [] NoDocumentLocation |> Error
        |   Value mappings ->
            let ymlpl = (yamlParser.``l-yaml-stream`` YamlExtended.Schema yml) |> List.head
            match ymlpl with
            |   NoRepresentation err -> ErrorInfo.Create (err.Error) (err.Warn) (err.StopLocation) |> Error
            |   EmptyRepresentation mt -> ErrorInfo.Create ([ParseMessageAtLine.Create NoDocumentLocation  "Document was empty"]) (mt.Warn) (mt.StopLocation) |> Error
            |   PartialRepresentaton pdr
            |   CompleteRepresentaton pdr -> 
                    mappings.map (pdr.Document)
                    |>  function
                        |   NoResult -> ErrorInfo.Create ([ParseMessageAtLine.Create NoDocumentLocation  "Document cannot be mapped"]) (pdr.Warn) (pdr.StopLocation) |> Error
                        |   ErrorResult e -> ErrorInfo.Create (e) (pdr.Warn) (pdr.StopLocation) |> Error
                        |   Value v -> 
                            let d = unbox<'tp> v
                            SuccessInfo<'tp>.Create d (pdr.Warn) (pdr.StopLocation) |> Succes
            

