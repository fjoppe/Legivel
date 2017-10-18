module Legivel.Mapper

open System
open Legivel.Parser
open Legivel.Common
open Legivel.TagResolution
open Legivel.RepresentationGraph
open Microsoft.FSharp.Reflection
open System.Reflection


let NoDocumentLocation = (DocumentLocation.Create 0 0)

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
    let tryFindFo<'a,'b, 'c> (f:'c->FallibleOption<'a,'b>) l = 
        l 
        |>  List.skipWhile(fun e -> 
                let i = f e
                i.IsNoResult) 
        |>  function 
            | []    ->  NoResult
            | h::_  ->  f h


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
type YamlFieldAttribute(Name : string) = 
    inherit System.Attribute()
    member this.Name' = Name


type YamlValueAttribute(Id : string) = 
    inherit System.Attribute()
    member this.Id' = Id


let AreTypesEqual (t1:Type) (t2:Type) =
    let c1 = sprintf "%s%s" t1.Namespace t1.Name
    let c2 = sprintf "%s%s" t2.Namespace t2.Name
    c1 = c2


let getMapNode (n:Node) =
    match n with
    |   MapNode n ->  Value n
    |   _    -> ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a mapping node")]


let getMapNodeQuiet (n:Node) =
    match n with
    |   MapNode n ->  Value n
    |   _    -> NoResult


let getSeqNode (n:Node) =
    match n with
    |   SeqNode n ->  Value n 
    |   _    -> ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a Sequence Node")]


let getScalarNode (n:Node) =
    match n with
    |   ScalarNode n ->  Value n
    |   _    -> ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a Scalar Node")]


let getScalarNodeQuiet (n:Node) =
    match n with
    |   ScalarNode n ->  Value n
    |   _    -> NoResult


let GetCustomAttributeTp<'T when 'T :> Attribute> (st:Type) =
    let at = Attribute.GetCustomAttributes(st, typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> NoResult
    |   1   -> Value (at.Head :?> 'T)
    |   _   -> ErrorResult [(ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.ToString()) (st.Name) (typeof<'T>.FullName)))]


let GetCustomAttributeMmbr<'T when 'T :> Attribute> (st:MemberInfo) =
    let at = Attribute.GetCustomAttributes(st, typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> NoResult
    |   1   -> Value (at.Head :?> 'T)
    |   _   -> ErrorResult [(ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.MemberType.ToString()) (st.Name) (typeof<'T>.FullName)))]

let GetCustomAttributeFld<'T when 'T :> Attribute> (st:FieldInfo) =
    let at = [for i in st.GetCustomAttributes(typeof<'T>) do yield i]
    match at.Length with
    |   0   -> NoResult
    |   1   -> Value (at.Head :?> 'T)
    |   _   -> ErrorResult [(ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.MemberType.ToString()) (st.Name) (typeof<'T>.FullName)))]

let GetCustomAttributeDU<'T when 'T :> Attribute> (st:UnionCaseInfo) =
    let at = st.GetCustomAttributes(typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> NoResult
    |   1   -> Value (at.Head :?> 'T)
    |   _   -> ErrorResult [(ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.DeclaringType.ToString()) (st.Name) (typeof<'T>.FullName)))]


type IYamlToNativeMapping =
    abstract member map : n:Node -> FallibleOption<obj, ParseMessageAtLine list>
    abstract member Default : FallibleOption<obj, ParseMessageAtLine list> with get


type TryFindMapperReturnType = FallibleOption<IYamlToNativeMapping,ParseMessageAtLine list>
type TryFindIdiomaticMapperForType = (AllTryFindIdiomaticMappers -> Type -> FallibleOption<IYamlToNativeMapping,ParseMessageAtLine list>)
and AllTryFindIdiomaticMappers = private {
        PotentialMappers : TryFindIdiomaticMapperForType list
    }
    with
        static member Create ml = {PotentialMappers = ml}
        member this.TryFindMapper (t:Type) : TryFindMapperReturnType =
            this.PotentialMappers
            |>  List.tryFindFo(fun pmf -> pmf this t)
            |>  function
                |   NoResult    -> ErrorResult [(ParseMessageAtLine.Create NoDocumentLocation (sprintf "Unsupported: no conversion for: %s.%s" (t.MemberType.GetType().FullName) (t.FullName)))]
                |   Value e     -> Value e
                |   _ -> failwith (sprintf "Ambigous: too many converters found for: %s.%s" (t.MemberType.GetType().FullName) (t.FullName))


type PrimitiveMappingInfo = {
        ScalarMapping   : YamlMapping
    }
    with
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            YamlTypeMappings
            |> List.tryFind(fun yt -> AreTypesEqual t yt.TargetType) 
            |> Option.map(fun yt -> { ScalarMapping = yt } :> IYamlToNativeMapping)
            |>  function
                |   Some d  -> Value d
                |   None    -> NoResult
        
        interface IYamlToNativeMapping with
            member this.map (n:Node) =
                if n.NodeTag.Uri = this.ScalarMapping.YamlTag.Uri then
                    faillableSequence {
                        let! scalar = getScalarNode n
                        return this.ScalarMapping.ToNative scalar.Data
                    }
                else
                    ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) (sprintf "Type mismatch '%s' for tag: %s" n.NodeTag.Uri this.ScalarMapping.TargetType.Name))]

            member this.Default
                with get() = NoResult
                    //try
                    //    Activator.CreateInstance(this.ScalarMapping.TargetType) |> box |> Value
                    //with
                    //|   e -> NoResult


type RecordFieldMapping = {
        YamlName        : string
        PropertyName    : string
        PropertyMapping : IYamlToNativeMapping
    }


type RecordMappingInfo = {
        Target       : Type
        FieldMapping : RecordFieldMapping list
    }
    with
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType  =
            if FSharpType.IsRecord t then
                FSharpType.GetRecordFields t
                |>  List.ofArray
                |>  List.map(fun fld ->
                    faillableSequence {
                        let! propMapping = mappers.TryFindMapper fld.PropertyType
                        let! yamlName =
                            GetCustomAttributeMmbr<YamlFieldAttribute> fld
                            |>  function
                                |   NoResult -> Value fld.Name
                                |   Value ya -> Value ya.Name'
                                |   ErrorResult e -> ErrorResult e
                        return { YamlName = yamlName; PropertyName = fld.Name; PropertyMapping = propMapping }
                    }
                )
                |>  FallibleOption.errorsOrValues(fun possibleMappings ->
                        let m = possibleMappings |>  List.map(fun pm -> pm.Data)
                        Value ( { Target = t; FieldMapping = m } :> IYamlToNativeMapping)
                    )
            else
                NoResult


        interface IYamlToNativeMapping with
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
                    let possibleValues = 
                        this.FieldMapping
                        |>  List.map(fun fm ->
                            if dataToFieldMapping.ContainsKey fm.YamlName then
                                dataToFieldMapping.[fm.YamlName] |> Value
                            else
                                let err = [(ParseMessageAtLine.Create (n.ParseInfo.Start) (sprintf "Missing value for field: '%s'" fm.YamlName))]
                                match fm.PropertyMapping.Default with
                                |   Value v -> Value v
                                |   NoResult -> ErrorResult err
                                |   ErrorResult e -> ErrorResult (err @ e)
                        )
                    possibleValues
                    |>  FallibleOption.errorsOrValues(fun values ->
                        let vs = values |> List.map(fun e -> e.Data) 
                        FSharpValue.MakeRecord(this.Target, vs |> List.toArray) |> Value
                    )
                )

            member this.Default with get() = NoResult


type OptionalMappingInfo = {
        OptionType    : Type
        OptionMapping : IYamlToNativeMapping
    }
    with
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            let IsOptional (t:Type) = AreTypesEqual typeof<FSharp.Core.Option<obj>> t
            if IsOptional t then
                mappers.TryFindMapper t.GenericTypeArguments.[0]
                |>  FallibleOption.map(fun mapping -> {OptionType=t; OptionMapping = mapping} :> IYamlToNativeMapping)
            else
                NoResult

        interface IYamlToNativeMapping with
            member this.map (n:Node) = 
                let ctor = this.OptionType.GetConstructors() |> Array.head
                faillableSequence {
                    let! v = this.OptionMapping.map n
                    return ctor.Invoke([| v |]) |> box
                }

            member this.Default with get() = None |> box |> Value


type ListMappingInfo = {
        ListType    : Type
        ListMapping : IYamlToNativeMapping
    }
    with
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            let IsList (t:Type) = AreTypesEqual typeof<FSharp.Collections.List<obj>> t
            if IsList t then
                mappers.TryFindMapper t.GenericTypeArguments.[0]
                |>  FallibleOption.map(fun mapping -> {ListType = t; ListMapping = mapping} :> IYamlToNativeMapping)
            else
                NoResult


        interface IYamlToNativeMapping with
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
                    |>  List.fold(fun (s:obj) e -> s.GetType().GetMethod("Cons").Invoke(null, [|e;s|])) (this:> IYamlToNativeMapping).Default.Data
                    |>  box
                    |>  Value
                )


            member this.Default
                with get() = 
                    this.ListType.GetProperty("Empty", BindingFlags.Static ||| BindingFlags.Public).GetGetMethod().Invoke(null, [||]) 
                    |> box |> Value


type EnumFieldMapping = {
        YamlName   : string
        EnumName   : string
        EnumValue  : obj
    }

type EnumMappingInfo = {
        EnumType  : Type
        EnumCases : EnumFieldMapping list
    }
    with
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            if t.IsEnum then
                let enml =  [for i in t.GetEnumValues() do yield i]

                let mappedValues =
                    enml 
                    |>  List.map(fun e -> 
                        let es = e.ToString()
                        let fi = t.GetField(es)
                        GetCustomAttributeFld<YamlValueAttribute> fi
                        |>  function
                            |   NoResult -> Value { YamlName = es; EnumName = es; EnumValue = e}
                            |   Value ya -> Value { YamlName = ya.Id'; EnumName = es; EnumValue = e}
                            |   ErrorResult e -> ErrorResult e 
                        )

                mappedValues
                |>  FallibleOption.errorsOrValues(fun ecm -> 
                    let um = ecm |> List.map(fun d -> d.Data)
                    { EnumType = t; EnumCases = um} :> IYamlToNativeMapping |> Value
                )
            else
                NoResult

        interface IYamlToNativeMapping with
            member this.map (n:Node) = 
                let FindUnionCase s =
                    this.EnumCases 
                    |>  List.tryFind(fun ucm -> s = ucm.YamlName)
                    |>  function
                        |   Some v  ->  Value v
                        |   None    ->  ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) (sprintf "Union case '%s' not availble in type: '%s'" s this.EnumType.FullName))]

                let WrappedStyle() =
                    faillableSequence {
                        let! d = getScalarNodeQuiet n
                        let! uc = FindUnionCase d.Data
                        return uc.EnumValue |> box
                    }

                WrappedStyle()

            member this.Default
                with get() = NoResult


type DUFieldMapping = {
        YamlName : string
        DUName   : string
        UCI      : UnionCaseInfo
    }


type DiscriminatedUnionMappingInfo = {
        DUType  : Type
        DUField : string option
        UnionCases : DUFieldMapping list
        UnionCaseMapping : (string * IYamlToNativeMapping) list
    }
    with
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            if FSharpType.IsUnion(t) then
                let ucl = FSharpType.GetUnionCases(t) |> List.ofArray

                let duf =
                    GetCustomAttributeTp<YamlFieldAttribute> t
                    |>  function
                        |   NoResult -> None
                        |   Value ya -> Some ya.Name'
                        |   ErrorResult _ -> None

                let mappedFields =
                    ucl
                    |>  List.map(fun uc ->
                            let fld = uc.GetFields() |> List.ofArray
                            match fld with
                            |   []      ->  NoResult
                            |   [fd]    ->  mappers.TryFindMapper(fd.PropertyType) |>  FallibleOption.map(fun mp -> uc.Name, mp)
                            |   _       ->  ErrorResult [(ParseMessageAtLine.Create NoDocumentLocation (sprintf "Union case contains more than one data-type '%s' in type: '%s'" uc.Name t.FullName))] 
                    )

                let ucmp =
                    ucl
                    |>  List.map(fun uc ->
                        GetCustomAttributeDU<YamlValueAttribute> uc
                        |>  function
                            |   NoResult -> Value { YamlName = uc.Name; DUName = uc.Name; UCI = uc}
                            |   Value ya -> Value { YamlName = ya.Id'; DUName = uc.Name; UCI = uc}
                            |   ErrorResult e -> ErrorResult e
                    )

                ucmp
                |>  FallibleOption.errorsOrValues(fun ucm -> 
                    let um = ucm |> List.map(fun d -> d.Data)
                    mappedFields
                    |>  FallibleOption.errorsOrValues(fun cml -> 
                        { DUType = t; DUField = duf; UnionCases = um; UnionCaseMapping = cml |> List.map(fun d -> d.Data)} :> IYamlToNativeMapping |> Value)
                )
            else
                NoResult

        //
        //  Plain Style
        //      field: UCValue  (UCValue is union-case without data, not enum)
        //
        //      NodeKind = Scalar
        //
        //  Wrapped style
        //      post: data1  ('post' is DU-case, 'data1' is contained data)
        //      get:  data2  ('post' and 'get' are in a sequence in this example)
        //
        //      NodeKind = Mapping, where each key is a DUCase
        //
        //  Contained style:
        //      field1 : value1
        //      typeid : unioncase (and other pairs in the mapping form the data - excluding field "typeid")
        //      field2 : value2
        //
        //      NodeKind = Mapping, where one k/v pair identifies the DU case, 
        //                          other pairs form the record data contained inthe union-case
        //      Remarks: requires [<YamlField("fieldid")>] for DU type, and preferably [<YamlValue("unioncase")>] per union-case

        interface IYamlToNativeMapping with
            member this.map (n:Node) = 
                let FindUnionCase s =
                    this.UnionCases 
                    |>  List.tryFind(fun ucm -> s = ucm.YamlName)
                    |>  function
                        |   Some v  ->  Value v
                        |   None    ->  ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) (sprintf "Union case '%s' not availble in type: '%s'" s this.DUType.FullName))]

                let PlainStyle() =
                    faillableSequence {
                        let! d = getScalarNodeQuiet n
                        let! uc = FindUnionCase d.Data
                        return FSharpValue.MakeUnion (uc.UCI, [||]) |> box
                    }

                //let WrappedStyle() =
                //    faillableSequence {
                //        let! d = getMapNodeQuiet n
                //        let! k = getScalarNode (fst d.Data)

                //        return d
                //    }

                let ContainedStyle() =
                    match this.DUField with
                    |   None -> NoResult
                    |   Some duf ->
                        faillableSequence {
                            let! d = getMapNodeQuiet n
                            let! duid =
                                d.Data
                                |>  List.map(fun (k,v) -> 
                                    faillableSequence {
                                        let! kv = getScalarNode k
                                        let! vv = getScalarNode v
                                        let! r = 
                                            if kv.Data = duf then
                                                FindUnionCase vv.Data
                                            else
                                                NoResult
                                        return r
                                    }
                                    )
                                |>  List.tryFindFo(id)
                            let (m,y) = this.UnionCaseMapping |> List.find(fun (m,_) -> m = duid.DUName)
                            let! data = y.map n
                            return FSharpValue.MakeUnion (duid.UCI, [|data|]) |> box
                        }
                let r = [PlainStyle; ContainedStyle] |>  List.tryFindFo(fun f -> f())
                r


            member this.Default
                with get() = NoResult


let BuildInTryFindMappers : TryFindIdiomaticMapperForType list = [
        PrimitiveMappingInfo.TryFindMapper
        RecordMappingInfo.TryFindMapper
        OptionalMappingInfo.TryFindMapper
        ListMappingInfo.TryFindMapper
        EnumMappingInfo.TryFindMapper

        //  Do discriminated union last:
        //  FSharpType.IsUnion(typeof<obj list>) = true, there could be other cases?
        DiscriminatedUnionMappingInfo.TryFindMapper 
    ]


let CreateTypeMappings<'tp>() (tryFindMappers:TryFindIdiomaticMapperForType list) =
    let tryFindMapper = AllTryFindIdiomaticMappers.Create tryFindMappers
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


let DeserializeWithMappers<'tp> (tryFindMappers:TryFindIdiomaticMapperForType list) yml : DeserializeResult<'tp> =
    let yamlParser = Yaml12Parser()
    CreateTypeMappings<'tp>() tryFindMappers
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
            

let Deserialize<'tp> yml : DeserializeResult<'tp> =
    DeserializeWithMappers<'tp> BuildInTryFindMappers yml
