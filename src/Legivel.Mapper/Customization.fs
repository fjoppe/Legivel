module Legivel.Customization.Mapping

open System
open Legivel.Parser
open Legivel.Common
open Legivel.TagResolution
open Legivel.RepresentationGraph
open Legivel.Customization.Utilities
open Legivel.Attributes
open Microsoft.FSharp.Reflection
open System.Reflection

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


let CreateTypeMappings<'tp> (tryFindMappers:TryFindIdiomaticMapperForType list) =
    let tryFindMapper = AllTryFindIdiomaticMappers.Create tryFindMappers
    tryFindMapper.TryFindMapper typeof<'tp>

