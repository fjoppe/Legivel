module Legivel.Customization.Mapping

open System
open Legivel.Parser
open Legivel.Common
open Legivel.TagResolution
open Legivel.RepresentationGraph
open Legivel.Customization.Utilities
open Legivel.Attributes
open Legivel.Mapper.Model
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Globalization
open System.Security.Cryptography.X509Certificates


/// To register one yaml scalar to native mapping
type ScalarToNativeMapping = {
        YamlTag     : GlobalTag
        TargetType  : Type
        ToNative    : (string -> obj)
    }
    with
        static member Create (yt, tt, tn) = { YamlTag = yt; TargetType = tt; ToNative = tn}

module YamlMapped =
    let providedSeqTags = [YamlExtended.SequenceGlobalTag]
    let providedScalarTags = [YamlExtended.BooleanGlobalTag; YamlCore.IntegerGlobalTag; YamlCore.FloatGlobalTag; YamlExtended.TimestampGlobalTag; YamlExtended.NullGlobalTag; YamlExtended.MergeGlobalTag; YamlExtended.StringGlobalTag]
    let providedMappingTags = [YamlExtended.UnOrderedSetGlobalTag;YamlExtended.MappingGlobalTag]

    let YEFailSafeResolution nst =
        match nst.Content.Kind with
        |   Mapping -> Some YamlExtended.MappingGlobalTag
        |   Sequence-> Some YamlExtended.SequenceGlobalTag
        |   Scalar  -> Some YamlExtended.StringGlobalTag

    let tagResolutionYamlExtended = Legivel.TagResolution.SchemaUtils.tagResolution YEFailSafeResolution (YamlExtended.MappingGlobalTag, YamlExtended.SequenceGlobalTag, YamlExtended.StringGlobalTag)


    let Schema = { YamlExtended.Schema with
        GlobalTags = providedScalarTags @ providedSeqTags @ providedMappingTags
        TagResolution = tagResolutionYamlExtended providedMappingTags providedSeqTags providedScalarTags
    }


/// All yaml-scalar to native mappings
let YamlScalarToNativeMappings = [
    ScalarToNativeMapping.Create (YamlExtended.StringGlobalTag, typeof<string>, fun (s:string) -> box s)
    ScalarToNativeMapping.Create (YamlCore.IntegerGlobalTag, typeof<int>, fun (s:string) -> YamlExtended.IntegerGlobalTag.ToCanonical s |> Option.get |> Int32.Parse |> box)
    ScalarToNativeMapping.Create (YamlCore.FloatGlobalTag, typeof<float>, fun (s:string) -> 
        let value = YamlExtended.FloatGlobalTag.ToCanonical s |> Option.get
        Double.Parse(value, CultureInfo.InvariantCulture) |> box)
    ScalarToNativeMapping.Create (YamlExtended.BooleanGlobalTag, typeof<bool>, fun (s:string) -> YamlExtended.BooleanGlobalTag.ToCanonical s |> Option.get |> Boolean.Parse |> box)
    ScalarToNativeMapping.Create (YamlExtended.TimestampGlobalTag, typeof<DateTime>, fun (s:string) -> 
        let value = YamlExtended.TimestampGlobalTag.ToCanonical s |> Option.get 
        DateTime.Parse(value, CultureInfo.InvariantCulture).ToUniversalTime() |> box)
    ]


/// The return type of a TryFindMapper function
type TryFindMapperReturnType = FallibleOption<IYamlToNativeMapping*AllTryFindIdiomaticMappers,ParseMessageAtLine list>

/// signature of a TryFindMapper function, which may return a mapping construct for the given native type
and TryFindIdiomaticMapperForType = (AllTryFindIdiomaticMappers -> Type -> TryFindMapperReturnType)

/// Structure containing all TryFindMapper functions available
and AllTryFindIdiomaticMappers = private {
        PotentialMappers : TryFindIdiomaticMapperForType list
        NullTagUri : string
        StringTagUri : string
        KnownTypes : MappedTypes
    }
    with
        static member Create ml nt st = {PotentialMappers = ml; NullTagUri = nt; StringTagUri=st; KnownTypes=MappedTypes.Create()}

        /// Try to find a mapper for the given type, look in all potential mappers
        member this.TryFindMapper (t:Type) : TryFindMapperReturnType =
            this.PotentialMappers
            |>  List.tryFindFo(fun pmf -> pmf this t)
            |>  function
                |   NoResult    -> ErrorResult [(ParseMessageAtLine.Create NoDocumentLocation (sprintf "Unsupported: no conversion for: %s.%s" (t.MemberType.GetType().FullName) (t.FullName)))]
                |   Value e     -> Value e
                |   _ -> failwith (sprintf "Ambigous: too many converters found for: %s.%s" (t.MemberType.GetType().FullName) (t.FullName))

        member this.GetMapper t = this.KnownTypes.GetMapper t, this

        member this.RegisterMapping t m =
            { this with KnownTypes = this.KnownTypes.RegisterTypeMapping t m}

/// Mapper structure for all primitive types
type PrimitiveMappingInfo = {
        ScalarMapping   : ScalarToNativeMapping
    }
    with
        /// Return a PrimitiveMappingInfo when the given type maps to one of the supported yaml scalar tags
        static member TryFindMapper (primitiveMappers:ScalarToNativeMapping list) (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            if mappers.KnownTypes.HasMapper t then
                mappers.GetMapper t |> Value
            else
                primitiveMappers
                |> List.tryFind(fun yt -> AreTypesEqual t yt.TargetType) 
                |> Option.map(fun yt -> { ScalarMapping = yt } :> IYamlToNativeMapping)
                |>  function
                    |   Some d  -> Value (d, mappers.RegisterMapping t d)
                    |   None    -> NoResult
        
        interface IYamlToNativeMapping with

            /// Map the given Node to the target primitive type
            member this.map (n:Node) =
                if n.NodeTag.Uri = this.ScalarMapping.YamlTag.Uri then
                    faillableSequence {
                        let! scalar = getScalarNode n
                        return this.ScalarMapping.ToNative scalar.Data
                    }
                else
                    ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) (sprintf "Type mismatch '%s' for tag: %s" n.NodeTag.Uri this.ScalarMapping.TargetType.Name))]

            /// Returns the default value of the target type - which is unavailable for primitive types
            member this.Default
                with get() = NoResult
                    //try
                    //    Activator.CreateInstance(this.ScalarMapping.TargetType) |> box |> Value
                    //with
                    //|   e -> NoResult


/// Contains the mapping of one record field
type RecordFieldMapping = {
        /// The yaml name of the field
        YamlName        : string

        /// The Field name in the record's type definition
        PropertyName    : string

        /// The mapping for this field
        PropertyMapping : IYamlToNativeMapping
    }


/// Mapper structure for a record type
type RecordMappingInfo = {
        Target       : Type
        FieldMapping : RecordFieldMapping list
        StringTagUri : string
    }
    with
        /// Return a RecordMappingInfo when the given type is a record and all record-fields can be mapped
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType  =
            if mappers.KnownTypes.HasMapper t then
                mappers.GetMapper t |> Value
            else
                if FSharpType.IsRecord t then
                    FSharpType.GetRecordFields t
                    |>  List.ofArray
                    |>  List.fold(fun (rfms, nms) fld ->
                        faillableSequence {
                            let! (propMapping,newMappers) = mappers.TryFindMapper fld.PropertyType
                            let! yamlName =
                                GetCustomAttributeMmbr<YamlFieldAttribute> fld
                                |>  function
                                    |   NoResult -> Value fld.Name
                                    |   Value ya -> Value ya.Name'
                                    |   ErrorResult e -> ErrorResult e
                            return { YamlName = yamlName; PropertyName = fld.Name; PropertyMapping = propMapping },newMappers
                        }
                        |>
                        function
                        |   Value (rf, nm) -> ((Value rf) :: rfms, nm)
                        |   NoResult -> (NoResult :: rfms, nms)
                        |   ErrorResult e -> (ErrorResult e :: rfms, nms)
                    ) ([], mappers)
                    |> fun (fl, nmps) ->
                        fl
                        |>  List.rev
                        |>  FallibleOption.errorsOrValues(fun possibleMappings ->
                            let m = possibleMappings |>  List.map(fun pm -> pm.Data)
                            let d = { Target = t; FieldMapping = m; StringTagUri = nmps.StringTagUri } :> IYamlToNativeMapping 
                            Value (d, nmps.RegisterMapping t d)
                        )
                else
                    NoResult


        interface IYamlToNativeMapping with

            /// Map the given Node to the target record type
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
                            //  k = record field name, should always be of type string
                            if k.NodeTag.Uri =  this.StringTagUri then
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

            /// Returns the default value of the target type - which is unavailable for record types
            member this.Default with get() = NoResult


/// Mapper structure for an Option<'a> type
type OptionalMappingInfo = {
        OptionType    : Type
        OptionMapping : IYamlToNativeMapping
        NullTagUri    : string
    }
    with
        /// Return a OptionalMappingInfo when the given type is an option and the option-data type can be mapped
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            if mappers.KnownTypes.HasMapper t then
                mappers.GetMapper t |> Value
            else
                let IsOptional (t:Type) = AreTypesEqual typeof<FSharp.Core.Option<obj>> t
                if IsOptional t then
                    mappers.TryFindMapper t.GenericTypeArguments.[0]
                    |>  FallibleOption.map(fun (mp, nmps) -> 
                            let d = {OptionType=t; OptionMapping = mp;NullTagUri = mappers.NullTagUri} :> IYamlToNativeMapping
                            d, nmps.RegisterMapping t d)
                else
                    NoResult

        interface IYamlToNativeMapping with

            /// Map the given Node to the target option type
            member this.map (n:Node) = 
                if n.NodeTag.Uri = this.NullTagUri then
                    (this:> IYamlToNativeMapping).Default
                else
                    let ctor = this.OptionType.GetConstructors() |> Array.head
                    faillableSequence {
                        let! v = this.OptionMapping.map n
                        return ctor.Invoke([| v |]) |> box
                    }

            /// Returns the default value of the target type - which is None for option
            member this.Default with get() = None |> box |> Value


/// Mapper structure for a List<'a> type
type ListMappingInfo = {
        ListType    : Type
        ListMapping : IYamlToNativeMapping
    }
    with
        /// Return a ListMappingInfo when the given type is a list and the list-data-type can be mapped
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            if mappers.KnownTypes.HasMapper t then
                mappers.GetMapper t |> Value
            else
                let IsList (t:Type) = AreTypesEqual typeof<FSharp.Collections.List<obj>> t
                if IsList t then
                    mappers.TryFindMapper t.GenericTypeArguments.[0]
                    |>  FallibleOption.map(fun (mp, nmps) -> 
                        let d = {ListType = t; ListMapping = mp} :> IYamlToNativeMapping
                        d, nmps.RegisterMapping t d)
                else
                    NoResult


        interface IYamlToNativeMapping with

            /// Map the given Node to the target list type
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

            /// Returns the default value of the target type - which is an empty list (may require revision)
            member this.Default
                with get() = 
                    this.ListType.GetProperty("Empty", BindingFlags.Static ||| BindingFlags.Public).GetGetMethod().Invoke(null, [||]) 
                    |> box |> Value


type MapMappingInfo = {
        MapType     : Type
        KeyType     : IYamlToNativeMapping
        ValueType   : IYamlToNativeMapping
    }
    with
        /// Return a MapMappingInfo when the given type is a Map
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            if mappers.KnownTypes.HasMapper t then
                mappers.GetMapper t |> Value
            else
                let IsMap (t:Type) = AreTypesEqual typeof<FSharp.Collections.Map<int, int>> t
                if IsMap t then
                    faillableSequence {
                        let! (keyType, nm) = mappers.TryFindMapper t.GenericTypeArguments.[0]
                        let! (valType, nm) = nm.TryFindMapper t.GenericTypeArguments.[1]
                        return (keyType,valType), nm
                    }
                    |>  FallibleOption.map(fun ((kt,vt), nmps) -> 
                        let d = {MapType = t; KeyType = kt; ValueType = vt} :> IYamlToNativeMapping
                        d, nmps.RegisterMapping t d)
                else
                    NoResult

        member private this.EmptyMap 
            with get() =
                let mm = [ for i in Assembly.GetAssembly(this.MapType).ExportedTypes do yield i]|> List.find(fun m -> m.Name.Contains("MapModule"))
                let mt = mm.GetMethod("Empty")
                mt.MakeGenericMethod(this.MapType.GetGenericArguments()).Invoke(null, [||])

        interface IYamlToNativeMapping with

            /// Map the given Node to the target map type
            member this.map (n:Node) = 
                getMapNode n
                |>  FallibleOption.forCollection(fun dt ->
                    dt.Data
                    |>  List.rev
                    |>  List.map(fun (k,v) ->
                        faillableSequence {
                            let! km = this.KeyType.map k
                            let! vm = this.ValueType.map v
                            return (km,vm)
                        }
                    )
                )
                |>  FallibleOption.errorsOrValues(fun possibleData ->
                    possibleData
                    |>  List.map(fun pd -> pd.Data)
                    |>  List.fold(fun (s:obj) (k,v) -> 
                            this.MapType.GetMethod("Add").Invoke(s, [|k;v|])
                        ) this.EmptyMap
                    |>  box
                    |>  Value
                )

            /// Returns the default value of the target type
            member this.Default
                with get() = NoResult


type EnumFieldMapping = {
        YamlName   : string
        EnumName   : string
        EnumValue  : obj
    }


/// Mapper structure for a Discriminated Union, which compiles to an enum type
type EnumMappingInfo = {
        EnumType  : Type
        EnumCases : EnumFieldMapping list
    }
    with
        /// Return a EnumMappingInfo when the given type is an enum
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            if mappers.KnownTypes.HasMapper t then
                mappers.GetMapper t |> Value
            else
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
                        let d = { EnumType = t; EnumCases = um} :> IYamlToNativeMapping 
                        (d, mappers.RegisterMapping t d) |> Value
                    )
                else
                    NoResult

        interface IYamlToNativeMapping with

            /// Map the given Node to the target enum value
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

            /// Returns the default value of the target type - which is unavailable for enum
            member this.Default
                with get() = NoResult


type DUFieldMapping = {
        YamlName : string
        DUName   : string
        UCI      : UnionCaseInfo
    }


/// Mapper structure for a Discriminated Union - non-enum
type DiscriminatedUnionMappingInfo = {
        DUType  : Type
        DUField : string option
        UnionCases : DUFieldMapping list
        UnionCaseMapping : (string * IYamlToNativeMapping) list
    }
    with
        /// Return a DiscriminatedUnionMappingInfo when the given type is a Discriminated Union
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            if mappers.KnownTypes.HasMapper t then
                mappers.GetMapper t |> Value
            else
                if FSharpType.IsUnion(t) then
                    let ucl = FSharpType.GetUnionCases(t) |> List.ofArray

                    let duf =
                        GetCustomAttributeTp<YamlFieldAttribute> t
                        |>  function
                            |   NoResult -> None
                            |   Value ya -> Some ya.Name'
                            |   ErrorResult _ -> None

                    let (mappedFields,nmprs) =
                        ucl
                        |>  List.fold(fun (mfl, (nmpl:AllTryFindIdiomaticMappers)) uc ->
                                let fld = uc.GetFields() |> List.ofArray
                                match fld with
                                |   []      ->  NoResult :: mfl, nmpl
                                |   [fd]    ->  
                                    nmpl.TryFindMapper(fd.PropertyType)
                                    |>  function
                                        |   Value (vmp, vmptl) -> Value(uc.Name, vmp) :: mfl , vmptl
                                        |   NoResult -> NoResult :: mfl , nmpl
                                        |   ErrorResult e  -> ErrorResult e :: mfl , nmpl
                                |   _       ->  ErrorResult [(ParseMessageAtLine.Create NoDocumentLocation (sprintf "Union case contains more than one data-type '%s' in type: '%s'" uc.Name t.FullName))] :: mfl, nmpl
                        ) ([], mappers)

                    let ucmp =
                        ucl
                        |>  List.map(fun uc ->
                            GetCustomAttributeDU<YamlValueAttribute> uc
                            |>  function
                                |   NoResult -> { YamlName = uc.Name; DUName = uc.Name; UCI = uc} |> Value
                                |   Value ya -> { YamlName = ya.Id'; DUName = uc.Name; UCI = uc} |> Value
                                |   ErrorResult e -> ErrorResult e
                        )

                    ucmp
                    |>  FallibleOption.errorsOrValues(fun ucm -> 
                        let um = ucm |> List.map(fun d -> d.Data)
                        mappedFields
                        |>  FallibleOption.errorsOrValues(fun cml -> 
                            let d = { DUType = t; DUField = duf; UnionCases = um; UnionCaseMapping = cml |> List.map(fun d -> d.Data)} :> IYamlToNativeMapping 
                            (d, nmprs.RegisterMapping t d) |> Value)
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
        //  Embedded style:
        //      field1 : value1
        //      typeid : unioncase (and other pairs in the mapping form the data - excluding field "typeid")
        //      field2 : value2
        //
        //      NodeKind = Mapping, where one k/v pair identifies the DU case, 
        //                          other pairs form the record data contained in the union-case
        //      Remarks: requires [<YamlField("fieldid")>] for DU type, and preferably [<YamlValue("unioncase")>] per union-case

        interface IYamlToNativeMapping with

            /// Map the given Node to the target Union Case
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

                let EmbeddedStyle() =
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
                let r = [PlainStyle; EmbeddedStyle] |>  List.tryFindFo(fun f -> f())
                r

            /// Returns the default value of the target type - which is unavailable for DU's
            member this.Default
                with get() = NoResult


/// All build-in TryFindMapper functions
let BuildInTryFindMappers : TryFindIdiomaticMapperForType list = [
        PrimitiveMappingInfo.TryFindMapper YamlScalarToNativeMappings
        RecordMappingInfo.TryFindMapper
        OptionalMappingInfo.TryFindMapper
        ListMappingInfo.TryFindMapper
        EnumMappingInfo.TryFindMapper
        MapMappingInfo.TryFindMapper
        //  Do discriminated union last:
        //  FSharpType.IsUnion(typeof<obj list>) = true, there could be other cases?
        DiscriminatedUnionMappingInfo.TryFindMapper 
    ]


/// Creates a yaml-to-native-mapper for the given type 'tp
let CreateTypeMappings<'tp> (tryFindMappers:TryFindIdiomaticMapperForType list) nullTagUri stringTagUri =
    let tryFindMapper = AllTryFindIdiomaticMappers.Create tryFindMappers nullTagUri stringTagUri
    tryFindMapper.TryFindMapper typeof<'tp>


/// Returned when deserialization in succesful
type Success<'tp> = {
        Data : 'tp
        Warn : ParseMessageAtLine list
    }
    with
        static member Create d w =  {Data = d; Warn = w}

/// Returned when deserialization contained errors
type Error = {
        Warn  : ParseMessageAtLine list 
        Error : ParseMessageAtLine list
        StopLocation : DocumentLocation
    }
    with
        static member Create e w sl = { Warn = w; Error = e ; StopLocation = sl}

/// Result of customized yaml to native mapping
type Result<'tp> =
    |   Processed of Success<'tp>
    |   WithErrors of Error


/// Maps a (parsed) yaml document to a native type-instance, using the given mapper
let MapYamlDocumentToNative (mapper:IYamlToNativeMapping) (pdr:ParsedDocumentResult) =
    mapper.map (pdr.Document)
    |>  function
        |   NoResult -> Error.Create ([ParseMessageAtLine.Create NoDocumentLocation  "Document cannot be mapped"]) (pdr.Warn) (pdr.StopLocation) |> WithErrors
        |   ErrorResult e -> Error.Create (e) (pdr.Warn) (pdr.StopLocation) |> WithErrors
        |   Value v -> 
            let d = unbox<'tp> v
            Success<'tp>.Create d (pdr.Warn) |> Processed


/// Parses a yaml string, for the given yaml-schema and maps it to a native type instance
let ParseYamlToNative (mapToNative:ParsedDocumentResult -> Result<'tp>) schema yml =
    let yamlParser = Yaml12Parser()
    (yamlParser.``l-yaml-stream`` schema yml) 
    |> List.map(fun ymlpl ->
        match ymlpl with
        |   NoRepresentation err -> Error.Create (err.Error) (err.Warn) (err.StopLocation) |> WithErrors
        |   EmptyRepresentation mt -> Error.Create ([ParseMessageAtLine.Create NoDocumentLocation  "Document was empty"]) (mt.Warn) (mt.StopLocation) |> WithErrors
        |   PartialRepresentaton pdr
        |   CompleteRepresentaton pdr -> mapToNative pdr
    )


/// Customized yaml deserialization, where one can inject everything required
let CustomDeserializeYaml<'tp> (tryFindMappers:TryFindIdiomaticMapperForType list) (mapYmlDocToNative:IYamlToNativeMapping->ParsedDocumentResult->Result<'tp>) (parseYmlToNative:(ParsedDocumentResult -> Result<'tp>) -> GlobalTagSchema -> string -> Result<'tp> list) schema nullTagUri stringTagUri yml : Result<'tp> list =
    CreateTypeMappings<'tp> tryFindMappers nullTagUri stringTagUri
    |>  function
        |   NoResult -> [Error.Create ([ParseMessageAtLine.Create (DocumentLocation.Create 0 0) "Cannot find yaml to type mappers"]) [] NoDocumentLocation |> WithErrors]
        |   ErrorResult e -> [Error.Create (e) [] NoDocumentLocation |> WithErrors]
        |   Value (mappings,_) -> parseYmlToNative (mapYmlDocToNative mappings) schema yml


