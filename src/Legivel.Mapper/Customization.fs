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




/// Mapper structure for all primitive types
type PrimitiveMappingInfo = {
        ScalarMapping   : ScalarToNativeMapping
    }
    with
        /// Return a PrimitiveMappingInfo when the given type maps to one of the supported yaml scalar tags
        static member TryFindMapper (primitiveMappers:ScalarToNativeMapping list) (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            if mappers.HasType t then
                FallibleOption<_,_>.Value (mappers.GetRef t, mappers)
            else
                let (r,nm) = mappers.RegisterType t
                primitiveMappers
                |> List.tryFind(fun yt -> AreTypesEqual t yt.TargetType) 
                |> Option.map(fun yt -> { ScalarMapping = yt } :> IYamlToNativeMapping)
                |>  function
                    |   Some d  -> FallibleOption<_,_>.Value (r, nm.RegisterMapper r d)
                    |   None    -> FallibleOption<_,_>.NoResult()
        
        interface IYamlToNativeMapping with

            /// Map the given Node to the target primitive type
            member this.map (mappers:AllTryFindIdiomaticMappers) (n:Node) =
                if n.NodeTag.Uri = this.ScalarMapping.YamlTag.Uri then
                    faillableSequence {
                        let! scalar = getScalarNode n
                        return this.ScalarMapping.ToNative (scalar.Data)
                    }
                else
                    FallibleOption<_,_>.ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) (sprintf "Type mismatch '%s' for tag: %s" n.NodeTag.Uri this.ScalarMapping.TargetType.Name))]

            /// Returns the default value of the target type - which is unavailable for primitive types
            member this.Default
                with get() = FallibleOption<_,_>.NoResult()
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
        PropertyMapping : YTMRef
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
            if mappers.HasType t then
                FallibleOption<_,_>.Value (mappers.GetRef t, mappers)
            else
                if FSharpType.IsRecord t then
                    let (r,nm) = mappers.RegisterType t
                    FSharpType.GetRecordFields t
                    |>  List.ofArray
                    |>  List.fold(fun (rfms, (nms:AllTryFindIdiomaticMappers)) (fld:PropertyInfo) ->
                        faillableSequence {
                            let! (propMapping,newMappers) = nms.TryFindMapper fld.PropertyType
                            let! yamlName =
                                GetCustomAttributeMmbr<YamlFieldAttribute> fld
                                |>  fun custAttrib ->
                                    match custAttrib.Result with
                                    |   FallibleOption.NoResult -> FallibleOption<string,_>.Value (fld.Name)
                                    |   FallibleOption.Value  -> FallibleOption<string,_>.Value (custAttrib.Data.Name')
                                    |   FallibleOption.ErrorResult -> FallibleOption<string,_>.ErrorResult (custAttrib.Error)
                                    |   _   -> failwith "Illegal value for custAttrib"
                            return { YamlName = yamlName; PropertyName = fld.Name; PropertyMapping = propMapping },newMappers
                        }
                        |>  fun isRecord ->
                            match isRecord.Result with
                            |   FallibleOption.Value -> 
                                let  (rf, nm) = isRecord.Data
                                ((FallibleOption<_,_>.Value rf) :: rfms, nm)
                            |   FallibleOption.NoResult -> (FallibleOption<_,_>.NoResult() :: rfms, nms)
                            |   FallibleOption.ErrorResult -> (FallibleOption<_,_>.ErrorResult isRecord.Error :: rfms, nms)
                            |   _   -> failwith "Illegal value for isRecord"
                    ) ([], nm)
                    |> fun (fl, nmps) ->
                        fl
                        |>  List.rev
                        |>  FallibleOption.errorsOrValues(fun possibleMappings ->
                            let m = possibleMappings |>  List.map(fun pm -> pm.Data)
                            let d = { Target = t; FieldMapping = m; StringTagUri = nmps.StringTagUri } :> IYamlToNativeMapping 
                            FallibleOption<_,_>.Value (r, nmps.RegisterMapper r d)
                        )
                else
                    FallibleOption<_,_>.NoResult()


        interface IYamlToNativeMapping with

            /// Map the given Node to the target record type
            member this.map (mappers:AllTryFindIdiomaticMappers) (n:Node) =
                let mapKeyValue kn v = 
                    this.FieldMapping
                    |>  List.tryFind(fun fm -> fm.YamlName = kn)
                    |>  Option.map(fun fm -> kn, mappers.map (fm.PropertyMapping) v)
                    |>  function
                        |   Some (k,ov) -> ov |> FallibleOption.map(fun v -> k,v)
                        |   None -> FallibleOption<_,_>.NoResult()

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
                                FallibleOption<_,_>.NoResult()
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
                                dataToFieldMapping.[fm.YamlName] |> FallibleOption<_,_>.Value
                            else
                                let err = [(ParseMessageAtLine.Create (n.ParseInfo.Start) (sprintf "Missing value for field: '%s'" fm.YamlName))]
                                let getMapper = (mappers.GetMapper (fm.PropertyMapping)).Default 
                                match getMapper.Result with
                                |   FallibleOption.Value -> FallibleOption<_,_>.Value getMapper.Data
                                |   FallibleOption.NoResult -> FallibleOption<_,_>.ErrorResult err
                                |   FallibleOption.ErrorResult -> FallibleOption<_,_>.ErrorResult (err @ getMapper.Error)
                                |   _ -> failwith "Illegal value for getMapper"
                        )
                    possibleValues
                    |>  FallibleOption.errorsOrValues(fun values ->
                        let vs = values |> List.map(fun e -> e.Data) 
                        FSharpValue.MakeRecord(this.Target, vs |> List.toArray) |> FallibleOption<_,_>.Value
                    )
                )

            /// Returns the default value of the target type - which is unavailable for record types
            member this.Default with get() = FallibleOption<_,_>.NoResult()


/// Mapper structure for an Option<'a> type
type OptionalMappingInfo = {
        OptionType    : Type
        OptionMapping : YTMRef
        NullTagUri    : string
    }
    with
        /// Return a OptionalMappingInfo when the given type is an option and the option-data type can be mapped
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            if mappers.HasType t then
                FallibleOption<_,_>.Value (mappers.GetRef t, mappers)
            else
                let IsOptional (t:Type) = AreTypesEqual typeof<FSharp.Core.Option<obj>> t
                if IsOptional t then
                    let (r,nm) = mappers.RegisterType t
                    nm.TryFindMapper t.GenericTypeArguments.[0]
                    |>  FallibleOption.map(fun (mr, nmps) -> 
                            let d = {OptionType=t; OptionMapping = mr;NullTagUri = mappers.NullTagUri} :> IYamlToNativeMapping
                            r, nmps.RegisterMapper r d)
                else
                    FallibleOption<_,_>.NoResult()

        interface IYamlToNativeMapping with

            /// Map the given Node to the target option type
            member this.map (mappers:AllTryFindIdiomaticMappers) (n:Node) = 
                if n.NodeTag.Uri = this.NullTagUri then
                    (this:> IYamlToNativeMapping).Default
                else
                    let ctor = this.OptionType.GetConstructors() |> Array.head
                    faillableSequence {
                        let! v =  mappers.map (this.OptionMapping) n
                        return ctor.Invoke([| v |]) |> box
                    }

            /// Returns the default value of the target type - which is None for option
            member this.Default with get() = None |> box |> FallibleOption<_,_>.Value


/// Mapper structure for a List<'a> type
type ListMappingInfo = {
        ListType    : Type
        ListMapping : YTMRef
    }
    with
        /// Return a ListMappingInfo when the given type is a list and the list-data-type can be mapped
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            if mappers.HasType t then
                FallibleOption<_,_>.Value (mappers.GetRef t, mappers)
            else
                let IsList (t:Type) = AreTypesEqual typeof<FSharp.Collections.List<obj>> t
                if IsList t then
                    let (r,nm) = mappers.RegisterType t
                    nm.TryFindMapper t.GenericTypeArguments.[0]
                    |>  FallibleOption.map(fun (mp, nmps) -> 
                        let d = {ListType = t; ListMapping = mp} :> IYamlToNativeMapping
                        r, nmps.RegisterMapper r d)
                else
                    FallibleOption<_,_>.NoResult()


        interface IYamlToNativeMapping with

            /// Map the given Node to the target list type
            member this.map (mappers:AllTryFindIdiomaticMappers) (n:Node) = 
                getSeqNode n
                |>  FallibleOption.forCollection(fun dt ->
                    dt.Data
                    |>  List.rev
                    |>  List.map(mappers.map this.ListMapping)
                )
                |>  FallibleOption.errorsOrValues(fun possibleData ->
                    possibleData
                    |>  List.map(fun pd -> pd.Data)
                    |>  List.fold(fun (s:obj) e -> s.GetType().GetMethod("Cons").Invoke(null, [|e;s|])) (this:> IYamlToNativeMapping).Default.Data
                    |>  box
                    |>  FallibleOption<_,_>.Value
                )

            /// Returns the default value of the target type - which is an empty list (may require revision)
            member this.Default
                with get() = 
                    this.ListType.GetProperty("Empty", BindingFlags.Static ||| BindingFlags.Public).GetGetMethod().Invoke(null, [||]) 
                    |> box |> FallibleOption<_,_>.Value


type MapMappingInfo = {
        MapType     : Type
        KeyType     : YTMRef
        ValueType   : YTMRef
    }
    with
        /// Return a MapMappingInfo when the given type is a Map
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            if mappers.HasType t then
                FallibleOption<_,_>.Value (mappers.GetRef t, mappers)
            else
                let IsMap (t:Type) = AreTypesEqual typeof<FSharp.Collections.Map<int, int>> t
                if IsMap t then
                    let (r,nm) = mappers.RegisterType t
                    faillableSequence {
                        let! (keyType, nm) = nm.TryFindMapper t.GenericTypeArguments.[0]
                        let! (valType, nm) = nm.TryFindMapper t.GenericTypeArguments.[1]
                        return (keyType,valType), nm
                    }
                    |>  FallibleOption.map(fun ((kt,vt), nmps) -> 
                        let d = {MapType = t; KeyType = kt; ValueType = vt} :> IYamlToNativeMapping
                        r, nmps.RegisterMapper r d)
                else
                    FallibleOption<_,_>.NoResult()

        member private this.EmptyMap 
            with get() =
                let mm = [ for i in Assembly.GetAssembly(this.MapType).ExportedTypes do yield i]|> List.find(fun m -> m.Name.Contains("MapModule"))
                let mt = mm.GetMethod("Empty")
                mt.MakeGenericMethod(this.MapType.GetGenericArguments()).Invoke(null, [||])

        interface IYamlToNativeMapping with

            /// Map the given Node to the target map type
            member this.map (mappers:AllTryFindIdiomaticMappers) (n:Node) = 
                getMapNode n
                |>  FallibleOption.forCollection(fun dt ->
                    dt.Data
                    |>  List.rev
                    |>  List.map(fun (k,v) ->
                        faillableSequence {
                            let! km = mappers.map (this.KeyType) k
                            let! vm = mappers.map (this.ValueType) v
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
                    |>  FallibleOption<_,_>.Value
                )

            /// Returns the default value of the target type
            member this.Default
                with get() = FallibleOption<_,_>.NoResult()


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
            if mappers.HasType t then
                FallibleOption<_,_>.Value (mappers.GetRef t, mappers)
            else
                if t.IsEnum then
                    let (r,nm) = mappers.RegisterType t
                    let enml =  [for i in t.GetEnumValues() do yield i]

                    let mappedValues =
                        enml 
                        |>  List.map(fun e -> 
                            let es = e.ToString()
                            let fi = t.GetField(es)
                            GetCustomAttributeFld<YamlValueAttribute> fi
                            |>  fun customAttrib -> 
                                match customAttrib.Result with
                                |   FallibleOption.NoResult -> FallibleOption<_,_>.Value { YamlName = es; EnumName = es; EnumValue = e}
                                |   FallibleOption.Value -> FallibleOption<_,_>.Value { YamlName = customAttrib.Data.Id'; EnumName = es; EnumValue = e}
                                |   FallibleOption.ErrorResult -> FallibleOption<_,_>.ErrorResult customAttrib.Error 
                                |   _ -> failwith "Illegal value for customAttrib"
                            )

                    mappedValues
                    |>  FallibleOption.errorsOrValues(fun ecm -> 
                        let um = ecm |> List.map(fun d -> d.Data)
                        let d = { EnumType = t; EnumCases = um} :> IYamlToNativeMapping 
                        (r, nm.RegisterMapper r d) |> FallibleOption<_,_>.Value
                    )
                else
                    FallibleOption<_,_>.NoResult()

        interface IYamlToNativeMapping with

            /// Map the given Node to the target enum value
            member this.map (mappers:AllTryFindIdiomaticMappers) (n:Node) = 
                let FindUnionCase s =
                    this.EnumCases 
                    |>  List.tryFind(fun ucm -> s = ucm.YamlName)
                    |>  function
                        |   Some v  ->  FallibleOption<_,_>.Value v
                        |   None    ->  FallibleOption<_,_>.ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) (sprintf "Union case '%s' not availble in type: '%s'" s this.EnumType.FullName))]

                let WrappedStyle() =
                    faillableSequence {
                        let! d = getScalarNodeQuiet n
                        let! uc = FindUnionCase d.Data
                        return uc.EnumValue |> box
                    }

                WrappedStyle()

            /// Returns the default value of the target type - which is unavailable for enum
            member this.Default
                with get() = FallibleOption<_,_>.NoResult()


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
        UnionCaseMapping : (string * YTMRef) list
    }
    with
        /// Return a DiscriminatedUnionMappingInfo when the given type is a Discriminated Union
        static member TryFindMapper (mappers:AllTryFindIdiomaticMappers) (t:Type) : TryFindMapperReturnType =
            if mappers.HasType t then
                FallibleOption<_,_>.Value (mappers.GetRef t, mappers)
            else
                if FSharpType.IsUnion(t) then
                    let (r,nm) = mappers.RegisterType t

                    let ucl = FSharpType.GetUnionCases(t) |> List.ofArray

                    let duf =
                        GetCustomAttributeTp<YamlFieldAttribute> t
                        |>  fun hasName ->
                            match hasName.Result with
                            |   FallibleOption.Value -> Some hasName.Data.Name'
                            |   _ -> None

                    let (mappedFields,nmprs) =
                        ucl
                        |>  List.fold(fun (mfl, (nmpl:AllTryFindIdiomaticMappers)) uc ->
                                let fld = uc.GetFields() |> List.ofArray
                                match fld with
                                |   []      ->  FallibleOption<_,_>.NoResult() :: mfl, nmpl
                                |   [fd]    ->  
                                    nmpl.TryFindMapper(fd.PropertyType)
                                    |>  fun foundMapper ->
                                        match foundMapper.Result with
                                        |   FallibleOption.Value -> 
                                            let (vmp, vmptl)  = foundMapper.Data
                                            FallibleOption<_,_>.Value(uc.Name, vmp) :: mfl , vmptl
                                        |   FallibleOption.NoResult -> FallibleOption<_,_>.NoResult() :: mfl , nmpl
                                        |   FallibleOption.ErrorResult -> FallibleOption<_,_>.ErrorResult foundMapper.Error :: mfl , nmpl
                                |   _       ->  FallibleOption<_,_>.ErrorResult [(ParseMessageAtLine.Create NoDocumentLocation (sprintf "Union case contains more than one data-type '%s' in type: '%s'" uc.Name t.FullName))] :: mfl, nmpl
                        ) ([], nm)

                    let ucmp =
                        ucl
                        |>  List.map(fun uc ->
                            GetCustomAttributeDU<YamlValueAttribute> uc
                            |>  fun customerAttrib ->
                                match customerAttrib.Result with
                                |   FallibleOption.NoResult -> { YamlName = uc.Name; DUName = uc.Name; UCI = uc} |> FallibleOption<_,_>.Value
                                |   FallibleOption.Value  -> { YamlName = customerAttrib.Data.Id'; DUName = uc.Name; UCI = uc} |> FallibleOption<_,_>.Value
                                |   FallibleOption.ErrorResult -> FallibleOption<_,_>.ErrorResult customerAttrib.Error
                                |   _ -> failwith "Illegal value for customerAttrib" 
                        )

                    ucmp
                    |>  FallibleOption.errorsOrValues(fun ucm -> 
                        let um = ucm |> List.map(fun d -> d.Data)
                        mappedFields
                        |>  FallibleOption.errorsOrValues(fun cml -> 
                            let d = { DUType = t; DUField = duf; UnionCases = um; UnionCaseMapping = cml |> List.map(fun d -> d.Data)} :> IYamlToNativeMapping 
                            (r, nmprs.RegisterMapper r d) |> FallibleOption<_,_>.Value)
                    )
                else
                    FallibleOption<_,_>.NoResult()

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
            member this.map (mappers:AllTryFindIdiomaticMappers) (n:Node) = 
                let FindUnionCase s =
                    this.UnionCases 
                    |>  List.tryFind(fun ucm -> s = ucm.YamlName)
                    |>  function
                        |   Some v  ->  FallibleOption<_,_>.Value v
                        |   None    ->  FallibleOption<_,_>.ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) (sprintf "Union case '%s' not availble in type: '%s'" s this.DUType.FullName))]

                let PlainStyle() =
                    faillableSequence {
                        let! d = getScalarNodeQuiet n
                        let! uc = FindUnionCase (d.Data)
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
                    |   None -> FallibleOption<_,_>.NoResult()
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
                                                FallibleOption<_,_>.NoResult()
                                        return r
                                    }
                                    )
                                |>  List.tryFindFo(id)
                            let (m,y) = this.UnionCaseMapping |> List.find(fun (m,_) -> m = duid.DUName)
                            let! data = mappers.map y n
                            return FSharpValue.MakeUnion (duid.UCI, [|data|]) |> box
                        }
                let r = [PlainStyle; EmbeddedStyle] |>  List.tryFindFo(fun f -> f())
                r

            /// Returns the default value of the target type - which is unavailable for DU's
            member this.Default
                with get() = FallibleOption<_,_>.NoResult()


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
let MapYamlDocumentToNative (mappers:AllTryFindIdiomaticMappers) (mapper:IYamlToNativeMapping) (pdr:ParsedDocumentResult) =
    mapper.map mappers (pdr.Document)
    |>  fun mapper ->
        match mapper.Result with
        |   FallibleOption.NoResult -> Error.Create ([ParseMessageAtLine.Create NoDocumentLocation  "Document cannot be mapped"]) (pdr.Warn) (pdr.StopLocation) |> WithErrors
        |   FallibleOption.ErrorResult -> Error.Create (mapper.Error) (pdr.Warn) (pdr.StopLocation) |> WithErrors
        |   FallibleOption.Value -> 
            let d = unbox<'tp> (mapper.Data)
            Success<'tp>.Create d (pdr.Warn) |> Processed
        |   _ -> failwith "Illegal value for mapper"

/// Parses a yaml string, for the given yaml-schema and maps it to a native type instance
let ParseYamlToNative (mapToNative:ParsedDocumentResult -> Result<'tp>) schema yml =
    let yamlParser = Yaml12Parser(schema)
    (yamlParser.``l-yaml-stream`` yml) 
    |> List.map(fun ymlpl ->
        match ymlpl with
        |   NoRepresentation err -> Error.Create (err.Error) (err.Warn) (err.StopLocation) |> WithErrors
        |   EmptyRepresentation mt -> Error.Create ([ParseMessageAtLine.Create NoDocumentLocation  "Document was empty"]) (mt.Warn) (mt.StopLocation) |> WithErrors
        |   PartialRepresentaton pdr
        |   CompleteRepresentaton pdr -> mapToNative pdr
    )


/// Customized yaml deserialization, where one can inject everything required
let CustomDeserializeYaml<'tp> (tryFindMappers:TryFindIdiomaticMapperForType list) (mapYmlDocToNative:AllTryFindIdiomaticMappers->IYamlToNativeMapping->ParsedDocumentResult->Result<'tp>) (parseYmlToNative:(ParsedDocumentResult -> Result<'tp>) -> GlobalTagSchema -> string -> Result<'tp> list) schema nullTagUri stringTagUri yml : Result<'tp> list =
    CreateTypeMappings<'tp> tryFindMappers nullTagUri stringTagUri
    |>  fun typeMapping ->
        match typeMapping.Result with
        |   FallibleOption.NoResult -> [Error.Create ([ParseMessageAtLine.Create (DocumentLocation.Create 0 0) "Cannot find yaml to type mappers"]) [] NoDocumentLocation |> WithErrors]
        |   FallibleOption.ErrorResult -> [Error.Create (typeMapping.Error) [] NoDocumentLocation |> WithErrors]
        |   FallibleOption.Value -> 
            let (rf,mappings) = typeMapping.Data
            parseYmlToNative (mapYmlDocToNative mappings (mappings.GetMapper rf)) schema yml
        |   _ -> failwith "Illegal value for typeMapping"


