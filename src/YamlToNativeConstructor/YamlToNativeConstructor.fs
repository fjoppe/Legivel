module YamlToNativeConstructor

open System
open YamlParse
open TagResolution
open RepresentationGraph
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


//  https://stackoverflow.com/questions/44804767/f-create-custom-attribute-to-expression
type YamlFieldAttribute(Name : string)  = 
    inherit System.Attribute()
    member this.Name' = Name

type RefTypeInfo = {
    Constructor     : ConstructorInfo
}

type ValTypeInfo = class end

type TypeStructure =
    |   ValueType // of ValTypeInfo
    |   RefType of RefTypeInfo


type FieldMapping = {
        SortOrder       : int
        YamlName        : string
        PropertyName    : string
        PropertyType    : Type
        Optional        : bool
        TypeStructure   : TypeStructure
    }


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


type RecordMappingInfo = {
        Target       : Type
        FieldMapping : FieldMapping list
    }
    with
        member this.map (n:Node) =
            let mn = getMapNode n
            let dataToFieldMapping =
                mn.Data
                |>  List.map(fun (k,v) ->
                    let kf = getScalarNode k
                    this.FieldMapping
                    |> List.tryFind(fun fm -> fm.YamlName = kf.Data)
                    |> Option.map(fun fm -> 
                        match v with
                        |   ScalarNode sn -> 
                            let yamlTag = 
                                YamlTypeMappings 
                                |>  List.tryFind(fun yt -> v.NodeTag.Uri = yt.YamlTag.Uri && fm.PropertyType.GUID = yt.TargetType.GUID)
                            match yamlTag with
                            |   None -> failwith (sprintf "Incompatible type for field: %s" fm.PropertyName)
                            |   Some yt ->
                                let constructingFiedValue = yt.ToNative sn.Data
                                if fm.Optional then
                                    let (RefType rt) = fm.TypeStructure
                                    (fm.PropertyName, rt.Constructor.Invoke([|constructingFiedValue|]) |> box)
                                else
                                    (fm.PropertyName, constructingFiedValue)
                        |   _ -> failwith "Expecting a scalar node"
                    ))
                    |>  List.filter(fun e -> e <> None)
                    |>  List.map(Option.get)
            let values = 
                this.FieldMapping
                |> List.sortBy(fun fm -> fm.SortOrder) 
                |>  List.map(fun fm -> 
                    let dfs =
                        dataToFieldMapping
                        |>  List.filter(fun (dfn, _) -> dfn = fm.PropertyName)
                    if dfs.Length = 0 then
                        if fm.Optional then
                            None |> box
                        else
                            Activator.CreateInstance(fm.PropertyType) |> box
                    else dfs |> List.head |> snd
                )
            FSharpValue.MakeRecord(this.Target,values |> List.toArray)


type NativeTypeKind =
    |   Record of RecordMappingInfo
    with
        member this.map (n:Node) =
            match this with
            |   Record rc -> rc.map n


let GetCustomAttribute<'T when 'T :> Attribute> (st:MemberInfo) =
    let at = Attribute.GetCustomAttributes(st, typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> None
    |   1   -> Some (at.Head :?> 'T)
    |   _   -> failwith (sprintf "'%s.%s' has more than one attributes of type '%s'" (st.MemberType.ToString()) (st.Name) (typeof<'T>.FullName))


let CreatTypeMappings<'tp>() =
    let IsOptional (t:Type) = t.GUID = typeof<FSharp.Core.Option<obj>>.GUID

    if FSharpType.IsRecord typeof<'tp> then
        let fields = FSharpType.GetRecordFields typeof<'tp> |> List.ofArray
        let mappings =
            fields
            |>  List.mapi(fun i f ->
                let optional = f.PropertyType |> IsOptional
                let propertyType = 
                    if optional then
                        f.PropertyType.GenericTypeArguments.[0]
                    else
                        f.PropertyType
                let typeStructure =
                    if f.PropertyType.IsClass then
                        let ctorInfo = f.PropertyType.GetConstructors() |> Array.head
                        RefType { Constructor = ctorInfo }
                    else
                        ValueType

                match (GetCustomAttribute<YamlFieldAttribute> f) with
                |   None    -> { SortOrder = i; YamlName = f.Name; PropertyName = f.Name; PropertyType = propertyType; Optional = optional; TypeStructure = typeStructure}
                |   Some at -> { SortOrder = i; YamlName = at.Name'; PropertyName = f.Name; PropertyType = propertyType; Optional = optional; TypeStructure = typeStructure}
            ) 
        Record({Target = typeof<'tp>; FieldMapping = mappings })
        |> Some
    else
        None


let Deserialize<'tp> yml : 'tp option =
    let yamlParser = Yaml12Parser()
    let mappings = 
        CreatTypeMappings<'tp>()
        |>  function
        |   None    -> failwith (sprintf "Unsupported type: '%s'" (typeof<'tp>.FullName))
        |   Some mp -> mp

    let ymlpl = (yamlParser.``l-yaml-stream`` YamlExtended.Schema yml) |> List.head
    match ymlpl with
    |   NoRepresentation err -> failwith "Exited with parse errors"
    |   EmptyRepresentation mt -> None
    |   PartialRepresentaton pdr 
    |   CompleteRepresentaton pdr -> 
            mappings.map (pdr.Document) :?> 'tp
            |> Some

