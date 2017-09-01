#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#r @"bin/Debug/YamlParser.dll"
#r @"NLog/lib/net45/NLog.dll"

open YamlParse
open TagResolution
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Reflection.FSharpReflectionExtensions
open System
open NLog
open RepresentationGraph
open System.Reflection


let typeMappings = [
    (YamlExtended.StringGlobalTag, typeof<string>, fun (s:string) -> box s)
    (YamlExtended.IntegerGlobalTag, typeof<int>, fun (s:string) -> YamlExtended.IntegerGlobalTag.ToCanonical s |> Option.get |> Int32.Parse |> box)
    (YamlExtended.FloatGlobalTag, typeof<float>, fun (s:string) -> YamlExtended.FloatGlobalTag.ToCanonical s |> Option.get |> Double.Parse |> box)
    (YamlExtended.BooleanGlobalTag, typeof<bool>, fun (s:string) -> YamlExtended.BooleanGlobalTag.ToCanonical s |> Option.get |> Boolean.Parse |> box)
    ]


//  https://stackoverflow.com/questions/44804767/f-create-custom-attribute-to-expression
type YamlFieldAttribute(Name : string)  = 
    inherit System.Attribute()
    member this.Name' = Name


type MyRec = {
    [<YamlField(Name = "name")>] Name   : string
    [<YamlField(Name = "age")>] Age     : int
}



type FieldMapping = {
        YamlToNative : Map<string,string*Type>
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
        FieldMapping : FieldMapping
    }
    with
        member this.map (n:Node) =
            let mn = getMapNode n
            mn.Data
            |>  List.map(fun (k,v) ->
                let kf = getScalarNode k
                if this.FieldMapping.YamlToNative.ContainsKey(kf.Data) then
                    let (constructingFieldName, constructingType) = this.FieldMapping.YamlToNative.[kf.Data]
                    match v with
                    |   ScalarNode sn -> 
                        let convOpt = 
                            typeMappings 
                            |>  List.tryFind(fun (tg,tp,cf) -> v.NodeTag.Uri = tg.Uri && constructingType.GUID = tp.GUID)
                            |>  Option.map(fun (_,_,cf) -> cf)
                        match convOpt with
                        |   None -> failwith (sprintf "Incompatible type for field: %s" constructingFieldName)
                        |   Some conv ->
                            let constructingFiedValue = conv sn.Data
                            Some(constructingFieldName, constructingFiedValue)
                    |   _ -> failwith "Expecting a scalar node"
                else
                    None
                )
                |>  List.filter(fun e -> e <> None)
                |>  List.map(Option.get)


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
    |   x   -> failwith (sprintf "'%s.%s' has more than one attributes of type '%s'" (st.MemberType.ToString()) (st.Name) (typeof<'T>.FullName))


let CreatTypeMappings<'tp>() =
    if FSharpType.IsRecord typeof<'tp> then
        let fields = FSharpType.GetRecordFields typeof<'tp> |> List.ofArray
        let mappings =
            fields
            |>  List.fold(fun (s:Map<string,string*Type>) f ->
                match (GetCustomAttribute<YamlFieldAttribute> f) with
                |   None    -> s.Add(f.Name, (f.Name, f.PropertyType))
                |   Some at -> s.Add(at.Name', (f.Name, f.PropertyType))
            ) Map.empty<string,string*Type>
        let fieldMapping = { YamlToNative = mappings }
        Record({Target = typeof<'tp>; FieldMapping = fieldMapping })
        |> Some
    else
        None


let engine = Yaml12Parser()

let Deserialize<'tp> yml =
    let mappings = 
        CreatTypeMappings<'tp>()
        |>  function
        |   None    -> failwith (sprintf "Unsupported type: '%s'" (typeof<'tp>.FullName))
        |   Some mp -> mp

    let ymlpl = (engine.``l-yaml-stream`` YamlExtended.Schema yml) |> List.head
    match ymlpl with
    |   NoRepresentation err -> failwith "Exited with parse errors"
    |   EmptyRepresentation mt -> None
    |   PartialRepresentaton pdr 
    |   CompleteRepresentaton pdr -> 
            mappings.map (pdr.Document)
            |> Some


let yml = "{ name: 'Frank', age: 43 }"

Deserialize<MyRec> yml
    

FSharpType.GetRecordFields typeof<MyRec> 

let f = FSharpType.GetRecordFields typeof<MyRec> |> List.ofArray |> List.head

let a =
    f.CustomAttributes
    |> List.ofSeq
    |> List.filter(fun fa -> fa.AttributeType = typeof<YamlFieldAttribute>)
    |> List.head


(5).GetType().GUID = typeof<int>.GUID


FSharpValue.MakeRecord (typeof<MyRec>, [|box("Frank"); box(43)|])

