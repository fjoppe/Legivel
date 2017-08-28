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


//let typaMappings = [
//    (YamlExtended.StringGlobalTag, typeof<string>, fun s -> box s)
//    (YamlExtended.IntegerGlobalTag, typeof<int>, fun s -> YamlExtended.IntegerGlobalTag.c
//    ]

//  https://stackoverflow.com/questions/44804767/f-create-custom-attribute-to-expression
type YamlFieldAttribute(Name : string)  = 
    inherit System.Attribute()
    member this.Name' = Name


type MyRec = {
    [<YamlField(Name = "name")>] Name   : string
    [<YamlField(Name = "age")>] Age     : int
}



type FieldMapping = {
        YamlToNative : Map<string,string>
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
                match v with
                |   ScalarNode sn -> 
                    sn.Data
                |   _ -> failwith "Expecting a scalar node"
                )
            |   _ -> failwith "Expecting a mapping node"


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
            |>  List.fold(fun (s:Map<string,string>) f ->
                match (GetCustomAttribute<YamlFieldAttribute> f) with
                |   None    -> s.Add(f.Name, f.Name)
                |   Some at -> s.Add(at.Name', f.Name)
            ) Map.empty<string,string>
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
    |   CompleteRepresentaton pdr -> mappings.map <| pdr.Document


let yml = "{ Name: 'Frank', Age: 43 }"

Deserialize typeof<MyRec> yml
    



let f = FSharpType.GetRecordFields typeof<MyRec> |> List.ofArray |> List.head

let a =
    f.CustomAttributes
    |> List.ofSeq
    |> List.filter(fun fa -> fa.AttributeType = typeof<YamlFieldAttribute>)
    |> List.head




FSharpValue.MakeRecord (typeof<MyRec>, [|box("Frank"); box(43)|])
