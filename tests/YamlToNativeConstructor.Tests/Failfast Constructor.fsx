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


//  https://stackoverflow.com/questions/44804767/f-create-custom-attribute-to-expression
type YamlFieldAttribute(Name : string)  = 
    inherit System.Attribute()
    member this.Name' = Name


type MyRec = {
    [<YamlField(Name = "Name")>] Name : string
    [<YamlField(Name = "Age")>] Age  :int
}


type NativeTypeKind =
    |   Record of Type
    |   Unsupported

let engine = Yaml12Parser()

let Deserialize (tp:Type) yml =
    let nttp = 
        if FSharpType.IsRecord tp then Record(tp)
        else Unsupported
    let ymlp = (engine.``l-yaml-stream`` YamlExtendedSchema s)
    match ymlp with
    |   Repr



let yml = "{ Name: 'Frank', Age: 43 }"

Deserialize typeof<MyRec> yml



FSharpType.GetRecordFields typeof<MyRec>
FSharpValue.MakeRecord (typeof<MyRec>, [|box("Frank"); box(43)|])
