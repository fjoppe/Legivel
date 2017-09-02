#I __SOURCE_DIRECTORY__ 
#I "../../packages"
#I "../.."

#r @"bin/YamlParser/YamlParser.dll"
#r @"bin/YamlToNativeConstructor/YamlToNativeConstructor.dll"
#r @"NLog/lib/net45/NLog.dll"

open YamlParse
open TagResolution
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Reflection.FSharpReflectionExtensions
open System
open NLog
open RepresentationGraph
open System.Reflection
open YamlToNativeConstructor


type MyRec = {
    [<YamlField(Name = "name")>] Name   : string
    [<YamlField(Name = "age")>] Age     : int option
}

let yml = "{ name: 'Frank', age: 43 }"

Deserialize<MyRec> yml
    

FSharpType.GetRecordFields typeof<MyRec> 

// let fields = FSharpType.GetRecordFields typeof<'tp> |> List.ofArray
let  fields = FSharpType.GetRecordFields typeof<MyRec> |> List.ofArray |> List.head

fields.PropertyType.FullName = typeof<FSharp.Core.Option<obj>>.FullName

let a =
    f.CustomAttributes
    |> List.ofSeq
    |> List.filter(fun fa -> fa.AttributeType = typeof<YamlFieldAttribute>)
    |> List.head


(5).GetType().GUID = typeof<int>.GUID


let myrec = FSharpValue.MakeRecord (typeof<MyRec>, [|box("Frank"); box(None)|]) :?> MyRec

myrec.Age


let r = FSharp.Core.Option<int>.None


typeof<FSharp.Core.Option<int>>.BaseType = typeof<FSharp.Core.Option<string>>.FullName

typeof<Option<int>> = typeof<Option<int>>
typeof<Option<int>> = typeof<Option<string>>

typeof<MyRec>.GetProperty("Age").PropertyType.GUID = typeof<FSharp.Core.Option<string>>.GUID

typeof<MyRec>.GetProperty("Age").PropertyType.GetConstructors()


let g = "{2f31d2f5-62af-3b46-8520-5b7c4151745d}"

{ Name = "f"; Age =  Option<int>.None}


typeof<FSharp.Core.Option<int>>.Name = typeof<FSharp.Core.Option<string>>.Name

