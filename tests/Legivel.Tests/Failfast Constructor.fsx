#I __SOURCE_DIRECTORY__ 
#I "../../packages"
#I "../.."

#r @"bin/Debug/Legivel.Parser.dll"
#r @"bin/Debug/Legivel.Mapper.dll"
#r @"NLog/lib/net45/NLog.dll"

open Legivel.Parser
open Legivel.TagResolution
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Reflection.FSharpReflectionExtensions
open System
open NLog
open Legivel.RepresentationGraph
open System.Reflection
open Legivel.Attributes
open Legivel.Serialization
open System.Dynamic
open NLog.Config


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


(5).GetType().GUID = typeof<int>.GUID


let myrec = FSharpValue.MakeRecord (typeof<MyRec>, [|box("Frank"); box(None)|]) :?> MyRec

myrec.Age


//let r = FSharp.Core.Option<int>.None


//typeof<Option<int>> = typeof<Option<int>>
//typeof<Option<int>> = typeof<Option<string>>

//typeof<MyRec>.GetProperty("Age").PropertyType.GUID = typeof<FSharp.Core.Option<string>>.GUID

//typeof<MyRec>.GetProperty("Age").PropertyType.GetConstructors()


//let g = "{2f31d2f5-62af-3b46-8520-5b7c4151745d}"

//{ Name = "f"; Age =  Option<int>.None}


//typeof<FSharp.Core.Option<int>>.Name = typeof<FSharp.Core.Option<string>>.Name

//typeof<FSharp.Core.Option<MyRec>>.GenericTypeArguments.[0].FullName

//typeof<FSharp.Collections.List<int>>.GenericTypeArguments.[0].FullName

//typeof<List<int>>.GetMember("Empty", BindingFlags.Static ||| BindingFlags.Public)

//let el = typeof<List<int>>.GetProperty("Empty", BindingFlags.Static ||| BindingFlags.Public).GetGetMethod().Invoke(null, [||])

//el.GetType().GetMethods() |> List.ofArray |> List.map(fun mi -> mi.Name)

//el.GetType().GetMethod("Cons").GetParameters() |> List.ofArray |> List.map(fun pi -> pi.Name)

//el.GetType().GetMethod("Cons").Invoke(null, [|1;el|])

//typeof<FSharp.Collections.seq<int>>.Module


type YamlValueAttribute(Id : string)  = 
    inherit System.Attribute()
    new() = YamlValueAttribute("")
    member this.Id' = Id

//
//  Wrapped style
//      post: data  (post is DU-case, data is contained data)
//
//  Contained style:
//      type : int (and other pairs in the mapping form the data - excluding field "type")
//

type YamlUnionCaseFormat =
    |   [<YamlValue("two")>] WithData=0      //  post: data
    |   InData=1        //  type : int (and other pairs in the mapping form the data - excluding field "type")
    |   Plain=2         //  duValue: one
    |   Literal=3       //  duValue: One  



Enum.GetValues(typeof<YamlUnionCaseFormat>) 

let h1 = typeof<YamlUnionCaseFormat>.GetEnumValues()
let h2 = typeof<YamlUnionCaseFormat>.GetEnumNames()
let lf = [for i in h1 do yield i]
typeof<YamlUnionCaseFormat>.GetEnumNames()
let a = lf.Item 0
a.GetType().GetCustomAttributes()

typeof<YamlUnionCaseFormat>.GetField("WithData").GetCustomAttributes(typeof<YamlValueAttribute>)

a.ToString()


h2 |> Array.head

lf.Head.GetType()

h.GetValue(0)


type problem = int list



type Mapping = Map<string,string>

FSharpType.IsUnion(typeof<Mapping>)
FSharpType.IsRecord(typeof<Mapping>)

typeof<YamlUnionCaseFormat>.IsEnum

typeof<Mapping>.GenericTypeArguments |> Array.length

typeof<Mapping>.GetMethods() |> Array.toList |> List.map(fun m -> m.Name)

typeof<Mapping>.GetMembers() |> Array.toList |> List.map(fun m -> m.Name)

let ct =typeof<Mapping>.GetConstructors() |> Array.head

ct.GetParameters()

ct.Invoke([|[]|])

typeof<Mapping>.BaseType.GetProperty("empty")



type DUTest =
    | [<YamlValue>] One of string
    | [<YamlValue("two")>] Two 
    | [<YamlValue("fld3")>] Three


FSharpType.IsUnion(typeof<DUTest>)
FSharpType.IsUnion(typeof<problem>)

FSharpType.GetUnionCases(typeof<problem>)

typeof<DUTest>.IsClass
typeof<problem>.IsClass


let h = FSharpType.GetUnionCases(typeof<DUTest>) |> Array.head

FSharpType.GetUnionCases(typeof<DUTest>)
|>  List.ofArray
|>  List.map(fun e -> e.Name)


let f = h.GetFields() |> Array.head

f.PropertyType




let a = h.GetCustomAttributes() |> Array.head

a.GetType().FullName = typeof<YamlValueAttribute>.FullName

