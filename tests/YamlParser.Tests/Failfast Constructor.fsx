#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#r @"bin/Debug/YamlParser.dll"
#r @"NLog/lib/net45/NLog.dll"

open RepresentationGraph
open TagResolution
open RegexDSL
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Reflection.FSharpReflectionExtensions


#load "YamlParserFuncs.fsx"
open Yaml

YamlParse "
- 1
- 2
- 3
"

//  https://stackoverflow.com/questions/44804767/f-create-custom-attribute-to-expression
type YamlFieldAttribute(Name : string, Path : string)  = 
    inherit System.Attribute()
    member this.Name' = Name
    member this.Path' = Path


type MyRec = {
    [<YamlField(Name = "Name", Path = "//")>] Name : string
    [<YamlField(Name = "Age", Path = "..")>] Age  :int
}


FSharpType.GetRecordFields typeof<MyRec>
FSharpValue.MakeRecord (typeof<MyRec>, [|box("Frank"); box(43)|])



