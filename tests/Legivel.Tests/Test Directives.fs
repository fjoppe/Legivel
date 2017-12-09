module Test_Directives

//  Extra Directives tests, not covered by the examples

open NUnit.Framework
open FsUnitTyped
open Legivel.TagResolution
open Legivel.RepresentationGraph
open Legivel.Internals
open TestUtils


[<Test>]
let ``TAG Idrective with invalid Uri``() =
    let err = YamlParseWithErrors "
%TAG !invalid! :not/valid/uri
!invalid!bar"
    err.Error.Length |> shouldBeGreaterThan 0
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Tag is not a valid Uri")) |> List.length |> shouldEqual 1

