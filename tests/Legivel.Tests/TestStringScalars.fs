module TestStringScalars

open Legivel
open Legivel.RepresentationGraph
open NUnit.Framework
open System
open System.Diagnostics
open FsUnitTyped
open Legivel.TagResolution

open TestUtils


//  Double quote strings

[<Test>]
let ``Test Double Quoted Single Line - Simple``() =
    let yml = YamlParse "\"my simple string\"" 
    [yml] |> Some |> ToScalar |> shouldEqual "my simple string"


//  Single Quoted strings
[<Test>]
let ``Test Single Quoted Single Line - Simple``() =
    let yml = YamlParse "'my simple string'" 
    [yml] |> Some |> ToScalar |> shouldEqual "my simple string"
