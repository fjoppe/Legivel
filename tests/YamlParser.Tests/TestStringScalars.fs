module TestStringScalars

open YamlParse
open RepresentationGraph
open NUnit.Framework
open System
open System.Diagnostics
open FsUnit

[<DebuggerStepThrough>]
let YamlParse s = 
    let engine = Yaml12Parser()
    let ps = ParseState.Create s
    let ps = ps.SetIndent -1
    let ps = ps.SetSubIndent 0
    let ps = ps.SetStyleContext ``Block-in``
    let d = engine.``s-l+block-node`` ps 
    match (fst(d.Value)) with
    |   ScalarNode  node -> node.Data
    |   _ -> raise (Exception "Not scalar data")

//  http://www.yaml.org/spec/1.2/spec.html#id2779048
[<Test>]
let ``Test Simple Line folding``() =
    YamlParse ">-\n  trimmed\n  \n \n\n  as\n  space" |> should equal "trimmed\n\n\nas space"

[<Test>]
let ``Test Block Folding with indent``() =
    YamlParse ">\n  foo \n \n  \t bar\n\n  baz\n" |> should equal "foo \n\n\t bar\n\nbaz\n"


//  Double quote strings

[<Test>]
let ``Test Double Quoted Single Line - Simple``() =
    YamlParse "\"my simple string\"" |> should equal "my simple string"

[<Test>]
let ``Test Double Quote Flow Folding - Simple 1``() =
    YamlParse "\"\n  foo \n \n  \t bar\n\n  baz\n\"" |> should equal " foo\nbar\nbaz "

[<Test>]
let ``Test Double Quote Flow Folding - Simple 2``() =
    YamlParse "\" 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty \"" |> should equal " 1st non-empty\n2nd non-empty 3rd non-empty " 

[<Test>]
let ``Test Double Quote Flow Folding - Escaped``() =
    YamlParse "\"folded \nto a space,\t\n \nto a line feed, or \t\\\n \\ \tnon-content\"" |> should equal "folded to a space,\nto a line feed, or \t \tnon-content"

//  http://www.yaml.org/spec/1.2/spec.html#id2796251
[<Test>]
let ``Test Folded Style``() =
    YamlParse ">\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n" |> should equal "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n" 

//  Single Quoted strings
[<Test>]
let ``Test Single Quoted Single Line - Simple``() =
    YamlParse "'my simple string'" |> should equal "my simple string"

[<Test>]
let ``Test Single Quoted Single Line - Escaped``() =
    YamlParse "'here''s to \"quotes\"'" |> should equal "here's to \"quotes\""

[<Test>]
let ``Test Single Quote Flow Folding - Simple``() =
    YamlParse "' 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty '" |> should equal " 1st non-empty\n2nd non-empty 3rd non-empty "

