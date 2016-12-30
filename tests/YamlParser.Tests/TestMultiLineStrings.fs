module TestMultiLineStrings

open YamlParse
open RepresentationGraph
open NUnit.Framework
open System

let engine = FlowCollectionStyles()


let ``s-l+block-node`` s = 
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
    Assert.AreEqual("trimmed\n\n\nas space", ``s-l+block-node`` ">-\n  trimmed\n  \n \n\n  as\n  space")

[<Test>]
let ``Test Block Folding with indent``() =
    Assert.AreEqual("foo \n\n\t bar\n\nbaz\n", ``s-l+block-node`` ">\n  foo \n \n  \t bar\n\n  baz\n")


//[<Test>]
//let ``Test Flow Folding``() =
//    Assert.AreEqual(" foo\nbar\nbaz ", ``s-l+block-node`` "\"\n  foo \n \n  \t bar\n\n  baz\n\"")


//  http://www.yaml.org/spec/1.2/spec.html#id2796251
[<Test>]
let ``Test Folded Style``() =
    Assert.AreEqual("\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n", ``s-l+block-node`` ">\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n")


