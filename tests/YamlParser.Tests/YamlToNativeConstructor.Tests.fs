module YamlToNativeConstructor.Tests

open NUnit.Framework
open FsUnit
open YamlToNativeConstructor

type MyRec = {
    [<YamlField(Name = "name")>] Name   : string
    [<YamlField(Name = "age")>] Age     : int
}

[<Test>]
let ``Deserialize - Named Record Fields - Sunny Day`` () =
    let yml = "{ name: 'Frank', age: 43 }"
    let res = Deserialize<MyRec> yml |> Option.get
    res.Name    |> should equal "Frank"
    res.Age     |> should equal 43

[<Test>]
let ``Deserialize - Named Record Fields - default values - Sunny Day`` () =
    let yml = "{ name: 'Frank' }"
    let res = Deserialize<MyRec> yml |> Option.get
    res.Name    |> should equal "Frank"
    res.Age     |> should equal 0


