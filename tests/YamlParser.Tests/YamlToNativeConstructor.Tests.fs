module YamlToNativeConstructor.Tests

open NUnit.Framework
open FsUnit
open YamlToNativeConstructor


type SimpleRecord = {
    Name   : string
    Age     : int
}

[<Test>]
let ``Deserialize - Naked Record Fields - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Age: 43 }"
    let res = Deserialize<SimpleRecord> yml |> Option.get
    res.Name    |> should equal "Frank"
    res.Age     |> should equal 43

[<Test>]
let ``Deserialize - Naked Record Fields - default values - Sunny Day`` () =
    let yml = "{ Name: 'Frank' }"
    let res = Deserialize<SimpleRecord> yml |> Option.get
    res.Name    |> should equal "Frank"
    res.Age     |> should equal 0



type SimpleAnnotatedRecord = {
    [<YamlField(Name = "name")>] Name   : string
    [<YamlField(Name = "age")>] Age     : int
}

[<Test>]
let ``Deserialize - Annotated Record Fields - Sunny Day`` () =
    let yml = "{ name: 'Frank', age: 43 }"
    let res = Deserialize<SimpleAnnotatedRecord> yml |> Option.get
    res.Name    |> should equal "Frank"
    res.Age     |> should equal 43

[<Test>]
let ``Deserialize - Annotated Record Fields - default values - Sunny Day`` () =
    let yml = "{ name: 'Frank' }"
    let res = Deserialize<SimpleAnnotatedRecord> yml |> Option.get
    res.Name    |> should equal "Frank"
    res.Age     |> should equal 0


type OptionalField = {
    Name   : string
    Age    : int option
}

[<Test>]
let ``Deserialize - Optional Record Field - present - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Age: 43 }"
    let res = Deserialize<OptionalField> yml |> Option.get
    res.Name    |> should equal "Frank"
    res.Age     |> should equal (Some 43)


[<Test>]
let ``Deserialize - Optional Record Fields - missing - Sunny Day`` () =
    let yml = "{ Name: 'Frank' }"
    let res = Deserialize<OptionalField> yml |> Option.get
    res.Name    |> should equal "Frank"
    res.Age     |> should equal None



type ListField = {
    Name   : string
    Age    : int option
}