module YamlToNativeConstructor.Tests

open NUnit.Framework
open FsUnit
open YamlToNativeConstructor


type SimpleRecord = {
    Name   : string
    Age     : int
}


let DeserializeSuccess<'tp> yml = 
    Deserialize<'tp> yml
    |>  function
        |   Succes s -> s.Data
        |   Error _ -> failwith "Unexpected error"
        
let DeserializeError<'tp> yml = 
    Deserialize<'tp> yml
    |>  function
        |   Succes _ -> failwith "Unexpected success"
        |   Error e -> e


[<Test>]
let ``Deserialize - Naked Record Fields - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Age: 43 }"
    let res = DeserializeSuccess<SimpleRecord> yml 
    res.Name    |> should equal "Frank"
    res.Age     |> should equal 43

[<Test>]
let ``Deserialize - Naked Record Fields - default values - Sunny Day`` () =
    let yml = "{ Name: 'Frank' }"
    let res = DeserializeSuccess<SimpleRecord> yml 
    res.Name    |> should equal "Frank"
    res.Age     |> should equal 0



type SimpleAnnotatedRecord = {
    [<YamlField(Name = "name")>] Name   : string
    [<YamlField(Name = "age")>] Age     : int
}

[<Test>]
let ``Deserialize - Annotated Record Fields - Sunny Day`` () =
    let yml = "{ name: 'Frank', age: 43 }"
    let res = DeserializeSuccess<SimpleAnnotatedRecord> yml 
    res.Name    |> should equal "Frank"
    res.Age     |> should equal 43

[<Test>]
let ``Deserialize - Annotated Record Fields - default values - Sunny Day`` () =
    let yml = "{ name: 'Frank' }"
    let res = DeserializeSuccess<SimpleAnnotatedRecord> yml 
    res.Name    |> should equal "Frank"
    res.Age     |> should equal 0


type OptionalField = {
    Name   : string
    Age    : int option
}

[<Test>]
let ``Deserialize - Optional Record Field - present - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Age: 43 }"
    let res = DeserializeSuccess<OptionalField> yml 
    res.Name    |> should equal "Frank"
    res.Age     |> should equal (Some 43)


[<Test>]
let ``Deserialize - Optional Record Fields - missing - Sunny Day`` () =
    let yml = "{ Name: 'Frank' }"
    let res = DeserializeSuccess<OptionalField> yml 
    res.Name    |> should equal "Frank"
    res.Age     |> should equal None


type NestedRecord = {
        Street      : string
        HouseNumber : int
    }
type ContainingNested = {
        Name    : string
        Address : NestedRecord
    }


[<Test>]
let ``Deserialize - Nested Record - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Address: { Street: 'Rosegarden', HouseNumber: 5 } }"
    let res = DeserializeSuccess<ContainingNested> yml 
    res.Name    |> should equal "Frank"
    res.Address.Street |> should equal "Rosegarden"
    res.Address.HouseNumber |> should equal 5

type ContainingOptionalNested = {
        Name    : string
        Address : NestedRecord option
    }

[<Test>]
let ``Deserialize - Nested Optional Record - present - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Address: { Street: 'Rosegarden', HouseNumber: 5 } }"
    let res = DeserializeSuccess<ContainingOptionalNested> yml 
    res.Name    |> should equal "Frank"
    res.Address |> Option.get |> fun a -> a.Street |> should equal "Rosegarden"
    res.Address |> Option.get |> fun a -> a.HouseNumber |> should equal 5

[<Test>]
let ``Deserialize - Nested Optional Record - missing - Sunny Day`` () =
    let yml = "{ Name: 'Frank' }"
    let res = DeserializeSuccess<ContainingOptionalNested> yml 
    res.Name    |> should equal "Frank"
    res.Address |> should equal None


[<Test>]
let ``Deserialize - List - Sunny Day`` () =
    let yml = "[1, 1, 3, 5, 8, 9]" // anti pattern :)
    let res = DeserializeSuccess<int list> yml 
    res |> should equal [1; 1; 3; 5; 8; 9]


type ListField = {
    Name   : string
    Scores : int list
}

[<Test>]
let ``Deserialize - Record with List - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Scores: [1, 1, 3, 5, 8, 9]}"
    let res = DeserializeSuccess<ListField> yml 
    res.Name    |> should equal "Frank"
    res.Scores  |> should equal [1; 1; 3; 5; 8; 9]


