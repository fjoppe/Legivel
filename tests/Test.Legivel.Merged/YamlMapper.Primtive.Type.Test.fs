module Legivel.Mapping.Primitive.Type.Tests


open NUnit.Framework
open FsUnitTyped
open Legivel.Serialization
open System


type SimpleRecord = {
    Name   : string
    Age    : int
}


let DeserializeSuccess<'tp> yml = 
    let r = Deserialize<'tp> yml
    r
    |> List.head
    |>  function
        |   Success s -> s.Data
        |   Error e -> failwith "Unexpected error"
      
      
let DeserializeError<'tp> yml = 
    Deserialize<'tp> yml
    |> List.head
    |>  function
        |   Success _ -> failwith "Unexpected success"
        |   Error e -> e


[<Test>]
let ``Deserialize - single float - Sunny Day`` () =
    [
        ("0.002", 0.002f)
        ("1.122", 1.122f)
        ("1005.56", 1005.56f)
        ("1005.56", 1005.56f)
        (".NaN", Single.NaN)
        (".inf", Single.PositiveInfinity)
        ("-.INF", Single.NegativeInfinity)
    ]
    |>  List.iter(fun (yml, expected) -> 
        let res = DeserializeSuccess<single> yml 
        res |> shouldEqual expected
    )


[<Test>]
let ``Deserialize - single float - Rainy Day`` () =
    [
        "NaN" 
        "inf" 
        "-INF"
    ]
    |>  List.iter(fun yml -> 
        let err = DeserializeError<single> yml 
        err.Error.Head.Message.StartsWith "Incorrect format" |> shouldEqual true
    )

[<Test>]
let ``Deserialize - double float - Sunny Day`` () =
    [
        ("0.002", 0.002)
        ("1.122", 1.122)
        ("1005.56", 1005.56)
        ("1005.56", 1005.56)
        (".NaN", Double.NaN)
        (".inf", Double.PositiveInfinity)
        ("-.INF", Double.NegativeInfinity)
    ]
    |>  List.iter(fun (yml, expected) -> 
        let res = DeserializeSuccess<float> yml 
        res |> shouldEqual expected
    )


[<Test>]
let ``Deserialize - double float - Rainy Day`` () =
    [
        "NaN" 
        "inf" 
        "-INF"
    ]
    |>  List.iter(fun yml -> 
        let err = DeserializeError<float> yml 
        err.Error.Head.Message.StartsWith "Incorrect format" |> shouldEqual true
    )



