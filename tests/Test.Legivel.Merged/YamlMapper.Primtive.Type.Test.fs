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


[<Test>]
let ``Deserialize - uint16 - Sunny Day`` () =
    [
        ("0", 0us)
        ("1", 1us)
        ("32768", 32768us)
    ]
    |>  List.iter(fun (yml,asrt) -> 
        let r = DeserializeSuccess<uint16> yml 
        r |> shouldEqual asrt
    )


[<Test>]
let ``Deserialize - uint16 - Rainy Day`` () =
    [
        "-1"
        "-10"
        "65536"
        "no a number"
    ]
    |>  List.iter(fun yml -> 
        let err = DeserializeError<uint16> yml 
        err.Error.Head.Message.StartsWith "Incorrect format:" |> shouldEqual true
    )


[<Test>]
let ``Deserialize - uint32 - Sunny Day`` () =
    [
        ("0", 0u)
        ("1", 1u)
        ("32768", 32768u)
    ]
    |>  List.iter(fun (yml,asrt) -> 
        let r = DeserializeSuccess<uint32> yml 
        r |> shouldEqual asrt
    )


[<Test>]
let ``Deserialize - uint32 - Rainy Day`` () =
    [
        "-1"
        "-10"
        "4294967296"
        "no a number"
    ]
    |>  List.iter(fun yml -> 
        let err = DeserializeError<uint32> yml 
        err.Error.Head.Message.StartsWith "Incorrect format:" |> shouldEqual true
    )


[<Test>]
let ``Deserialize - uint64 - Sunny Day`` () =
    [
        ("0", 0UL)
        ("1", 1UL)
        ("32768", 32768UL)
    ]
    |>  List.iter(fun (yml,asrt) -> 
        let r = DeserializeSuccess<uint64> yml 
        r |> shouldEqual asrt
    )


[<Test>]
let ``Deserialize - uint64 - Rainy Day`` () =
    [
        "-1"
        "-10"
        "9223372036854775808"
        "no a number"
    ]
    |>  List.iter(fun yml -> 
        let err = DeserializeError<uint64> yml 
        err.Error.Head.Message.StartsWith "Incorrect format:" |> shouldEqual true
    )
