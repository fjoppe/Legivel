module TestScalars

open YamlParse
open NUnit.Framework
open FsUnit
open RepresentationGraph
open System

let ToScalar n = 
    match n with
    |   ScalarNode nd -> nd.Data
    |   _ -> raise (Exception "Is no scalar")

let GetNodeTagUri (n:Node) = n.NodeTag.Uri
   

[<Test>]
let ``Test Canonical Integers - Simple``() =
    IntegerGlobalTag.Canonical "5" |> should equal "+5"

[<Test>]
let ``Test Canonical Integers - Binary``() =
    IntegerGlobalTag.Canonical "0b101" |> should equal "+5"

[<Test>]
let ``Test Canonical Integers - Octal``() =
    IntegerGlobalTag.Canonical "017" |> should equal "+15"

[<Test>]
let ``Test Canonical Integers - Hexadecimal``() =
    IntegerGlobalTag.Canonical "0x12" |> should equal "+18"

[<Test>]
let ``Test Canonical Integers - Sexagesimal``() =
    IntegerGlobalTag.Canonical "190:20:30" |> should equal "+685230"

[<Test>]
let ``Test Canonical Floats - Simple``() =
    FloatGlobalTag.Canonical "81.23" |> should equal "+0.8123e+002"
    float(FloatGlobalTag.Canonical "81.23") |> should equal (float "81.23")

[<Test>]
let ``Test Canonical Floats - Shifted decimal``() =
    FloatGlobalTag.Canonical "0.008123" |> should equal "+0.8123e-002"
    float(FloatGlobalTag.Canonical "0.008123") |> should equal (float "0.008123")

[<Test>]
let ``Test Canonical Floats - Normalized decimal``() =
    FloatGlobalTag.Canonical "1.008123" |> should equal "+0.1008123e+001"
    float(FloatGlobalTag.Canonical "1.008123") |> should equal (float "1.008123")  

    FloatGlobalTag.Canonical "0.8123" |> should equal "+0.8123e+000"
    float(FloatGlobalTag.Canonical "0.8123") |> should equal (float "+0.8123e+000")

[<Test>]
let ``Test Canonical Floats - Sexagesimal``() =
    FloatGlobalTag.Canonical "190:20:30.15" |> should equal "+0.68523015e+006"
    float(FloatGlobalTag.Canonical "190:20:30.15") |> should equal (float "685230.15")

[<Test>]
let ``Test Canonical Floats - Specials``() =
    FloatGlobalTag.Canonical ".inf"  |> should equal "+.inf"
    FloatGlobalTag.Canonical "+.inf" |> should equal "+.inf"
    FloatGlobalTag.Canonical "-.inf" |> should equal "-.inf"
    FloatGlobalTag.Canonical ".nan"  |> should equal ".nan"

[<Test>]
let ``Test MapScalar Null - Sunny Day``() =
    ["null"; "NULL"; "Null"]
    |> List.iter(fun input ->
        let node = MapScalar input
        node         |> ToScalar      |> should equal input
        node         |> GetNodeTagUri |> should equal NullGlobalTag.Uri
    )

[<Test>]
let ``Test MapScalar Null - Rainy Day``() =
    [
        "null and more text"
        "null\nand more text"
    ]
    |> List.iter(fun input ->
        let faultyNode = MapScalar input
        faultyNode |> ToScalar      |> should equal input
        faultyNode |> GetNodeTagUri |> should equal StringGlobalTag.Uri
    )

[<Test>]
let ``Test MapScalar Bool - Sunny Day``() =
    [
        "yes";"no";"y";"n";"Y";"N";"Yes";"No";"YES";"NO";
        "true";"false";"True";"False";"TRUE";"FALSE";
        "on";"off";"On";"Off";"ON";"OFF"
    ]
    |> List.iter(fun input ->
        let node = MapScalar input
        node        |> ToScalar      |> should equal input
        node        |> GetNodeTagUri |> should equal BooleanGlobalTag.Uri
    )

[<Test>]
let ``Test MapScalar Bool - Rainy Day``() =
    [
        "true and more text"
        "true\nand more text"
    ]
    |> List.iter(fun input ->
        let faultyNode = MapScalar input
        faultyNode |> ToScalar      |> should equal input
        faultyNode |> GetNodeTagUri |> should equal StringGlobalTag.Uri
    )

[<Test>]
let ``Test MapScalar Intgeger - Sunny Day``() =
    [
        "1"; "100"; "1000"
        "-1"; "-100"; "-1000"
        "+1"; "+100"; "+1000"
        "0b1"; "0b100"; "0b1000"
        "-0b1"; "-0b100"; "-0b1000"
        "+0b1"; "+0b100"; "+0b1000"
        "0x1"; "0x100"; "1000"
        "-0x1"; "-0x100"; "-0x1000"
        "+0x1"; "+0x100"; "+0x1000"
        "190:20:30"
    ]
    |> List.iter(fun input ->
        let node = MapScalar input
        node         |> ToScalar      |> should equal input
        node         |> GetNodeTagUri |> should equal IntegerGlobalTag.Uri
    )

[<Test>]
let ``Test MapScalar Integer - Rainy Day``() =
    [
        "100 and more text"
        "100\nand more text"
    ]
    |> List.iter(fun input ->
        let faultyNode = MapScalar input
        faultyNode |> ToScalar      |> should equal input
        faultyNode |> GetNodeTagUri |> should equal StringGlobalTag.Uri
    )