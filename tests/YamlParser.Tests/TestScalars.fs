module TestScalars

open YamlParse
open NUnit.Framework
open FsUnit

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

