module TestScalars

open YamlParse
open NUnit.Framework

[<Test>]
let ``Test Canonical Integers - Simple``() =
    Assert.AreEqual(IntegerGlobalTag.Canonical "5", "+5")

[<Test>]
let ``Test Canonical Integers - Binary``() =
    Assert.AreEqual(IntegerGlobalTag.Canonical "0b101", "+5")

[<Test>]
let ``Test Canonical Integers - Octal``() =
    Assert.AreEqual(IntegerGlobalTag.Canonical "017", "+15")

[<Test>]
let ``Test Canonical Integers - Hexadecimal``() =
    Assert.AreEqual(IntegerGlobalTag.Canonical "0x12", "+18")
[<Test>]
let ``Test Canonical Integers - Sexagesimal``() =
    Assert.AreEqual(IntegerGlobalTag.Canonical "190:20:30", "+685230")


[<Test>]
let ``Test Canonical Floats - Simple``() =
    Assert.AreEqual(FloatGlobalTag.Canonical "81.23", "+0.8123e+002")
    Assert.AreEqual(float(FloatGlobalTag.Canonical "81.23"), float("81.23"))

[<Test>]
let ``Test Canonical Floats - Shifted decimal``() =
    Assert.AreEqual(FloatGlobalTag.Canonical "0.008123", "+0.8123e-002")
    Assert.AreEqual(float(FloatGlobalTag.Canonical "0.008123"), float("0.008123"))

[<Test>]
let ``Test Canonical Floats - Shifted normalization``() =
    Assert.AreEqual(FloatGlobalTag.Canonical "1.008123", "+0.1008123e+001")
    Assert.AreEqual(float(FloatGlobalTag.Canonical "1.008123"), float("1.008123"))

    Assert.AreEqual(FloatGlobalTag.Canonical "0.8123", "0.8123")
    Assert.AreEqual(float(FloatGlobalTag.Canonical "0.8123"), float("+0.8123e+000"))

