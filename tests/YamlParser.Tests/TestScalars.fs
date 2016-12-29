module TestScalars

open YamlParse
open NUnit.Framework

[<Test>]
let ``Test Canonical Integers - Simple``() =
    Assert.AreEqual("+5", IntegerGlobalTag.Canonical "5")

[<Test>]
let ``Test Canonical Integers - Binary``() =
    Assert.AreEqual("+5", IntegerGlobalTag.Canonical "0b101")

[<Test>]
let ``Test Canonical Integers - Octal``() =
    Assert.AreEqual("+15", IntegerGlobalTag.Canonical "017")

[<Test>]
let ``Test Canonical Integers - Hexadecimal``() =
    Assert.AreEqual("+18", IntegerGlobalTag.Canonical "0x12")
[<Test>]
let ``Test Canonical Integers - Sexagesimal``() =
    Assert.AreEqual("+685230", IntegerGlobalTag.Canonical "190:20:30")

[<Test>]
let ``Test Canonical Floats - Simple``() =
    Assert.AreEqual("+0.8123e+002", FloatGlobalTag.Canonical "81.23")
    Assert.AreEqual(float("81.23"), float(FloatGlobalTag.Canonical "81.23"))

[<Test>]
let ``Test Canonical Floats - Shifted decimal``() =
    Assert.AreEqual("+0.8123e-002", FloatGlobalTag.Canonical "0.008123")
    Assert.AreEqual(float("0.008123"), float(FloatGlobalTag.Canonical "0.008123"))

[<Test>]
let ``Test Canonical Floats - Normalized decimal``() =
    Assert.AreEqual("+0.1008123e+001", FloatGlobalTag.Canonical "1.008123")
    Assert.AreEqual(float("1.008123"), float(FloatGlobalTag.Canonical "1.008123"))

    Assert.AreEqual("+0.8123e+000", FloatGlobalTag.Canonical "0.8123")
    Assert.AreEqual(float("+0.8123e+000"), float(FloatGlobalTag.Canonical "0.8123"))

[<Test>]
let ``Test Canonical Floats - Sexagesimal``() =
    Assert.AreEqual("+0.68523015e+006", FloatGlobalTag.Canonical "190:20:30.15")
    Assert.AreEqual(float("685230.15"), float(FloatGlobalTag.Canonical "190:20:30.15"))

[<Test>]
let ``Test Canonical Floats - Specials``() =
    Assert.AreEqual("+.inf",  FloatGlobalTag.Canonical ".inf")
    Assert.AreEqual("+.inf",  FloatGlobalTag.Canonical "+.inf")
    Assert.AreEqual("-.inf",  FloatGlobalTag.Canonical "-.inf")
    Assert.AreEqual(".nan",  FloatGlobalTag.Canonical ".nan")




