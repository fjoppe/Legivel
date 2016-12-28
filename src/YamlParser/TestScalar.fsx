open System
open System.Text
open System.IO
open System.Text.RegularExpressions

#I "."
#r @"bin\Debug\YamlParser.dll"

open YamlParse

IntegerGlobalTag.Canonical "5"
IntegerGlobalTag.Canonical "0b101"
IntegerGlobalTag.Canonical "017"
IntegerGlobalTag.Canonical "0x12"
IntegerGlobalTag.Canonical "190:20:30"

FloatGlobalTag.Canonical "81.23"
float("+0.8123e+002") = float("81.23")
FloatGlobalTag.Canonical "0.008123"
float("+0.8123e-002") = float("0.008123")
FloatGlobalTag.Canonical "1.008123"
float("+0.1008123e+001") = float("1.008123")
FloatGlobalTag.Canonical "0.8123"
float("0.8123") = float("+0.8123e+000")

//FloatGlobalTag.Canonical "0o7"
//Regex.Matches("0o7", FloatGlobalTag.Regex)

FloatGlobalTag.Canonical "190:20:30.15"
float("+0.68523015e+006") = float("685230.15")

//    FloatGlobalTag.Canonical ".inf"
//    FloatGlobalTag.Canonical "-.inf"
//    FloatGlobalTag.Canonical ".nan"

//    Core Schema:  http://www.yaml.org/spec/1.2/spec.html#id2804923
