
#I "."
#r @"bin\Debug\Legivel.Parser.dll"

open Legivel.TagResolution.JSON

//    Core Schema:  http://www.yaml.org/spec/1.2/spec.html#id2804923

IntegerGlobalTag.ToCanonical "5"
IntegerGlobalTag.ToCanonical "0b101"
IntegerGlobalTag.ToCanonical "017"
IntegerGlobalTag.ToCanonical "0x12"
IntegerGlobalTag.ToCanonical "190:20:30"

FloatGlobalTag.ToCanonical "81.23"
float("+0.8123e+002") = float("81.23")
FloatGlobalTag.ToCanonical "0.008123"
float("+0.8123e-002") = float("0.008123")
FloatGlobalTag.ToCanonical "1.008123"
float("+0.1008123e+001") = float("1.008123")
FloatGlobalTag.ToCanonical "0.8123"
float("0.8123") = float("+0.8123e+000")

//  Should not match
//FloatGlobalTag.Canonical "0o7"
//Regex.Matches("0o7", FloatGlobalTag.Regex)

FloatGlobalTag.ToCanonical "190:20:30.15"
float("+0.68523015e+006") = float("685230.15")

FloatGlobalTag.ToCanonical ".inf"
FloatGlobalTag.ToCanonical "-.inf"
FloatGlobalTag.ToCanonical ".nan"

let digitToValue c = if c >= 'A' then 10+(int c)-(int 'A') else (int c)-(int '0')

digitToValue '3'
digitToValue 'A'

