#I __SOURCE_DIRECTORY__ 

#time

//#r @"bin/Debug/net45/FSharp.Core.dll"
//#r @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
#r @"bin/Debug/net45/Legivel.Parser.dll"
//#r @"bin/Debug/net45/NLog.dll"
//#r @"bin/Debug/net45/nunit.framework.dll"
#r @"bin/Debug/net45/Test.Legivel.ThompsonParser.dll"

open Legivel.Tokenizer
open System.Drawing
open System.Diagnostics
//open NUnit.Framework
open System.Text.RegularExpressions
open Legivel.Utilities.RegexDSL
open Legivel.ThompsonParser

//   ``Colliding Plain/OiS in nested Repeater X-paths with one state deep``() 
let nfa = 
    rgxToNFA <| 
            ZOM(RGP("ABC", [Token.``c-printable``]))

PrintIt nfa


