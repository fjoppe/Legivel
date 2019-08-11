#I __SOURCE_DIRECTORY__ 

#time

//#r @"bin/Debug/net45/FSharp.Core.dll"
//#r @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
#r @"bin/Debug/net45/Legivel.Parser.dll"
#r @"bin/Debug/net45/NLog.dll"
#r @"bin/Debug/net45/nunit.framework.dll"
#r @"bin/Debug/net45/Test.Legivel.ThompsonParser.dll"

open Legivel.Tokenizer
open System.Drawing
open System.Diagnostics
open NUnit.Framework
open System.Text.RegularExpressions
open Legivel.Utilities.RegexDSL
open Legivel.ThompsonParser


let nfa = 
    rgxToNFA <| 
            OPT(
                RGP("A", [Token.``c-printable``]) |||
                RGO("\t-", [Token.``t-tab``; Token.``t-hyphen``]) 
                ) +
                (
                RGP("A", [Token.``c-printable``]) |||
                RGO("\t\n", [Token.``t-tab``; Token.NewLine])
                )


PrintIt nfa


