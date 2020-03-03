#I __SOURCE_DIRECTORY__ 

#time

#r @"bin/Debug/net45/Legivel.Parser.dll"
#r @"bin/Debug/net45/Test.Legivel.ThompsonParser.dll"
#r @"bin/Debug/net45/NLog.dll"

open Legivel.Tokenizer
open Legivel.Utilities.RegexDSL
open Legivel.ThompsonParser
open NLog

#load "nlog.fsx"

NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let logger = LogManager.GetLogger("*")


let nfa = 
    rgxToNFA <| 
            RGP("AB", [Token.``c-printable``]) + GRP(RGP("CD", [Token.``c-printable``])) + RGP("AB", [Token.``c-printable``])


PrintIt nfa





