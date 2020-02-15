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


let assertPartialMatch nfa yaml strmatched =
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "\x00")
    parseIt nfa stream
    
    //r |> ParseResult.IsMatch   |> shouldEqual true
    //r |> ParseResult.FullMatch |> clts |> shouldEqual strmatched

    //let rest = yaml.Substring(strmatched.Length)
    //let c = stream.Get()
    //c.Source.[0] |> shouldEqual rest.[0]


let nfa = 
    rgxToNFA <| 
            ZOM(RGP("ABC", [Token.``c-printable``]))



assertPartialMatch nfa "ABCABCABD" "ABCABC"
assertPartialMatch nfa "ABCAB" "ABC"




