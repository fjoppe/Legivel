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


let ``s-indent(n)`` = Repeat(RGP (HardValues.``s-space``, [Token.``t-space``]), 1)
let ``s-flow-line-prefix`` = (``s-indent(n)``) + OPT(HardValues.``s-separate-in-line``)
let ``s-separate-lines`` = (HardValues.``s-l-comments`` + (``s-flow-line-prefix``)) ||| HardValues.``s-separate-in-line``
let ``s-separate`` = ``s-separate-lines``


let parse p yaml rgx =
    let nfa = rgx |> rgxToNFA
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
    stream.Position <- p
    let r = parseIt nfa stream
    (r, stream.Position)


let yaml = "\nkey:    \n        \n  value\n\n"

ZOM(OOM(HardValues.``s-white``) + HardValues.``b-comment``) + (``s-indent(n)`` + OPT(OOM(HardValues.``s-white``)))
|>  parse 5 yaml 

ZOM(OOM(HardValues.``s-white``) + HardValues.``b-comment``)
|>  parse 5 yaml 





ZOM(OOM(HardValues.``s-white``) + HardValues.``b-comment``) + (``s-indent(n)`` + OPT(OOM(HardValues.``s-white``)))
|>  rgxToNFA
|>  PrintIt


//ZOM(OOM(HardValues.``s-white``) + HardValues.``b-comment``) //+ (``s-indent(n)`` + RGP("A", [Token.``nb-json``]))
//|>  rgxToNFA
//|>  PrintIt


//ZOM(OPT(RGP("A", [Token.``nb-json``])) + ZOM(RGP("C", [Token.``nb-json``])))
//|>  rgxToNFA
//|>  PrintIt


//ZOM(OPT(RGP("A", [Token.``nb-json``])) + ZOM(RGP("C", [Token.``nb-json``])))
//|> parse 0 "CC"



