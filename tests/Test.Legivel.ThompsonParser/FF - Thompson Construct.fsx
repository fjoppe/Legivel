open System.Threading.Tasks
open Microsoft.FSharp.Control
open System.Threading

#I __SOURCE_DIRECTORY__ 

#time

#r @"bin/Debug/net45/Legivel.Parser.dll"
#r @"bin/Debug/net45/Test.Legivel.ThompsonParser.dll"
#r @"bin/Debug/net45/NLog.dll"

open System
open Legivel.Tokenizer
open Legivel.Utilities.RegexDSL
open Legivel.ThompsonParser
open NLog

#load "nlog.fsx"

NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let logger = LogManager.GetLogger("*")

let parseStream rx =
    let nfa = 
        rgxToNFA <| rx

    let yaml = "\n \n\n"

    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "\x00")

    let r = parseIt nfa stream
    r

//  c	Block-in	Legivel.Parser.Context
//  m	0	int
//  n	1	int
//  t	Clip	Legivel.Parser.Chomping


let ``s-indent(<n)`` = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 0) (* Where m < n *)
let ``s-indent(n)`` = Repeat(RGP (HardValues.``s-space``, [Token.``t-space``]), 1)
let ``s-indent(<=n)`` = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 1)  (* Where m ≤ n *)


let ``s-block-line-prefix`` = ``s-indent(n)``
let ``s-line-prefix Block-in`` = ``s-block-line-prefix``

let ``l-empty Block-in`` = (``s-line-prefix Block-in`` ||| (``s-indent(<n)``)) + HardValues.``b-as-line-feed``

let ``s-nb-folded-text`` = (``s-indent(n)``) + ZOM(HardValues.``nb-char``)

let ``b-l-trimmed`` = HardValues.``b-non-content`` + OOM(``l-empty Block-in``)
let ``b-l-folded Block-in`` = (``b-l-trimmed``) ||| HardValues.``b-as-space``

let ``s-nb-spaced-text`` = (``s-indent(n)``) + HardValues.``s-white`` + ZOM(HardValues.``nb-char``)

let ``b-l-spaced`` = HardValues.``b-as-line-feed`` + ZOM(``l-empty Block-in``)

let ``l-nb-spaced-lines`` = (``s-nb-spaced-text``) + ZOM((``b-l-spaced``) + (``s-nb-spaced-text``))
let ``l-nb-folded-lines`` = (``s-nb-folded-text``) + ZOM((``b-l-folded Block-in``) + ``s-nb-folded-text``)

let ``l-nb-same-lines`` = ZOM(``l-empty Block-in``) + ((``l-nb-folded-lines``) ||| (``l-nb-spaced-lines``))


//let yaml = " fo\n t\n\n"
//val r : ParseResult = { IsMatch = true
//FullMatch = [' '; 'f'; 'o'; '\010'; ' '; 't'; '\010']
//Groups = [[' '; 'f'; 'o'; '\010'; 't']] }


//let ``folded-content`` = ``l-folded-content``


//let ``l-folded-content`` = 
//GRP(OPT(
//  ZOM(HardValues.``b-as-line-feed`` + ``s-indent(n)``) 
//+ HardValues.``b-as-line-feed``))
//+ ZOM((``s-indent(<=n)``) + HardValues.``b-non-content``) 
//|>  parseStream




GRP(
    OPT(
        ZOM(HardValues.``b-as-line-feed`` + ``s-indent(n)``)
    )
)
+ ZOM((``s-indent(<=n)``) + HardValues.``b-non-content``) 
|>  parseStream

//val r : ParseResult = { IsMatch = true
//FullMatch = ['\010'; ' '; '\010'; '\010']
//Groups = [['\010'; '\010']] }



GRP(
    OPT(
        ZOM(HardValues.``b-as-line-feed`` + ``s-indent(n)``)
    )
)
+ ZOM((``s-indent(<=n)``) + HardValues.``b-non-content``)
|>  rgxToNFA
|>  PrintIt

