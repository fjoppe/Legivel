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
let ``s-indent(<n)``= Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 0) 
let ``s-flow-line-prefix`` = (``s-indent(n)``) + OPT(HardValues.``s-separate-in-line``)
let ``s-line-prefix`` = ``s-flow-line-prefix``
let ``l-empty`` = ((``s-line-prefix``) ||| (``s-indent(<n)``)) + HardValues.``b-as-line-feed``
let ``b-l-trimmed`` = HardValues.``b-non-content`` + OOM(``l-empty``)
let ``b-l-folded`` = ``b-l-trimmed`` ||| HardValues.``b-as-space``
let ``s-flow-folded`` = OPT(HardValues.``s-separate-in-line``) + ``b-l-folded`` + ``s-flow-line-prefix``
let ``ns-plain-safe-out`` = HardValues.``ns-plain-safe-out``
let ``ns-plain-safe`` = ``ns-plain-safe-out``
let ``ns-plain-char`` = (HardValues.``ns-char`` + HardValues.``c-comment``) ||| ((``ns-plain-safe``) - (RGO (":#", [Token.``t-colon``; Token.``t-hash``]))) ||| (HardValues.``c-mapping-value`` + ``ns-plain-safe``)
let ``nb-ns-plain-in-line`` = ZOM(ZOM(HardValues.``s-white``) + ``ns-plain-char``)
let ``s-ns-plain-next-line`` = ``s-flow-folded`` + ``ns-plain-char`` + ``nb-ns-plain-in-line``
let ``ns-plain-first`` = (HardValues.``ns-char`` - HardValues.``c-indicator``) ||| (HardValues.``c-mapping-key`` ||| HardValues.``c-mapping-value`` ||| HardValues.``c-sequence-entry``) + ``ns-plain-safe``
let ``ns-plain-one-line`` = ``ns-plain-first`` + ``nb-ns-plain-in-line``
let ``ns-plain-multi-line`` = (``ns-plain-one-line``) + ZOM(``s-ns-plain-next-line``)



//c	Flow-out	Legivel.Parser.Context
//m	1	int
//n	1	int
//t	Clip	Legivel.Parser.Chomping


let ``ns-plain`` = ``ns-plain-multi-line``

let nfa = rgxToNFA <| ``ns-plain``

let y = "
- Me
- Sammy Sosa
- Ken Griffey"


let yaml = y.Substring(3)

let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")

PrintIt nfa


parseIt nfa stream

