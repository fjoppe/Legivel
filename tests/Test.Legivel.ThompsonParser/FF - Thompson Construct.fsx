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


let ``s-indent(n)`` = Repeat(RGP (HardValues.``s-space``, [Token.``t-space``]), 0)
//let ``s-indent(<n)``= Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 1) 
//let ``s-flow-line-prefix`` = (``s-indent(n)``) + OPT(HardValues.``s-separate-in-line``)
//let ``s-line-prefix`` = ``s-flow-line-prefix``
//let ``l-empty`` = ((``s-line-prefix``) ||| (``s-indent(<n)``)) + HardValues.``b-as-line-feed``
//let ``b-l-trimmed`` = HardValues.``b-non-content`` + OOM(``l-empty``)
//let ``b-l-folded`` = ``b-l-trimmed`` ||| HardValues.``b-as-space``
//let ``s-flow-folded`` = OPT(HardValues.``s-separate-in-line``) + ``b-l-folded`` + ``s-flow-line-prefix``
//let ``ns-plain-safe-out`` = HardValues.``ns-plain-safe-out``
//let ``ns-plain-safe`` = ``ns-plain-safe-out``
//let ``ns-plain-char`` = (HardValues.``ns-char`` + HardValues.``c-comment``) ||| ((``ns-plain-safe``) - (RGO (":#", [Token.``t-colon``; Token.``t-hash``]))) ||| (HardValues.``c-mapping-value`` + ``ns-plain-safe``)
//let ``nb-ns-plain-in-line`` = ZOM(ZOM(HardValues.``s-white``) + ``ns-plain-char``)
//let ``s-ns-plain-next-line`` = ``s-flow-folded`` + ``ns-plain-char`` + ``nb-ns-plain-in-line``
//let ``ns-plain-first`` = (HardValues.``ns-char`` - HardValues.``c-indicator``) ||| (HardValues.``c-mapping-key`` ||| HardValues.``c-mapping-value`` ||| HardValues.``c-sequence-entry``) + ``ns-plain-safe``
//let ``ns-plain-one-line`` = ``ns-plain-first`` + ``nb-ns-plain-in-line``
//let ``ns-plain-multi-line`` = (``ns-plain-one-line``) + ZOM(``s-ns-plain-next-line``)


//let ``s-double-escaped`` = ZOM(HardValues.``s-white``) + HardValues.``c-escape`` + HardValues.``b-non-content`` + ZOM(``l-empty``) + ``s-flow-line-prefix``
//let ``s-double-break`` = ``s-double-escaped`` ||| ``s-flow-folded``

////c	Flow-out	Legivel.Parser.Context
////m	1	int
////n	1	int
////t	Clip	Legivel.Parser.Chomping


//let ``illegal-chars`` = HardValues.``c-double-quote`` + OOM((HardValues.``nb-json`` - HardValues.``c-double-quote``) ||| ``s-double-break``) + HardValues.``c-double-quote``

//let nfa = rgxToNFA <| ``illegal-chars`` 

//let yaml = "
//Bad escapes:
//  \"\\c
//  \\xq-\"
//"



//let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
//stream.Position <- 16

//PrintIt nfa


//parseIt nfa stream


//(HardValues.``nb-json`` - HardValues.``c-double-quote``) ||| (HardValues.``b-break`` ||| HardValues.``b-break``)
//|>  rgxToNFA
//|>  PrintIt

//(HardValues.``nb-json`` - HardValues.``c-double-quote``) ||| HardValues.``b-break`` ||| HardValues.``b-break``
//|>  rgxToNFA
//|>  PrintIt


``s-indent(n)`` 
|>  rgxToNFA
|>  PrintIt



OPT(RGP("\t", [Token.``t-tab``]) + RGP("D", [Token.``c-printable``])) + 
(RGO("\t\n", [Token.``t-tab``; Token.NewLine]) + RGP("A", [Token.``c-printable``]))
|>  rgxToNFA
|>  PrintIt
