#I __SOURCE_DIRECTORY__ 

#time

//#r @"bin/Debug/net45/FSharp.Core.dll"
//#r @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
#r @"bin/Debug/net45/Legivel.Parser.dll"
//#r @"bin/Debug/net45/NLog.dll"
//#r @"bin/Debug/net45/nunit.framework.dll"

open Legivel.Tokenizer
open Legivel.Utilities.RegexDSL


//  63
let ``s-indent(n)`` = Repeat(RGP (HardValues.``s-space``, [Token.``t-space``]), 2)

//  64
let ``s-indent(<n)`` = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 3)


//   69
let ``s-flow-line-prefix`` = (``s-indent(n)``) + OPT(HardValues.``s-separate-in-line``)

//67
let ``s-line-prefix``= ``s-flow-line-prefix``

//  70
let ``l-empty`` = ((``s-line-prefix``) ||| (``s-indent(<n)``)) + HardValues.``b-as-line-feed``

//  71
let ``b-l-trimmed`` = HardValues.``b-non-content`` + OOM(``l-empty``)

//  73
let ``b-l-folded`` = (``b-l-trimmed``) ||| HardValues.``b-as-space``


//  74
let ``s-flow-folded``  =
    OPT(HardValues.``s-separate-in-line``) + (``b-l-folded``) + (``s-flow-line-prefix``)

//  [127]
let ``ns-plain-safe`` = HardValues.``ns-plain-safe-out``

//  [126] 
let ``ns-plain-first`` = (HardValues.``ns-char`` - HardValues.``c-indicator``) ||| (HardValues.``c-mapping-key`` ||| HardValues.``c-mapping-value`` ||| HardValues.``c-sequence-entry``) + (``ns-plain-safe``)

//  130
let ``ns-plain-char`` = (HardValues.``ns-char`` + HardValues.``c-comment``) ||| ((``ns-plain-safe``) - (RGO (":#", [Token.``t-colon``; Token.``t-hash``]))) ||| (HardValues.``c-mapping-value`` + (``ns-plain-safe``))

//  132
let ``nb-ns-plain-in-line`` = ZOM(ZOM(HardValues.``s-white``) + (``ns-plain-char``))

//  [133]
let ``ns-plain-one-line`` = (``ns-plain-first``) + (``nb-ns-plain-in-line``)

//  134
let ``s-ns-plain-next-line`` = (``s-flow-folded``) + (``ns-plain-char``) + (``nb-ns-plain-in-line``)



``ns-plain-one-line`` + 
OOM(``s-ns-plain-next-line``)


let nfa = 
    rgxToNFA <|
        OOM((OOM(``s-indent(n)``) + OPT(HardValues.``s-separate-in-line``) (*+ ``s-flow-line-prefix``*)) + (HardValues.``ns-char`` ||| ((``ns-plain-safe``) - (RGO (":#", [Token.``t-colon``; Token.``t-hash``])))) )

PrintIt nfa





(*

(this.``ns-plain-one-line`` ps) + OOM(this.``s-ns-plain-next-line`` ps)


ps.c = Block-Key
ps.m = 0
ps.n = 2


*)

``s-indent(<n)``
|>  rgxToNFA
|>  PrintIt 


