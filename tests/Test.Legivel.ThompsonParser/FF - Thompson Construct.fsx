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
open System.Text.RegularExpressions
open Legivel.Utilities.RegexDSL
open Legivel.ThompsonParser
open NFAValues




//HardValues.``s-l-comments``
//|>  rgxToNFA
//|>  PrintIt 

//ZOM(HardValues.``l-comment``)
//|>  rgxToNFA
//|>  PrintIt 

//HardValues.``s-separate-in-line`` + OPT(HardValues.``c-nb-comment-text``) + HardValues.``b-comment``
//|>  rgxToNFA
//|>  PrintIt 


//ZOM (HardValues.``l-document-prefix``)
//|>  rgxToNFA
//|>  PrintIt

//HardValues.``c-byte-order-mark``
//|>  rgxToNFA
//|>  PrintIt


//OPT(RGP("A", [Token.``c-printable``])) + ZOM(RGP("B", [Token.``c-printable``]))
//|>  rgxToNFA
//|>  PrintIt


//ZOM (OPT(RGP("A", [Token.``c-printable``])) + ZOM(RGP("B", [Token.``c-printable``])))
//|>  rgxToNFA
//|>  PrintIt



//let yaml = "
//- Mark McGwire
//- Sammy Sosa
//- Ken Griffey"
//let rgx = (ZOM(HardValues.``l-document-prefix``)) |> rgxToNFA
//let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
//parseIt rgx stream 



//HardValues.``ns-yaml-directive`` + HardValues.``s-l-comments`` 
//|>  rgxToNFA
//|>  PrintIt


let ``ns-plain-safe-out`` = HardValues.``ns-plain-safe-out``
let ``ns-plain-safe`` = ``ns-plain-safe-out``
let ``ns-plain-char`` = (HardValues.``ns-char`` + HardValues.``c-comment``) ||| ((``ns-plain-safe``) - (RGO (":#", [Token.``t-colon``; Token.``t-hash``]))) ||| (HardValues.``c-mapping-value`` + (``ns-plain-safe``))
let ``ns-plain-first`` = (HardValues.``ns-char`` - HardValues.``c-indicator``) ||| (HardValues.``c-mapping-key`` ||| HardValues.``c-mapping-value`` ||| HardValues.``c-sequence-entry``) + (``ns-plain-safe``)
let ``nb-ns-plain-in-line`` = ZOM(ZOM(HardValues.``s-white``) + (``ns-plain-char``))
let ``ns-plain-one-line`` = (``ns-plain-first``) + (``nb-ns-plain-in-line``)
let ``s-indent(n)`` = Repeat(RGP (HardValues.``s-space``, [Token.``t-space``]), 2)
let ``s-indent(<n)`` = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 1) (* Where m < n *)
let ``s-flow-line-prefix`` = (``s-indent(n)``) + OPT(HardValues.``s-separate-in-line``)
let ``s-line-prefix`` = ``s-flow-line-prefix``
let ``l-empty``  = ((``s-line-prefix``) ||| (``s-indent(<n)``)) + HardValues.``b-as-line-feed``
let ``b-l-trimmed`` = HardValues.``b-non-content`` + OOM(``l-empty``)
let ``b-l-folded`` = (``b-l-trimmed``) ||| HardValues.``b-as-space``
let ``s-flow-folded`` =
        OPT(HardValues.``s-separate-in-line``) + (``b-l-folded``) + (``s-flow-line-prefix``)

let ``s-ns-plain-next-line`` = (``s-flow-folded``) + (``ns-plain-char``) + (``nb-ns-plain-in-line``)

//(``ns-plain-one-line``) + OOM(``s-ns-plain-next-line``)
//|>  rgxToNFA



//(``ns-plain-one-line``) 
//|>  rgxToNFA


OOM((``s-flow-folded``) + (``ns-plain-char``) + (``nb-ns-plain-in-line``))
|>  rgxToNFA


