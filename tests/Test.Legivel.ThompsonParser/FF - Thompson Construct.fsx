#I __SOURCE_DIRECTORY__ 

#time

//#r @"bin/Debug/net45/FSharp.Core.dll"
//#r @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
#r @"bin/Debug/net45/Legivel.Parser.dll"
//#r @"bin/Debug/net45/NLog.dll"
//#r @"bin/Debug/net45/nunit.framework.dll"
#r @"bin/Debug/net45/Test.Legivel.ThompsonParser.dll"
#r @"bin/Debug/net45/NLog.dll"

open Legivel.Tokenizer
open System.Drawing
open System.Diagnostics
open System.Text.RegularExpressions
open Legivel.Utilities.RegexDSL
open Legivel.ThompsonParser
open NFAValues
open NLog

#load "nlog.fsx"

open System
open System.Globalization

NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let logger = LogManager.GetLogger("*")

//let ``s-indent(n)`` = Repeat(RGP (HardValues.``s-space``, [Token.``t-space``]), 2)
//let ``s-indent(<n)``  = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 1) (* Where m < n *)

//let ``ns-plain-char`` = RGP("B", [Token.``nb-json``]) 

//let ``ns-plain-one-line`` = RGP("A", [Token.``nb-json``]) 

//let ``s-flow-line-prefix`` = ``s-indent(n)``

//let ``s-line-prefix`` = ``s-flow-line-prefix``

//let ``l-empty`` = (``s-line-prefix`` ||| ``s-indent(<n)``) + HardValues.``b-line-feed``

//let ``b-l-trimmed`` = HardValues.``b-line-feed`` + OOM(``l-empty``)

//let ``b-l-folded`` = ``b-l-trimmed`` ||| HardValues.``b-line-feed``
////let ``b-l-folded`` = (*(``b-l-trimmed``) |||*) HardValues.``b-line-feed``

//let ``s-flow-folded`` = ``b-l-folded`` +  ``s-flow-line-prefix``

//let ``s-ns-plain-next-line`` = ``s-flow-folded`` + ``ns-plain-char`` 


////``b-l-folded`` + RGP("A", [Token.``nb-json``]) 
////|>  rgxToNFA
////|>  PrintIt


//``b-l-trimmed`` ||| HardValues.``b-line-feed``
//|>  rgxToNFA
//|>  PrintIt


//(HardValues.``b-line-feed`` + RGP("A", [Token.``nb-json``])) ||| HardValues.``b-line-feed``
//|>  rgxToNFA
//|>  PrintIt


//RGP("AB", [Token.``nb-json``]) ||| RGP("A", [Token.``nb-json``])
//|>  rgxToNFA
//|>  PrintIt



////``l-empty``
////|>  rgxToNFA
////|>  PrintIt

////(``s-line-prefix`` ||| ``s-indent(<n)``)
////|>  rgxToNFA
////|>  PrintIt



////``s-line-prefix`` 
////|>  rgxToNFA
////|>  PrintIt

////``s-indent(<n)``
////|>  rgxToNFA
////|>  PrintIt



////  c	Block-key	Legivel.Parser.Context
////  m	0	int
////  n	2	int
////  t	Clip	Legivel.Parser.Chomping

//let ``illegl multiline`` = ``s-ns-plain-next-line``


//let yaml = "\n  B"


//let nfa = ``illegl multiline`` |> rgxToNFA

let nfa = (ZOM(HardValues.``l-document-prefix``)) |> rgxToNFA

//let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")


//parseIt nfa stream
HardValues.``l-document-prefix``
//(ZOM(HardValues.``l-document-prefix``)) 
//|>  rgxToNFA
//|>  PrintIt 


ZOM(OPT(RGP("C", [Token.``nb-json``])))
|>  rgxToNFA
|>  PrintIt 


OPT(RGP("C", [Token.``nb-json``]))
|>  rgxToNFA
|>  PrintIt 
