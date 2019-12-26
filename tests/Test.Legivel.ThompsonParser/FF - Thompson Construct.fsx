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


let ``b-non-content`` = HardValues.``b-non-content``
let ``s-space`` = HardValues.``s-space``

let ``s-indent(n)`` = Repeat(RGP (``s-space``, [Token.``t-space``]), 1)
let ``s-indent(<n)`` = Range(RGP (``s-space``, [Token.``t-space``]), 0, 0) 
let ``s-indent(<=n)`` = Range(RGP (``s-space``, [Token.``t-space``]), 0, 1)

let ``s-block-line-prefix`` = ``s-indent(n)``
let ``b-as-line-feed`` = HardValues.``b-as-line-feed``
let ``s-line-prefix`` = ``s-block-line-prefix``
let ``l-empty`` = ((``s-line-prefix``) ||| (``s-indent(<n)``)) + ``b-as-line-feed``
let ``nb-char``  = HardValues.``nb-char``

let ``l-nb-literal-text`` = ZOM(``l-empty``) + (``s-indent(n)``) + OOM(``nb-char``)

let ``b-nb-literal-next`` = ``b-as-line-feed`` + (``l-nb-literal-text``)
let ``c-nb-comment-text`` = HardValues.``c-nb-comment-text``

let ``b-comment`` = HardValues.``b-comment``
let ``l-comment`` = HardValues.``l-comment``

let ``l-trail-comments`` = (``s-indent(<n)``) + ``c-nb-comment-text`` + ``b-comment`` + ZOM(``l-comment``)

let ``l-strip-empty`` = ZOM((``s-indent(<=n)``) + ``b-non-content``) + OPT(``l-trail-comments``)


let ``b-chomped-last`` = ``b-as-line-feed``   ||| RGP("\\z", [Token.EOF])
let ``l-chomped-empty`` = ``l-strip-empty``



//c	Block-in	Legivel.Parser.Context
//m	0	int
//n	1	int
//t	Clip	Legivel.Parser.Chomping

//  [173]   http://www.yaml.org/spec/1.2/spec.html#l-literal-content(n,t)
let ``l-literal-content`` = 
    GRP(OPT(``l-nb-literal-text`` + ZOM(``b-nb-literal-next``) + (``b-chomped-last``)) + (``l-chomped-empty``))



//``l-literal-content``
//|>  rgxToNFA
//|>  PrintIt

//OPT((``l-nb-literal-text``) + ZOM(``b-nb-literal-next``) + (``b-chomped-last``)) + (``l-chomped-empty``)
//|>  rgxToNFA
//|>  PrintIt



//OPT(``l-nb-literal-text`` + ZOM(``b-nb-literal-next``) + (``b-chomped-last``))
//|>  rgxToNFA
//|>  PrintIt



//``l-chomped-empty``
//|>  rgxToNFA
//|>  PrintIt




//``l-literal-content``
//|>  rgxToNFA
//|>  PrintIt



//let y = "
//# ASCII Art
//--- |
//  \//||\/||
//  // ||  ||__"


let y = "
# ASCII Art
--- |
  a"


let yaml = y.Substring(19)

let nfa = ``l-literal-content`` |> rgxToNFA


//nfa |> PrintIt

let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
stream.Position <- 0
let r = parseIt nfa stream

