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


let ``start-of-line`` = RGP ("^", [Token.NoToken])


//  literal-content internal	 loc:(4,1) i:2 c:Block-in &a:0 e:0 w:0 sp:19

let ``s-indent(n)`` = Repeat(RGP (HardValues.``s-space``, [Token.``t-space``]), 2)
let ``s-indent(<n)`` = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, (1)) (* Where m < n *)
let ``s-indent(<=n)`` = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 2)  (* Where m ≤ n *)

let ``s-block-line-prefix`` = ``s-indent(n)`` 
let ``s-line-prefix`` = ``s-block-line-prefix``

let ``l-empty Block-in`` = ((``s-line-prefix``) ||| (``s-indent(<n)``)) + HardValues.``b-as-line-feed``

let ``l-nb-literal-text`` = ZOM(``l-empty Block-in``) + (``s-indent(n)``) + OOM(HardValues.``nb-char``)

let ``b-nb-literal-next`` = HardValues.``b-as-line-feed`` + (``l-nb-literal-text``)

let ``b-chomped-last`` = HardValues.``b-as-line-feed``   ||| RGO("\\z", [Token.EOF])
let ``l-trail-comments`` = (``s-indent(<n)``) + HardValues.``c-nb-comment-text`` + HardValues.``b-comment`` + ZOM(HardValues.``l-comment``)


let ``l-strip-empty`` = ZOM((``s-indent(<=n)``) + HardValues.``b-non-content``) + OPT(``l-trail-comments``)

let ``l-chomped-empty`` = ``l-strip-empty``

let ``l-literal-content`` = 
    GRP(OPT(``l-nb-literal-text`` + ZOM(``b-nb-literal-next``) + ``b-chomped-last``) + ``l-chomped-empty``)


//let nfa =
//    ``l-literal-content``
//    |>  rgxToNFA

//nfa    |>  PrintIt


//let yml = "
//# ASCII Art
//--- |
//  \//||\/||
//  // ||  ||__"


//let stream = RollingStream<_>.Create (tokenProcessor yml) (TokenData.Create (Token.EOF) "")
//stream.Position <- 19

//let r = parseIt nfa stream


``l-nb-literal-text``
|>  rgxToNFA
|>  PrintIt



