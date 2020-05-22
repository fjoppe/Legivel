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


let ``s-indent(n)`` = Repeat(RGP (HardValues.``s-space``, [Token.``t-space``]), 1)
let ``s-indent(<n)`` = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 0) (* Where m < n *)
let ``s-indent(<=n)`` = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 1)  (* Where m ≤ n *)


let ``l-trail-comments`` = (``s-indent(<n)``) + HardValues.``c-nb-comment-text`` + HardValues.``b-comment`` + ZOM(HardValues.``l-comment``)
let ``l-strip-empty`` = ZOM((``s-indent(<=n)``) + HardValues.``b-non-content``) + OPT(``l-trail-comments``)
let ``l-chomped-empty`` = ``l-strip-empty``
let ``b-chomped-last`` = HardValues.``b-as-line-feed``   ||| RGP("\\z", [Token.EOF])
let ``s-block-line-prefix`` = ``s-indent(n)``
let ``s-line-prefix`` = ``s-block-line-prefix``
let ``l-empty Block-in`` = ((``s-line-prefix``) ||| (``s-indent(<n)``)) + HardValues.``b-as-line-feed``
let ``b-l-trimmed`` = HardValues.``b-non-content`` + OOM(``l-empty Block-in``)
let ``s-nb-spaced-text`` = (``s-indent(n)``) + HardValues.``s-white`` + ZOM(HardValues.``nb-char``)
let ``b-l-folded Block-in`` = (``b-l-trimmed``) ||| HardValues.``b-as-space``
let ``s-nb-folded-text`` = (``s-indent(n)``) + ZOM(HardValues.``nb-char``)
let ``l-nb-folded-lines`` = (``s-nb-folded-text``) + ZOM((``b-l-folded Block-in``) + ``s-nb-folded-text``)

let ``b-l-spaced`` = HardValues.``b-as-line-feed`` + ZOM(``l-empty Block-in``)
let ``l-nb-spaced-lines`` = (``s-nb-spaced-text``) + ZOM((``b-l-spaced``) + (``s-nb-spaced-text``))

let ``l-nb-same-lines`` = 
    ZOM(``l-empty Block-in``) + ((``l-nb-folded-lines``) ||| (``l-nb-spaced-lines``))


let ``l-nb-diff-lines`` = (``l-nb-same-lines``) + ZOM(HardValues.``b-as-line-feed`` + (``l-nb-same-lines``))
let ``l-folded-content`` = GRP(OPT((``l-nb-diff-lines``) + (``b-chomped-last``))) + (``l-chomped-empty``)


let nfa = ``l-folded-content`` |> rgxToNFA


PrintIt nfa

let yaml = "
>
 Sammy Sosa completed another
 fine season with great stats.

   63 Home Runs
   0.288 Batting Average

 What a year!"

let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "\x00")
stream.Position <- 3

let r = parseIt nfa stream


