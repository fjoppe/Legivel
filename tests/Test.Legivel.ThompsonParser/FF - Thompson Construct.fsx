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


let ``s-indent(n)`` = Repeat(RGP (HardValues.``s-space``, [Token.``t-space``]), 1)
let ``s-indent(<n)`` = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 0) (* Where m < n *)

let ``s-flow-line-prefix`` = (``s-indent(n)``) + OPT(HardValues.``s-separate-in-line``)
let ``s-line-prefix Flow-in`` = ``s-flow-line-prefix``
let ``l-empty Flow-in`` = ((``s-line-prefix Flow-in``) ||| (``s-indent(<n)``)) + HardValues.``b-as-line-feed``
let ``b-l-trimmed Flow-in`` = HardValues.``b-non-content`` + OOM(``l-empty Flow-in``)
let ``b-l-folded Flow-in`` = ``b-l-trimmed Flow-in`` ||| HardValues.``b-as-space``

let ``s-flow-folded`` =
    OPT(HardValues.``s-separate-in-line``) + (``b-l-folded Flow-in``) + ``s-line-prefix Flow-in``

let ``s-single-next-line`` = 
    ZOM((``s-flow-folded``) + HardValues.``ns-single-char`` + HardValues.``nb-ns-single-in-line``) + (``s-flow-folded``) |||
    OOM((``s-flow-folded``) + HardValues.``ns-single-char`` + HardValues.``nb-ns-single-in-line``) + ZOM(HardValues.``s-white``)        

let ``nb-single-multi-line`` = HardValues.``nb-ns-single-in-line`` + ((``s-single-next-line``) ||| ZOM(HardValues.``s-white``))
let ``nb-single-text`` = ``nb-single-multi-line``
let ``c-single-quoted`` = HardValues.``c-single-quote`` + GRP(``nb-single-text``) + HardValues.``c-single-quote``

``c-single-quoted`` |> rgxToNFA


