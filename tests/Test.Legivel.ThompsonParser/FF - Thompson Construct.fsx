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


let nfa =
    GRP(HardValues.``ns-reserved-directive``) + HardValues.``s-l-comments`` 
    |>  rgxToNFA

PrintIt nfa

let yaml = "
%FOO bar baz # Should be ignored
              # with a warning.
--- \"foo\"
"

let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "\x00")
stream.Position <- 1

let r = parseIt nfa stream





HardValues.``s-b-comment``
|> rgxToNFA
|> PrintIt

