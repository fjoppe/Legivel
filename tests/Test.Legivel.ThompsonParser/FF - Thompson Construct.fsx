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

let ``s-flow-line-prefix`` = (``s-indent(n)``) + OPT(HardValues.``s-separate-in-line``)
let ``s-separate-lines`` = (HardValues.``s-l-comments`` + (``s-flow-line-prefix``)) ||| HardValues.``s-separate-in-line``
let ``s-separate`` = ``s-separate-lines``

let nfa = ``s-separate`` |> rgxToNFA

PrintIt nfa


//s-l+block-scalar	 loc:(2,5) i:0 c:Block-out &a:0 e:0 w:0 sp:5
//s-separate	 loc:(2,5) i:1 c:Block-out &a:0 e:0 w:0 sp:5
//c-ns-properties	 loc:(4,3) i:0 c:Block-out &a:0 e:0 w:0 sp:37
//s-separate	 loc:(4,3) i:0 c:Block-out &a:0 e:0 w:0 sp:37
//s-separate	 loc:(4,3) i:0 c:Block-out &a:0 e:0 w:0 sp:37
//c-l+literal	 loc:(4,3) i:0 c:Block-out &a:0 e:0 w:0 sp:37

let yaml = "
key:    # Comment
        # lines
  value

"

let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
stream.Position <- 5


let r = parseIt nfa stream

r.FullMatch





