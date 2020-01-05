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


//////c	Flow-out	Legivel.Parser.Context
//////m	1	int
//////n	1	int
//////t	Clip	Legivel.Parser.Chomping

//let nfa = ``s-separate`` |> rgxToNFA

//let yaml = "
//key:    # Comment
//        # lines
//  value

//"



//let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
//stream.Position <- 5

//PrintIt nfa

//parseIt nfa stream


let parse pos yaml rgx =
    let nfa = rgxToNFA rgx
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
    stream.Position <- pos
    parseIt nfa stream
    


(HardValues.``s-l-comments`` + (``s-flow-line-prefix``)) ||| HardValues.``s-separate-in-line``
|>  rgxToNFA
|>  PrintIt


(HardValues.``s-b-comment`` + ``s-flow-line-prefix``) (*||| HardValues.``s-separate-in-line``*)
|>  rgxToNFA
|>  PrintIt



//parse 1 "   # abc  " ((HardValues.``s-b-comment`` ) ||| HardValues.``s-separate-in-line``)

//parse 1 "   # abc  " ((OPT(HardValues.``s-separate-in-line`` + OPT(HardValues.``c-nb-comment-text``)) + HardValues.``b-comment`` ) ||| ((*OOM(HardValues.``s-white``) ||| *)HardValues.``start-of-line``))

//parse 1 "   # abc  B" ((OPT(HardValues.``s-separate-in-line`` + OPT(HardValues.``c-nb-comment-text``)) + RGP("B", [Token.``nb-json``])) ||| (OOM(HardValues.``s-white``) + RGP("A", [Token.``nb-json``])))

//parse 1 "   # and B" ((OPT(HardValues.``s-separate-in-line`` + OPT(HardValues.``c-nb-comment-text``)) + RGP("B", [Token.``nb-json``])) ||| (OOM(HardValues.``s-white``) + RGP("A", [Token.``nb-json``])))


////  werkt goed:
//parse 1 "   # abc  " ((OPT(HardValues.``s-separate-in-line`` + OPT(HardValues.``c-nb-comment-text``)) + RGO("\\z", [Token.EOF]) ) ||| ((*OOM(HardValues.``s-white``) ||| *)HardValues.``start-of-line``))

////  werkt niet goed:
//parse 1 "   # abc  " ((OPT(HardValues.``s-separate-in-line`` + OPT(HardValues.``c-nb-comment-text``)) + RGO("\\z", [Token.EOF]) ) ||| (OOM(HardValues.``s-white``) ||| HardValues.``start-of-line``))




////  werkt goed:
//parse 1 "   #" ((OPT(HardValues.``s-separate-in-line`` + OPT(RGP("#", [Token.``t-hash``]))) + RGO("\\z", [Token.EOF]) ) (*||| ((*OOM(HardValues.``s-white``) ||| *)HardValues.``start-of-line``)*))

////  werkt niet goed:
//parse 1 "   #" ((OPT(HardValues.``s-separate-in-line`` + OPT(RGP("#", [Token.``t-hash``]))) + RGO("\\z", [Token.EOF]) ) ||| (OOM(HardValues.``s-white``) (*||| HardValues.``start-of-line``*)))



//  werkt goed:
parse 1 "   #" (OPT(OOM(HardValues.``s-white``) + OPT(RGP("#", [Token.``t-hash``]))) + RGO("\\z", [Token.EOF]))

//  werkt niet goed:
parse 1 "   #" ((OPT(OOM(HardValues.``s-white``) + OPT(RGP("#", [Token.``t-hash``]))) + RGO("\\z", [Token.EOF]) ) ||| (OOM(HardValues.``s-white``))) 




//  werkt goed:
parse 1 "   #A" (OPT(OOM(HardValues.``s-white``) + OPT(RGP("#", [Token.``t-hash``]))) + RGP("A", [Token.``nb-json``]))

//  werkt niet goed:
parse 1 "   #A" ((OPT(OOM(HardValues.``s-white``) + OPT(RGP("#", [Token.``t-hash``]))) + RGP("A", [Token.``nb-json``])) ||| (OOM(HardValues.``s-white``))) 





//  werkt goed:
(OPT(OOM(HardValues.``s-white``) + OPT(RGP("#", [Token.``t-hash``]))) + RGP("A", [Token.``nb-json``]))
|>  rgxToNFA
|>  PrintIt


//  werkt niet goed:
((OPT(OOM(HardValues.``s-white``) + OPT(RGP("#", [Token.``t-hash``]))) + RGP("A", [Token.``nb-json``])) ||| (OOM(HardValues.``s-white``))) 
|>  rgxToNFA
|>  PrintIt




