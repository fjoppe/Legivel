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
//open NUnit.Framework
open System.Text.RegularExpressions
open Legivel.Utilities.RegexDSL
open Legivel.ThompsonParser

let ``start-of-line`` = RGP ("^", [Token.NoToken])
let ``c-printable`` = 
    RGO (
        "\u0009\u000a\u000d\u0020-\u007e" +   // 8 - bit, #x9 | #xA | #xD | [#x20-#x7E]
        "\u0085\u00a0-\ud7ff\ue000-\ufffd",   // 16- bit, #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD]
                                               //  32-bit -> currently not supported because .Net does not encode naturally. Yaml: [#x10000-#x10FFFF]
        [
        Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``c-printable``; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark`` 
        Token.``t-colon`` ; Token.``t-comma``; Token.``t-dot`` ; Token.``t-square-bracket-start`` ; Token.``t-square-bracket-end`` ; Token.``t-curly-bracket-start``
        Token.``t-curly-bracket-end`` ; Token.``t-hash`` ; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``
        Token.``t-gt``; Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``;Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``
        Token.``ns-dec-digit``; Token.``c-escape``
        ])

let ``s-space`` : string = "\u0020" 
let ``s-tab`` = "\u0009"    // tab
let ``s-white`` = RGO(``s-space`` + ``s-tab``, [Token.``t-space``; Token.``t-tab``])
let ``s-separate-in-line`` = OOM(``s-white``) ||| ``start-of-line``
let ``ns-dec-digit`` = RGO ("\u0030-\u0039", [Token.``ns-dec-digit``])      //  0-9
let ``b-line-feed`` = RGP ("\u000a", [Token.NewLine])
let ``b-carriage-return`` = RGP ("\u000d", [Token.NewLine])

let ``b-break`` = 
    (``b-carriage-return`` + ``b-line-feed``) |||  //  DOS, Windows
    ``b-carriage-return``                          |||  //  MacOS upto 9.x
    ``b-line-feed``                                     //  UNIX, MacOS X
let ``nb-char``  = ``c-printable`` - RGO("\u000a\u000d", [Token.NewLine]) // ``b-char``
let ``b-non-content`` = ``b-break``
let ``b-comment`` = ``b-non-content`` ||| RGP("\\z", [Token.EOF]) // EOF..
let ``ns-yaml-version`` = OOM(``ns-dec-digit``) + RGP("\\.", [Token.``c-printable``]) + OOM(``ns-dec-digit``)
let ``c-nb-comment-text`` = RGP("#", [Token.``t-hash``]) + ZOM(``nb-char``)
let ``ns-yaml-directive`` = RGP("YAML", [Token.``c-printable``]) + ``s-separate-in-line`` + GRP(``ns-yaml-version``)
let ``l-comment`` = ``s-separate-in-line`` + OPT(``c-nb-comment-text``) + ``b-comment``
let ``s-b-comment`` = OPT(``s-separate-in-line`` + OPT(``c-nb-comment-text``)) + ``b-comment`` 
let ``s-l-comments`` = (``s-b-comment`` ||| ``start-of-line``) + ZOM(``l-comment``)
let ``ns-yaml-directive with comments`` = ``ns-yaml-directive`` + ``s-l-comments`` 


let nfa = 
    rgxToNFA <| ``ns-yaml-directive with comments`` 

PrintIt nfa


