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


let ``ns-yaml-directive with comments`` = HardValues.``ns-yaml-directive`` + HardValues.``s-l-comments`` |> rgxToNFA

let nfa = ``ns-yaml-directive with comments`` 

//let nfa = 
//    rgxToNFA <| 
//            OPT(
//                OPT(RGP("A", [Token.``c-printable``]))+ RGP("B", [Token.``c-printable``])
//            ) +
//            RGP("A", [Token.``c-printable``]) 


PrintIt nfa



HardValues.``s-l-comments`` 
|>  rgxToNFA
|>  PrintIt 

(HardValues.``s-b-comment`` ||| HardValues.``start-of-line``) + ZOM(HardValues.``l-comment``)
|>  rgxToNFA
|>  PrintIt 


ZOM(HardValues.``l-comment``)
|>  rgxToNFA
|>  PrintIt 


HardValues.``s-separate-in-line`` + OPT(HardValues.``c-nb-comment-text``) + HardValues.``b-comment``
|>  rgxToNFA
|>  PrintIt 


OPT(HardValues.``c-nb-comment-text``) + HardValues.``b-comment``
|>  rgxToNFA
|>  PrintIt 

OPT(HardValues.``c-nb-comment-text``) 
|>  rgxToNFA
|>  PrintIt 

OPT(RGP("#", [Token.``t-hash``]) + ZOM(HardValues.``nb-char``))
|>  rgxToNFA
|>  PrintIt 

OPT(ZOM(HardValues.``nb-char``))
|>  rgxToNFA
|>  PrintIt 


OPT(ZOM(RGP("A", [Token.``c-printable``])))
|>  rgxToNFA
|>  PrintIt 

