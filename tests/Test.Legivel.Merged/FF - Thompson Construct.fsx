#I __SOURCE_DIRECTORY__ 

#time

//#r @"bin/Debug/net45/FSharp.Core.dll"
//#r @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
#r @"bin/Debug/net45/Legivel.Parser.dll"
//#r @"bin/Debug/net45/NLog.dll"
//#r @"bin/Debug/net45/nunit.framework.dll"

open Legivel.Tokenizer
open Legivel.Utilities.RegexDSL

//  (HardValues.``e-node`` + HardValues.``s-l-comments``)

let nfa = 
    rgxToNFA <| (HardValues.``e-node`` + HardValues.``s-l-comments``)


PrintIt nfa




