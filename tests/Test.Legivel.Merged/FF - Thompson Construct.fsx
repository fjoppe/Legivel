#I __SOURCE_DIRECTORY__ 

#time

//#r @"bin/Debug/net45/FSharp.Core.dll"
//#r @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
#r @"bin/Debug/net45/Legivel.Parser.dll"
//#r @"bin/Debug/net45/NLog.dll"
//#r @"bin/Debug/net45/nunit.framework.dll"

open Legivel.Tokenizer
open Legivel.Utilities.RegexDSL


//let this.``b-non-content`` + OOM(this.``l-empty`` ps)
//let ``s-double-escaped`` = ZOM(this.``s-white``) + this.``c-escape`` + this.``b-non-content`` + ZOM(this.``l-empty`` (ps.SetStyleContext Context.``Flow-in``)) + (this.``s-flow-line-prefix`` ps)
//let ``b-l-folded`` = (this.``b-l-trimmed`` ps) ||| HardValues.``b-as-space``
//let ``s-flow-folded``  =
//    OPT(HardValues.``s-separate-in-line``) + (``b-l-folded`` (ps.SetStyleContext Context.``Flow-in``)) + (this.``s-flow-line-prefix`` ps)

//let ``s-double-break`` = (``s-double-escaped`` ) ||| (``s-flow-folded`` )

//HardValues.``c-double-quote`` + OOM((HardValues.``nb-json`` - HardValues.``c-double-quote``) ||| ``s-double-break``) + HardValues.``c-double-quote``


HardValues.``c-ns-local-tag-prefix``
|>  rgxToNFA
|>  PrintIt 




