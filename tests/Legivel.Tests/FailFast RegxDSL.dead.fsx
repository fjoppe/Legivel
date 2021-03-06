﻿#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#r @"bin/Debug/Legivel.Parser.dll"
#r @"NLog/lib/net45/NLog.dll"

open Legivel.Parser
open Legivel.RegexDSL
open NLog

#load "nlog.fsx"
NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let logger = LogManager.GetLogger("*")


let RGP c = Plain(Plain.Create c)
let RGO c = OneInSet(OneInSet.Create c)

let y = RGP "yaml"
let s = RGO "a-zA-Z"
let u = RGO "bml"
let t = RGP "stoppy"

s + u
y + t

let c = y+t ||| s-u ||| t
let d = c ||| (RGP "yuk")

d.ToString()

let ymp = Yaml12Parser()
let ps = ParseState.Create "" (TagResolution.FailsafeSchema) |> ParseState.SetStyleContext (Context.``Flow-in``)
let ps1 = ps.SetRestString "http://example.com/foo#bar"
//let ps1 = ps.SetRestString "o#bar"
let r = ZOM((ymp.``ns-char`` + (RGP "#")) ||| ((ymp.``ns-plain-safe`` ps1) - (RGO ":#")) ||| ((RGP ":") + (ymp.``ns-plain-safe`` ps1)))

(|Regex3|_|) r ps1



open System.Text.RegularExpressions

let catchupString = "
1 hi
2 hell56
skipit"

let patt = "\x0d\x0a|\x0d|\x0a"

(Regex.Split(catchupString, patt) |> List.ofArray |> List.last).Length
