#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#r @"bin/Debug/YamlParser.dll"
#r @"NLog/lib/net45/NLog.dll"

open YamlParse
open RegexDSL
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


