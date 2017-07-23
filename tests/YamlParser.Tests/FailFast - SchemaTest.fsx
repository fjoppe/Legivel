#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#r @"bin/Debug/YamlParser.dll"
#r @"NLog/lib/net45/NLog.dll"

open RepresentationGraph
open TagResolution
open RegexDSL

let p = "^([0-9][0-9][0-9][0-9])-([0-9][0-9]?)-([0-9][0-9]?)(?:[Tt]|[ \t]+)([0-9][0-9]?):([0-9][0-9]):([0-9][0-9])(?:\.([0-9]*))?(?:(?:[ \t]*)Z|([-+][0-9][0-9])?(?::([0-9][0-9]))?)?$"
let s = "2001-12-15 2:59:43.10"

IsMatch(s,p)

(|Regex|_|) p s |> Option.get

let [year; month; day; hour; min; sec; fraction; tzzero; tzoffset] = (|Regex|_|) p s |> Option.get


printfn "%04d" 2
