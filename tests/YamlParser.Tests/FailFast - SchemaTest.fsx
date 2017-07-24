#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#r @"bin/Debug/YamlParser.dll"
#r @"NLog/lib/net45/NLog.dll"

open RepresentationGraph
open TagResolution
open RegexDSL

let rgyear = Repeat(RGO("0-9"),4)
let rgmonth = RGO("0-9") + OPT(RGO("0-9"))
let rgmonthf = Repeat(RGO("0-9"),2)
let rgday = RGO("0-9") + OPT(RGO("0-9"))
let rgdayf = Repeat(RGO("0-9"),2)
let rgdate = (GRP rgyear) + RGP("-") + (GRP rgmonth) + RGP("-") + (GRP rgday)
let rgdatef = (GRP rgyear) + RGP("-") + (GRP rgmonthf) + RGP("-") + (GRP rgdayf)
let rghour = RGO("0-9") + OPT(RGO("0-9"))
let rgmin = Repeat(RGO "0-9", 2)
let rgsec = Repeat(RGO "0-9", 2)
let rgfrac= RGP("\.") + ZOM(RGO "0-9")
let rgtime = (GRP rghour) + RGP(":") + (GRP rgmin) + RGP(":") + (GRP rgsec) + (GRP(OPT rgfrac))
let rgztimez = RGP("Z")
let rgdtimez = (RGO "-+") + rghour + OPT(RGP(":") + rgmin)
let rgws = ZOM(RGO " \t")
 
let rgISO8601 = rgdate + OPT(((RGO "Tt") ||| OOM(rgws)) + rgtime + OPT(rgws + GRP((rgztimez ||| rgdtimez))))
let rgtimestamp = rgdate ||| rgISO8601


//let p = "^([0-9][0-9][0-9][0-9])-([0-9][0-9]?)-([0-9][0-9]?)(?:[Tt]|[ \t]+)([0-9][0-9]?):([0-9][0-9]):([0-9][0-9])(?:\.([0-9]*))?(?:[ \t]*)?((?:[-+][0-9][0-9])?(?:\:[0-9][0-9])?)?$"
//let p = "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]|[0-9][0-9][0-9][0-9]-[0-9][0-9]?-[0-9][0-9]?([Tt]|[ \t]+)[0-9][0-9]?:[0-9][0-9]:[0-9][0-9](\.[0-9]*)?(([ \t]*)Z|[-+][0-9][0-9]?(:[0-9][0-9])?)?$"
let p = RGSF(rgISO8601)
let s = "2001-12-14t21:59:43.10-05:00"

IsMatch(s,p)

(|Regex|_|) p s |> Option.get

let [full; year; month; day; hour; min; sec; fraction; timezone] = (|Regex|_|) p s |> Option.get


System.DateTime.Now.ToUniversalTime().ToString("o")

printfn "%04d" 2

System.DateTime.Parse("2001-12-14t21:59:43.10-05:00").ToUniversalTime().ToString("o")

System.DateTime.Parse("2001-12-14 21:59:43.10 -5").ToUniversalTime().ToString("o")

