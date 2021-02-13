#I __SOURCE_DIRECTORY__ 

#time

//#r @"bin/Debug/net45/FSharp.Core.dll"
//#r @"C:\Users\frank\AppData\Local\assembly\dl3\TKW89VBG.392\6P91C2V3.50M\74b4f6e9\00c5fa3e_5449d501\FSharp.Core.dll"
#r @"bin/Debug/net45/Legivel.Parser.dll"
#r @"bin/Debug/net45/Legivel.Mapper.dll"
#r @"bin/Debug/net45/NLog.dll"


//#r @"bin/Release/net45/FSharp.Core.dll"
//#r @"bin/Release/net45/Legivel.Parser.dll"
//#r @"bin/Release/net45/NLog.dll"

open System
open System.Globalization
open Legivel.Parser
open Legivel.TagResolution
open Legivel.Serialization
open Legivel.RepresentationGraph
open Legivel.Common
open NLog
open System.IO

#load "nlog.fsx"

open System
open System.Globalization


type Unsupported = {
    Field : uint
}

Deserialize<Unsupported> "
Field : 4294967296
"

