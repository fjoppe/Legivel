#I __SOURCE_DIRECTORY__ 
#I "../../packages"
#I __SOURCE_DIRECTORY__

#r @"FSharp.Data\lib\net45\FSharp.Data.dll"
#r @"FSharp.Charting\lib\net45\FSharp.Charting.dll"

open System.IO
open FSharp.Data
open FSharp.Charting

type Lines = CsvProvider<"./lines profiling.csv", HasHeaders=false>

let data = Lines.Load(Path.Combine(__SOURCE_DIRECTORY__, "lines profiling.csv"))

data.Rows
|>  Seq.groupBy(fun e -> e)
|>  Seq.map(fun (e,s) -> e, Seq.length s)
|>  Chart.Column
|>  Chart.Show



