#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#r @"bin/Debug/Legivel.Common.dll"

open System.Text
open Legivel.Common

let fo = FallibleOption<string,int list>.Value ("hi")

fo
|>  FallibleOption.bind(fun s -> FallibleOption<float,int list>.Value 60.0)
|>  FallibleOption.map(fun fl -> fl * 2.0)



