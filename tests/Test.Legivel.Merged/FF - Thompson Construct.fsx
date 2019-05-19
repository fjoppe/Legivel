#I __SOURCE_DIRECTORY__ 

#time

#r @"bin/Debug/net45/FSharp.Core.dll"
//#r @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
#r @"bin/Debug/net45/Legivel.Parser.dll"
#r @"bin/Debug/net45/NLog.dll"

#load "RegexDSL.fsx"

open Legivel.Utilities.RegexDSLScript 

open Legivel.Tokenizer
open System.Drawing

let pat1 = RGP("A", [Token.``nb-json``]) + RGP("A", [Token.``nb-json``]) + RGP("B", [Token.``nb-json``])
let pat2 = RGP("A", [Token.``nb-json``]) ||| RGP("B", [Token.``nb-json``])

type ExactChar = char

type OneOfChar = {
        QuickCheck : uint32
        ListCheck  : uint32 list
    }

type SingleCharMatch =
    |   ExactMatch      of ExactChar
    |   OneInSetMatch   of OneOfChar
    with
        member this.Match (t:TokenData) =
            match this with
            |   ExactMatch    c  ->  t.Source.[0] = c
            |   OneInSetMatch sm ->
                if uint32(t.Token) >= 0b0100_0000_0000_0000_0000_0000_0000_0000u then
                    sm.ListCheck |> List.exists(fun e -> e=uint32(t.Token))
                else
                    (sm.QuickCheck &&& uint32(t.Token) > 0u)
                

type PointerToState = int
type MatchTo  = {
        MatchTo    : SingleCharMatch
        NextState  : PointerToState 
    }

type SinglePath = {
    State : MatchTo
}

type multiPath = {
    States  :   MatchTo list
}



let RgxToStateMachine (rgx:RGXType) =
    let getNewId =
        let mutable currentId = 0
        fun() -> 
            currentId <- (currentId + 1)
            currentId

    let rec conv offset (rgx:RGXType) =
        match rgx with
        |   Plain       d -> { MatchTo = ExactMatch(d.``fixed``.[0]); NextState = 0 }
        |   OneInSet    d -> 
            let subtr = d.Token |> List.filter(fun e -> OneInSet.subtractable |> List.exists(fun s -> e=s))
            let listCheck = d.Token'.Force() |> List.filter(fun tf -> subtr |> List.exists(fun te -> te = tf) |> not)
            let quickCheck = listCheck |> List.fold(fun s i -> s ||| uint32(i)) 0u
            { MatchTo = OneInSetMatch({ QuickCheck = quickCheck; ListCheck = listCheck}); NextState = 0}
        |   Concat l -> l |> List.map(fun e -> )
    cov rgx 0


