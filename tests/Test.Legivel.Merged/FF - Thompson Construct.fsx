#I __SOURCE_DIRECTORY__ 

#time

//#r @"bin/Debug/net45/FSharp.Core.dll"
//#r @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
#r @"bin/Debug/net45/Legivel.Parser.dll"
#r @"bin/Debug/net45/NLog.dll"
#r @"bin/Debug/net45/nunit.framework.dll"
#r @"bin/Debug/net45/Legivel.Tests.dll"

#load "RegexDSL.fsx"

open Legivel.Utilities.RegexDSLScript 

open Legivel.Tokenizer
open System.Drawing
open System.Diagnostics
open NUnit.Framework
open FsUnitTyped
open TestUtils



type ExactChar = char

type OneOfChar = {
        QuickCheck : uint32
        ListCheck  : uint32 list
    }

type SingleCharMatch =
    |   ExactMatch      of ExactChar
    |   OneInSetMatch   of OneOfChar
    |   EmptyMatch
    with
        member this.Match (t:TokenData) =
            match this with
            |   ExactMatch    c  ->  t.Source.[0] = c
            |   OneInSetMatch sm ->
                if uint32(t.Token) >= 0b0100_0000_0000_0000_0000_0000_0000_0000u then
                    sm.ListCheck |> List.exists(fun e -> e=uint32(t.Token))
                else
                    (sm.QuickCheck &&& uint32(t.Token) > 0u)
            |   _ -> failwith "Not a single char match"

type StateId        = System.UInt32                
let PointerToStateFinal = 0u


type MatchTo  = {
        MatchTo    : SingleCharMatch
    }


type SinglePath = {
        Id         : StateId
        State      : MatchTo
        NextState  : StateId 
    }
    with
        static member Create id mt nx = { Id = id; State = mt; NextState = nx }
        member this.LinkTo i = { this with NextState = i}
type MultiPath = {
        Id         : StateId
        States     : StateId list
    }
    with
        static member Create id mt = { Id = id; States = mt }
type StateNode =
    |   SinglePath of SinglePath
    |   MultiPath  of MultiPath
    with
        member this.Id 
            with get() =
                match this with
                |   SinglePath d -> d.Id
                |   MultiPath  d -> d.Id


type NFAMachine = {
        Start  : StateId
        States : StateNode list
    }
    with
        static member Create i s = { States = s; Start = i}



let getNewId =
    let mutable currentId = 0u
    fun() -> 
        currentId <- (currentId + 1u)
        currentId


let getSingle rgx =
    let id = getNewId()
    match rgx with
    |   Plain       d ->
        id, [SinglePath (SinglePath.Create id { MatchTo = ExactMatch(d.``fixed``.[0])} 0u)]
    |   OneInSet    d -> 
        let id = getNewId()
        let subtr = d.Token |> List.map(uint32) |> List.filter(fun e -> OneInSet.subtractable  |> List.map(uint32) |> List.exists(fun s -> e=s)) 
        let listCheck = d.Token'.Force() |> List.map(uint32) |> List.filter(fun tf -> subtr |> List.exists(fun te -> te = tf) |> not)
        let quickCheck = listCheck |> List.fold(fun s i -> s ||| uint32(i)) 0u
        id, [SinglePath (SinglePath.Create id { MatchTo = OneInSetMatch({ QuickCheck = quickCheck; ListCheck = listCheck})} 0u)]
    |   _ -> failwith "Not a single char match"



let rgxToNFA rgx =
    let rec processConversion rgx : NFAMachine =
        let convert rg : NFAMachine =
            match rg with
            |   Plain pl   -> if pl.``fixed``.Length > 1 then processConversion rg else getSingle rg ||> NFAMachine.Create
            |   OneInSet _ -> getSingle rg ||> NFAMachine.Create
            |   _ -> failwith "Not Implemented Yet"
            
        let emptyState() = SinglePath.Create (getNewId()) { MatchTo = EmptyMatch } 0u

        match rgx with
        |   Plain  pl ->
            if pl.``fixed``.Length > 1 then
                pl.OptimizeOnce()
                processConversion (Concat (pl.optimized |> List.rev))
            else
                failwith "Uncontained plain - not implemented yet"
        |   Concat l -> 
            let linkState = SinglePath(emptyState())
            l
            |>  List.map(convert)
            |>  List.rev
            |>  List.map(fun nfa -> nfa.Start, nfa.States)
            |>  List.fold(fun (nextId:StateId, accList:StateNode list) (entryStartId:StateId, entryStateList:StateNode list) ->
                    let linkedStateList = 
                        entryStateList
                        |>  List.map(
                            function
                            |   SinglePath sp -> 
                                if sp.NextState = PointerToStateFinal then 
                                    SinglePath { sp with NextState = nextId }
                                else
                                    SinglePath sp

                            |   MultiPath  mp ->
                                failwith "Uncontained plain - not implemented yet"
                        )
                    let stlst = linkedStateList @ accList
                    (entryStartId, stlst)
                    ) (linkState.Id, [linkState])
            |>  snd
            |>  fun l -> NFAMachine.Create (l.Head.Id) l
        |   Or     l -> 
            let linkState = emptyState()
            l
            |>  List.map(convert)
            |>  List.fold(fun (sil, snl) nfa ->
                    let (mpl, spl) = 
                        nfa.States
                        |>  List.fold(fun (mps,sps) -> 
                            function
                            |   MultiPath  mp -> (mp :: mps), sps
                            |   SinglePath sp -> mps, (sp.Id :: sps)
                        ) ([],[])
                    let mpl2spl =
                        mpl 
                        |>  List.map(fun mp -> mp.Id)
                    ((nfa.Start :: sil) @ mpl2spl), (snl @ nfa.States)
            ) ([],[])
            |>  fun l -> 
                let id = getNewId()
                id, MultiPath(MultiPath.Create id (fst l))  :: (snd l)
            |>  fun (id,sl) -> NFAMachine.Create id sl

        |   _ -> failwith "Not Implemented Yet"
    processConversion rgx


type ParseResult = {
    IsMatch     : bool
    FullMatch   : char list
}

let parseIt (nfa:NFAMachine) yaml =
    let stMap = nfa.States |> List.fold(fun (m:Map<_,_>) i -> m.Add(i.Id, i)) Map.empty<StateId, StateNode>
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")

    let rec processStr cs acc =
        let NoMatch = { IsMatch = false ; FullMatch = [] }
        if cs = PointerToStateFinal then
            { IsMatch = true; FullMatch = acc |> List.rev }
        else
            let st = stMap.[cs]
            match st with
            |   SinglePath p ->
                let nxt = p.NextState
                match p.State.MatchTo with
                |   EmptyMatch -> processStr nxt acc
                |   _ ->
                    let chk = stream.Get()
                    if (p.State.MatchTo.Match chk) then
                        processStr nxt (chk.Source.[0] :: acc)
                    else 
                        NoMatch
            |   MultiPath p ->
                let chk = stream.Get()
                p.States
                |>  List.tryFind(fun t -> 
                    let (SinglePath st) = stMap.[t]
                    st.State.MatchTo.Match chk)
                |>  function
                    |   Some v -> 
                        let (SinglePath st) = stMap.[v]
                        let nxt = st.NextState
                        processStr nxt (chk.Source.[0] :: acc)
                    |   None -> NoMatch 
    processStr nfa.Start []

let clts (cl:char list) = System.String.Concat(cl)


[<Test>]
let ``Simple Concat - match string``() =
    let nfa = rgxToNFA <| RGP("A", [Token.``nb-json``]) + RGP("A", [Token.``nb-json``]) + RGP("B", [Token.``nb-json``])
    let yaml = "AAB"
    let r = parseIt nfa yaml
    r.IsMatch |> shouldEqual true
    r.FullMatch |> clts |> shouldEqual "AAB"


[<Test>]
let ``Simple Concat - nomatch string``() =
    let nfa = rgxToNFA <| RGP("A", [Token.``nb-json``]) + RGP("A", [Token.``nb-json``]) + RGP("B", [Token.``nb-json``])
    let yaml = "aab"
    let r = parseIt nfa yaml
    r.IsMatch |> shouldEqual false
    r.FullMatch |> shouldEqual  []



[<Test>]
let ``Simple Or - match string``() =
    let nfa = rgxToNFA <|  (RGP("A", [Token.``nb-json``]) ||| RGP("B", [Token.``nb-json``]))
    let yaml = "A"
    let r = parseIt nfa yaml
    r.IsMatch |> shouldEqual true


[<Test>]
let ``Simple Or with nested concat - match string``() =
    let nfa = rgxToNFA <|  (RGP("AC", [Token.``nb-json``]) ||| RGP("BC", [Token.``nb-json``]))
    let yaml1 = "AC"
    let yaml2 = "BC"
    let r1 = parseIt nfa yaml1
    let r2 = parseIt nfa yaml2

    r1.IsMatch |> shouldEqual true
    r2.IsMatch |> shouldEqual true

    r1.FullMatch |> clts |> shouldEqual "AC"
    r2.FullMatch |> clts |> shouldEqual "BC"




``Simple Concat - match string``()
``Simple Concat - nomatch string``() 
``Simple Or - match string``()
``Simple Or with nested concat - match string``()



