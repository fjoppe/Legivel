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
        member this.IsEmptyMatch 
            with get() =
                match this with
                |   EmptyMatch -> true
                |   _ -> false

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


let rec refactorCommonPlains (sil:StateId list) (snl:StateNode list) =
    //  holds all plain (exact char matches) for which there are multiple occurences in ``sil``
    let stnl = 
        sil 
        |>  List.map(fun id -> snl |>  List.find(fun e -> e.Id = id))
        |>  List.map(
            function
            |   MultiPath  _  -> None
            |   SinglePath sp -> Some sp
        )
        |>  List.filter(fun e -> e<>None)
        |>  List.map(fun e -> e.Value)  // only singlepath values
        |>  List.filter(fun e -> 
            match e.State.MatchTo with
            |   ExactMatch _ -> true
            |   _ -> false
            )
        |>  List.map(fun e -> 
            let (ExactMatch ch) = e.State.MatchTo
            ch, e.Id, e.NextState
        )   //  only exactmatch values 
        |>  List.groupBy(fun (ch, id, nx) -> ch)
        |>  List.filter(fun (ch,lst) -> List.length lst > 1)    // only with more than one occurrence (ie only ambigious situations)

    if stnl.Length = 0 then
        (sil, snl)
    else
        let rec refactorPlains sil snl stnl =
            match stnl with
            |   []  -> (sil, snl)
            |   hd :: tail ->
                let target = 
                    let idl = hd |> snd |> List.map(fun (ch, id, nx) -> (id,nx))
                    idl
                let (SinglePath primary) = 
                    let id = fst target.Head
                    snl |>  List.find(fun e -> e.Id = id)
                let filterIds = target |> List.map(fst)
                let nextIds = target |> List.map(snd)
                let silNew, snlNew = 
                    let (siln, snln) = refactorCommonPlains nextIds snl
                    let silNew = primary.Id :: (sil |> List.filter(fun e -> filterIds |> List.exists(fun x -> x = e)|> not))
                    if siln.Length = 1 then
                        silNew, SinglePath({ primary with NextState = siln.Head }) :: (snln |>  List.filter(fun e -> filterIds |> List.exists(fun x -> x = e.Id) |> not))
                    else 

                        /// how to deal with empty matches?
                        /// it is possible that a character does not match, while all chars before matched into a final state.
                        /// ie in a multipath, a correct char continues parsing, while an incorrect char exits in final state.
                        /// this also may happen for multiple chars.

                        let isAllEmpty = 
                            siln 
                            |> List.forall(fun e -> 
                                let nd = snl |> List.find(fun sn -> sn.Id = e)
                                match nd with
                                |   SinglePath sp when sp.State.MatchTo.IsEmptyMatch -> true
                                |   _ -> false
                            )
                        if isAllEmpty then
                            
                        else
                            let bundle = MultiPath(MultiPath.Create (getNewId()) siln)
                            silNew, SinglePath({ primary with NextState = bundle.Id }) :: bundle :: (snln |>  List.filter(fun e -> filterIds |> List.exists(fun x -> x = e.Id) |> not))
                refactorPlains silNew snlNew tail
        refactorPlains sil snl stnl


let rgxToNFA rgx =
    let rec processConversion rgx : NFAMachine =
        let convert rg : NFAMachine =
            match rg with
            |   Plain pl   -> if pl.``fixed``.Length > 1 then processConversion rg else getSingle rg ||> NFAMachine.Create
            |   OneInSet _ -> getSingle rg ||> NFAMachine.Create
            |   _ -> processConversion rg
            
        let emptyState() = SinglePath.Create (getNewId()) { MatchTo = EmptyMatch } 0u

        match rgx with
        |   Plain  pl ->
            if pl.``fixed``.Length > 1 then
                pl.OptimizeOnce()
                processConversion (Concat (pl.optimized (*|> List.rev*)))
            else
                failwith "Uncontained plain - not implemented yet"
        |   Concat l -> 
            let linkState = SinglePath(emptyState())
            l
            |>  List.map(convert)
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
            ||> refactorCommonPlains
            |>  fun l -> 
                let idlst = fst l
                let snlst = snd l
                if idlst.Length = 1 then
                    let id = idlst.Head
                    id, snlst
                else
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

module ParseResult =
    let IsMatch pr = pr.IsMatch
    let FullMatch pr = pr.FullMatch


[<Test>]
let ``Simple Concat - match string``() =
    let nfa = rgxToNFA <| RGP("A", [Token.``nb-json``]) + RGP("A", [Token.``nb-json``]) + RGP("B", [Token.``nb-json``])
    let r = parseIt nfa "AAB"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AAB"

    let r = parseIt nfa "aab"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual  []


[<Test>]
let ``Simple Or - match string``() =
    let nfa = rgxToNFA <|  (RGP("A", [Token.``nb-json``]) ||| RGP("B", [Token.``nb-json``]))
    let r = parseIt nfa "A"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "A"

    let r = parseIt nfa "B"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "B"

    let r = parseIt nfa "C"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual  []


[<Test>]
let ``Simple Or with nested concat - match string``() =
    let nfa = rgxToNFA <|  (RGP("AC", [Token.``nb-json``]) ||| RGP("BC", [Token.``nb-json``]))
    let r = parseIt nfa "AC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AC"

    let r = parseIt nfa "BC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "BC"

    let r = parseIt nfa "AB"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "BD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "C"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []



let ``Simple Or with simple overlapping concat - match string``() =
    let nfa = rgxToNFA <|  (RGP("AB", [Token.``nb-json``]) ||| RGP("AC", [Token.``nb-json``]))
    let r = parseIt nfa "AB"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AB"

    let r = parseIt nfa "AC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AC"

    let r = parseIt nfa "AD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "B"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


let ``Simple Or with nested overlapping concat - match string``() =
    let nfa = rgxToNFA <|  
        (
            RGP("AAB",  [Token.``nb-json``]) 
        ||| RGP("AACA", [Token.``nb-json``]) 
        ||| RGP("AACB", [Token.``nb-json``]) 
        ||| RGP("AAB",  [Token.``nb-json``]) 
        ||| RGP("BA",   [Token.``nb-json``])
        ||| RGP("BC",   [Token.``nb-json``])
        ||| RGP("CD",   [Token.``nb-json``])
        )
    let r = parseIt nfa "AAB"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AAB"

    let r = parseIt nfa "AACA"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AACA"

    let r = parseIt nfa "AACB"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AACB"

    let r = parseIt nfa "BA"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "BA"

    let r = parseIt nfa "BC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "BC"

    let r = parseIt nfa "CD"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "CD"

    let r = parseIt nfa "AACD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "AAD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "AD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "D"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


[<Test>]
let ``Conflicting Plain/OneOf within Or with simple concat - match string``() =
    let nfa = rgxToNFA <|  ((RGP("\n", [Token.NewLine]) + RGP("A", [Token.``nb-json``])) ||| (RGO("B\n", [Token.``nb-json``;Token.NewLine]) + RGP("X", [Token.``nb-json``])))
    let r = parseIt nfa "\nA"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "\nA"

    let r = parseIt nfa "\nX"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "\nX"

    let r = parseIt nfa "BX"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "BX"

    let r = parseIt nfa "?"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


``Simple Concat - match string``()
``Simple Or - match string``()
``Simple Or with nested concat - match string``()
``Simple Or with simple overlapping concat - match string``()
``Simple Or with nested overlapping concat - match string``()

``Conflicting Plain/OneOf within Or with simple concat - match string``()


