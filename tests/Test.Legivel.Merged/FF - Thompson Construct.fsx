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
open System.Text.RegularExpressions


type ExactChar = {
        Char       : char
        ListCheck  : uint32 list
    }


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
            |   ExactMatch    c  ->  if t.Source.Length > 0 then t.Source.[0] = c.Char else false
            |   OneInSetMatch sm ->
                if uint32(t.Token) >= 0b0100_0000_0000_0000_0000_0000_0000_0000u then
                    sm.ListCheck |> List.exists(fun e -> e=uint32(t.Token))
                else
                    (sm.QuickCheck &&& uint32(t.Token) > 0u)


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

type EmptyPath = {
        Id         : StateId
        NextState  : StateId 
    }
    with
        static member Create id mt nx = { Id = id; State = mt; NextState = nx }
        member this.LinkTo i = { this with NextState = i}


type StateNode =
    |   SinglePath of SinglePath
    |   MultiPath  of MultiPath
    |   EmptyPath  of EmptyPath
    with
        member this.Id 
            with get() =
                match this with
                |   SinglePath d -> d.Id
                |   MultiPath  d -> d.Id
                |   EmptyPath  d -> d.Id

        member this.IsEmptyPathValue 
            with get() =
                match this with
                |   EmptyPath _ -> true
                |   _ -> false
            

type NFAMachine = {
        Start  : StateId
        States : StateNode list
    }
    with
        static member Create i s = { States = s; Start = i}


let mutable currentId = 0u
let getNewId() =
    currentId <- (currentId + 1u)
    currentId


let qcOneInSet ls = ls |> List.fold(fun s i -> s ||| uint32(i)) 0u
    

let getSingle rgx =
    let id = getNewId()
    match rgx with
    |   Plain       d ->
        id, [SinglePath (SinglePath.Create id { MatchTo = ExactMatch({ Char = d.``fixed``.[0]; ListCheck = d.Token |> List.map(uint32) })} 0u)]
    |   OneInSet    d -> 
        let id = getNewId()
        let listCheck = d.Token'.Force() |> List.map(uint32) 
        let quickCheck = qcOneInSet listCheck
        id, [SinglePath (SinglePath.Create id { MatchTo = OneInSetMatch({ QuickCheck = quickCheck; ListCheck = listCheck})} 0u)]
    |   _ -> failwith "Not a single char match"


let SortStateNodes lst =
    lst
    |>  List.sortWith(fun c1 c2 ->
        match (c1, c2) with
        |   (EmptyPath _,  MultiPath _) -> 1
        |   (SinglePath _, MultiPath _) -> -1
        |   (MultiPath  _, EmptyPath _) -> -1
        |   (MultiPath _, SinglePath _) -> 1
        |   (EmptyPath _, SinglePath _) -> 1
        |   (SinglePath _, EmptyPath _) -> -1
        |   _ -> 0
    )

let rec refactorCommonPlains (sil:StateId list, snl:StateNode list) =
    let stMap = snl |> List.map(fun e -> e.Id, e) |> Map.ofList

    //  holds all plain (exact char matches) for which there are multiple occurences in ``sil``
    let stnl = 
        sil 
        |>  List.map(fun id -> stMap.[id])
        |>  List.map(
            function
            |   SinglePath sp -> Some sp
            |   _  -> None
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
                let target = hd |> snd |> List.map(fun (ch, id, nx) -> (id,nx))
                let (SinglePath primary) = 
                    let id = fst target.Head
                    snl |>  List.find(fun e -> e.Id = id)
                let filterIds = target |> List.map(fst)
                let nextIds = target |> List.map(snd)
                let silNew, snlNew = 
                    let (siln, snln) = refactorCommonPlains (nextIds, snl)
                    let silNew = primary.Id :: (sil |> List.filter(fun e -> filterIds |> List.exists(fun x -> x = e)|> not))
                    if siln.Length = 1 then
                        silNew, SinglePath({ primary with NextState = siln.Head }) :: (snln |>  List.filter(fun e -> filterIds |> List.exists(fun x -> x = e.Id) |> not))
                    else 
                        let isAllEmpty = 
                            siln 
                            |> List.forall(fun e -> 
                                let nd = snl |> List.find(fun sn -> sn.Id = e)
                                nd.IsEmptyPathValue
                            )
                        if isAllEmpty then
                            let link = snl.Head
                            silNew, SinglePath({ primary with NextState = link.Id }) :: link :: (snln |>  List.filter(fun e -> filterIds |> List.exists(fun x -> x = e.Id) |> not))
                        else
                            let silnSorted =
                                siln
                                |>  List.map(fun id -> snln |> List.find(fun st -> st.Id = id))
                                //|>  List.sortWith(fun c1 c2 ->
                                //    match (c1, c2) with
                                //    |   (EmptyPath _,  MultiPath _) -> 1
                                //    |   (SinglePath _, MultiPath _) -> -1
                                //    |   (MultiPath  _, EmptyPath _) -> -1
                                //    |   (MultiPath _, SinglePath _) -> 1
                                //    |   (EmptyPath _, SinglePath _) -> 1
                                //    |   (SinglePath _, EmptyPath _) -> -1
                                //    |   _ -> 0
                                //)
                                |>  SortStateNodes
                                |>  List.map(fun i -> i.Id)
                                
                            let bundle = MultiPath(MultiPath.Create (getNewId()) silnSorted)
                            silNew, SinglePath({ primary with NextState = bundle.Id }) :: bundle :: (snln |>  List.filter(fun e -> filterIds |> List.exists(fun x -> x = e.Id) |> not))
                refactorPlains silNew snlNew tail
        refactorPlains sil snl stnl
   

let nonGenericTokens =
    [   Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark``; Token.``t-colon``;
        Token.``t-dot``; Token.``t-square-bracket-start``; Token.``t-square-bracket-end``; Token.``t-curly-bracket-start``; Token.``t-curly-bracket-end``;
        Token.``t-hash``; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``; Token.``t-gt``;
        Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``; Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``;
        Token.``ns-dec-digit``; Token.``c-escape``; Token.``t-comma``]
    |>  List.map(uint32)
    |>   Set.ofList




let refacorCharacterSets (sil:StateId list, snl:StateNode list) =
    let stMap = snl |> List.map(fun e -> e.Id, e) |> Map.ofList

    let sp = 
        sil 
        |>  List.map(fun id -> stMap.[id])
        |>  List.map(
            function
            |   SinglePath sp -> Some sp
            |   _  -> None
        )
        |>  List.filter(fun e -> e<>None)
        |>  List.map(fun e -> e.Value)  // only singlepath values

    let stnl =
        sp
        |>  List.filter(fun e -> 
            match e.State.MatchTo with
            |   OneInSetMatch _ -> true
            |   _ -> false
            )
        |>  List.map(fun e -> 
            let (OneInSetMatch sm) = e.State.MatchTo
            sm.ListCheck
            |>  List.map(fun t -> t, sm.QuickCheck, e.Id, e.NextState)
        )
        |>  List.collect(id)
        |>  List.map(fun (t, qc, id, nx) -> {| Token = t; QuickCheck = qc; Id = id; Next = nx |})
        |>  List.filter(fun e -> nonGenericTokens |> Set.contains e.Token)
        |>  List.groupBy(fun e -> e.Token)

    ////  Combine duplicate tokens
    //let rec refactorCharacterSets sil snl stnl =
    //    match stnl with
    //    |   []  -> (sil, snl)
    //    |   hd :: tail ->
    //        let target = 
    //            hd |> snd |> List.map(fun (t, qc, id, nx) -> (id,nx))
    //        let (SinglePath primary) = 
    //            let id = fst hd
    //            snl |>  List.find(fun e -> e.Id = id)
    //refactorCharacterSets sil snl (stnl |> List.filter(fun (t, ls) -> List.length ls > 1))

    // try to combine with exactchar
    let ec = 
        let targetTokens = stnl |> List.map(fst)
        sp
        |>  List.filter(fun e  -> 
            match e.State.MatchTo with
            |   ExactMatch ec -> ec.ListCheck.Length = 1 && ec.ListCheck |> List.exists(fun x -> targetTokens |> List.contains(x))
            |   _ -> false
        )
        |>  List.map(fun e -> 
            let (ExactMatch ch) = e.State.MatchTo
            {| Token = ch.ListCheck.Head; ExactChar = ch; Id = e.Id; Next = e.NextState |})
        |>  List.groupBy(fun e -> e.Token)
        |>  List.fold(fun st (el,elst) ->
            let (cht, clst) = stnl |> List.find(fun (cht, lst) -> cht = el)
            (cht, clst, elst) :: st
        ) []
    if List.length ec = 0 then
        (sil, snl)
    else
        let rec refactorPlainsWithCharSets sil (snl:StateNode list) (stnl:(uint32*({| Token : uint32; QuickCheck : uint32; Id : StateId; Next : StateId |} list)*({| Token : uint32; ExactChar : ExactChar; Id :StateId; Next : StateId|} list)) list) =
            match stnl with
            |   []  -> (sil, snl)
            |   (cht, clst, elst)  :: tail -> 
                let (SinglePath primary) = elst |> List.head |> fun i -> stMap.[i.Id]
                let toRemove, allNextIds =
                    clst
                    |>  List.map(fun e -> e.Id, e.Next)
                    |>  List.append (elst |> List.map(fun e -> e.Id, e.Next))
                    |>  List.distinct
                    |>  List.unzip

                let allRefactoredOneInSet =
                    let oisids = clst |> List.map(fun e -> e.Id)
                    let (nodes:StateNode list) = oisids |> List.map(fun i -> stMap.[i])
                    let newNodes =
                        nodes
                        |>  List.map(fun n ->
                            match n with
                            |   SinglePath sp -> 
                                let (OneInSetMatch ois) = sp.State.MatchTo
                                let newLC = ois.ListCheck |> List.filter(fun t -> t <> cht)
                                let newQC = qcOneInSet newLC
                                SinglePath({ sp with State = {MatchTo = OneInSetMatch({ QuickCheck = newQC; ListCheck = newLC})}})
                            |   _ -> failwith "Not implemented - this should never happen"
                        )
                    newNodes
                let snln =
                    snl 
                    |>  List.filter(fun e -> toRemove |> List.exists(fun x -> x = e.Id) |> not)
                    |>  List.append allRefactoredOneInSet

                let bundle = MultiPath(MultiPath.Create (getNewId()) allNextIds)

                //  aan silNew moeten nog de nieuwe aangemaakte nodes worden toegevoegd
                let silNew = 
                    sil 
                    |>  List.filter(fun i -> toRemove |> List.contains(i) |> not)
                    |>  List.append ( primary.Id :: (allRefactoredOneInSet |> List.map(fun i -> i.Id)))
                silNew, SinglePath({ primary with NextState = bundle.Id }) :: bundle :: snln

        refactorPlainsWithCharSets sil snl ec


let getAllTailNodes startId (snl:StateNode list) =
    let nm = snl |> List.map(fun n -> n.Id, n) |> Map.ofList
    let passedNodes = Set.empty<StateId>
    
    let rec traverse prev current (found, toRemove) (passedNodes:Set<StateId>) =
        if  current = 0u || passedNodes.Contains (current) then (prev :: found, toRemove)
        else
            let node = nm.[current]
            match node with
            |   SinglePath sp -> traverse (sp.Id) (sp.NextState) (found, toRemove) (passedNodes.Add (sp.Id))
            |   MultiPath  mp -> 
                mp.States 
                |> List.fold(fun (fnd, tr) stid -> traverse (mp.Id) stid (fnd, tr) (passedNodes.Add mp.Id)) (found, toRemove)
            |   EmptyPath  ep -> 
                match nm.[prev] with
                |   SinglePath _ -> (prev :: found),(current::toRemove)
                |   MultiPath  _ -> found,toRemove
                |   _ -> failwith "Not implemented - this should never happen"

    traverse 0u startId ([], []) passedNodes

let rgxToNFA rgx =
    currentId <- 0u
    let rec processConversion rgx : NFAMachine =
        let convert rg : NFAMachine =
            match rg with
            |   Plain pl   -> if pl.``fixed``.Length > 1 then processConversion rg else getSingle rg ||> NFAMachine.Create
            |   OneInSet _ -> getSingle rg ||> NFAMachine.Create
            |   _ -> processConversion rg
            
        let emptyState() = EmptyPath({ Id = getNewId(); NextState = 0u })

        match rgx with
        |   Plain  pl ->
            if pl.``fixed``.Length > 1 then
                pl.OptimizeOnce()
                processConversion (Concat (pl.optimized))
            else
                failwith "Uncontained plain - not implemented yet"
        |   Concat l -> 
            let linkState = emptyState()
            l
            |>  List.map(convert)
            |>  List.map(fun nfa -> nfa.Start, nfa.States)
            |>  List.fold(fun (nextId:StateId, accList:StateNode list) (entryStartId:StateId, entryStateList:StateNode list) ->
                    let (fnd,rmv) = getAllTailNodes entryStartId entryStateList
                    let rewiredNodes = 
                        entryStateList 
                        |>  List.filter(fun n -> List.contains(n.Id) fnd)
                        |>  List.map(fun  e ->
                            match e with
                            |   SinglePath sp -> SinglePath { sp with NextState = nextId }
                            |   _ -> failwith "Not implemented - this should never happen"
                        )

                    let linkedStateList = 
                        let removeList = fnd @ rmv
                        entryStateList 
                        |>  List.filter(fun n -> not(List.contains(n.Id) removeList))
                        |>  List.append rewiredNodes

                    let stlst = linkedStateList @ accList
                    (entryStartId, stlst)
                    ) (linkState.Id, [linkState])
            ||>  NFAMachine.Create
        |   Or     l -> 
            l
            |>  List.map(convert)
            |>  List.fold(fun (sil, snl) nfa ->
                    let mpl = 
                        nfa.States
                        |>  List.find(fun e -> e.Id = nfa.Start)
                        |>  function
                            |   MultiPath  mp -> mp.States
                            |   _             -> []
                    ((nfa.Start :: sil) @ mpl), (snl @ nfa.States)
            ) ([],[])
            |>  refacorCharacterSets
            |>  refactorCommonPlains
            |>  fun (idlst, snlst) -> 
                if idlst.Length = 1 then
                    let id = idlst.Head
                    id, snlst
                else
                    let id = getNewId()
                    id, MultiPath(MultiPath.Create id (idlst)) :: snlst
            |>  fun (id,sl) -> NFAMachine.Create id sl

        |   _ -> failwith "Not Implemented Yet"
    processConversion rgx


type ParseResult = {
    IsMatch     : bool
    FullMatch   : char list
}


type LevelType = 
    |   Concat = 0
    |   Multi   = 1


let PrintIt (nfa:NFAMachine) =
    let stMap = nfa.States |> List.map(fun e -> e.Id,e) |> Map.ofList


    let rec printLine (hist : LevelType list) (current: StateId) =
        hist
        |>  List.rev
        |>  List.iter(fun i ->
            match i with
            |   LevelType.Concat    -> printf "         "
            |   LevelType.Multi     -> printf "|    "
        )

        let rec printLineRest (hist : LevelType list) (current: StateId) =
            if current = 0u then
                printf "-*\n"
            else
                match stMap.[current] with
                |   EmptyPath  ep   ->  
                    printf "~"
                    printLineRest (LevelType.Concat :: hist) ep.NextState
                |   SinglePath sp   -> 
                    match sp.State.MatchTo with
                    |   ExactMatch c    -> printf "-(%2d:\"%s\")" sp.Id (Regex.Escape(c.Char.ToString()))
                    |   OneInSetMatch o -> printf "-(%2d:[@])" sp.Id
                    printLineRest (LevelType.Concat :: hist) sp.NextState
                |   MultiPath mp    ->
                    let h::t = mp.States
                    printf "|(%2d)" mp.Id
                    printLineRest (LevelType.Multi :: hist) h
                    t |> List.iter(fun e -> printLine (LevelType.Multi :: hist) e)
        printLineRest hist current
    printLine [] nfa.Start


let parseIt (nfa:NFAMachine) yaml =
    let stMap = nfa.States |> List.fold(fun (m:Map<_,_>) i -> m.Add(i.Id, i)) Map.empty<StateId, StateNode>
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")

    let rec processStr cs acc rollback =
        let NoMatch = { IsMatch = false ; FullMatch = [] }
        if cs = PointerToStateFinal then
            { IsMatch = true; FullMatch = acc |> List.rev }
        else
            let st = stMap.[cs]
            match st with
            |   SinglePath p ->
                let nxt = p.NextState
                let chk = stream.Get()
                if (p.State.MatchTo.Match chk) then
                    processStr nxt (chk.Source.[0] :: acc) rollback
                else 
                    NoMatch
            |   MultiPath p ->
                let chk = stream.Get()
                p.States
                |>  List.tryFind(fun t -> 
                    match stMap.[t] with
                    |   SinglePath st -> st.State.MatchTo.Match chk
                    |   EmptyPath  _  -> true
                    |   _ -> failwith "Not implemented yet"
                    )
                |>  function
                    |   Some v -> 
                        match stMap.[v] with
                        |   EmptyPath  st -> processStr (st.NextState) acc (rollback+1) 
                        |   SinglePath st -> processStr (st.NextState) (chk.Source.[0] :: acc) rollback
                        |   MultiPath  _  -> failwith "Not implemented yet"
                    |   None -> NoMatch 
            |   EmptyPath p -> processStr p.NextState acc rollback
                


    processStr nfa.Start [] 0

let clts (cl:char list) = System.String.Concat(cl)

module ParseResult =
    let IsMatch pr = pr.IsMatch
    let FullMatch pr = pr.FullMatch


[<Test>]
let ``Simple Concat - match string``() =
    let nfa = rgxToNFA <| RGP("A", [Token.``c-printable``]) + RGP("A", [Token.``c-printable``]) + RGP("B", [Token.``c-printable``])
    let r = parseIt nfa "AAB"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AAB"

    let r = parseIt nfa "aab"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual  []


[<Test>]
let ``Simple Or - match string``() =
    let nfa = rgxToNFA <|  (RGP("A", [Token.``c-printable``]) ||| RGP("B", [Token.``c-printable``]))
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
    let nfa = rgxToNFA <|  (RGP("AC", [Token.``c-printable``]) ||| RGP("BC", [Token.``c-printable``]))
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

[<Test>]
let ``Simple Or with concat before - match string``() =
    let nfa = rgxToNFA <|  RGP("A", [Token.``c-printable``]) + (RGP("C", [Token.``c-printable``]) ||| RGP("B", [Token.``c-printable``]))
    let r = parseIt nfa "AC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AC"

    let r = parseIt nfa "AB"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AB"
    
    let r = parseIt nfa "B"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "AD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


[<Test>]
let ``Simple Or with concat after - match string``() =
    let nfa = rgxToNFA <|  (RGP("A", [Token.``c-printable``]) ||| RGP("B", [Token.``c-printable``])) + RGP("GH", [Token.``c-printable``])  
    let r = parseIt nfa "AGH"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AGH"

    let r = parseIt nfa "BGH"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "BGH"
    
    let r = parseIt nfa "C"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "BA"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


[<Test>]
let ``Complex Or with various nested concats - match string``() =
    let nfa = 
        rgxToNFA <| 
        RGP("XY", [Token.``c-printable``]) + 
        (RGP("A", [Token.``c-printable``]) ||| RGP("B", [Token.``c-printable``])) + 
        RGP("GH", [Token.``c-printable``]) + 
        (RGP("ABD", [Token.``c-printable``]) ||| RGP("ABDAC", [Token.``c-printable``]))

    let r = parseIt nfa "XYAGHABD"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "XYAGHABD"

    let r = parseIt nfa "XYBGHABD"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "XYBGHABD"
    
    let r = parseIt nfa "XYBGHABDAC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "XYBGHABDAC"

    let r = parseIt nfa "XYC"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "XYCABE"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


[<Test>]
let ``Complex Or with deep nested concats - match string``() =
    let nfa = 
        rgxToNFA <| 
        RGP("XY", [Token.``c-printable``]) + 
        (RGP("AB", [Token.``c-printable``]) ||| (RGP("BA", [Token.``c-printable``]) + 
            (RGP("CX", [Token.``c-printable``]) ||| RGP("DX", [Token.``c-printable``])))) + 
            RGP("GH", [Token.``c-printable``]) + 
            (RGP("ABD", [Token.``c-printable``]) ||| RGP("ABDAC", [Token.``c-printable``]))

    let r = parseIt nfa "XYABGHABD"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "XYABGHABD"

    let r = parseIt nfa "XYBACXGHABDAC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "XYBACXGHABDAC"
    
    let r = parseIt nfa "XYBADXGHABD"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "XYBADXGHABD"

    let r = parseIt nfa "XYC"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "XYBADY"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


let ``Simple Or with simple overlapping concat - match string``() =
    let nfa = rgxToNFA <|  (RGP("AB", [Token.``c-printable``]) ||| RGP("AC", [Token.``c-printable``]))
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
            RGP("AAB",  [Token.``c-printable``]) 
        ||| RGP("AACA", [Token.``c-printable``]) 
        ||| RGP("AACB", [Token.``c-printable``]) 
        ||| RGP("AABA", [Token.``c-printable``]) 
        ||| RGP("BA",   [Token.``c-printable``])
        ||| RGP("BC",   [Token.``c-printable``])
        ||| RGP("CD",   [Token.``c-printable``])
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

    let r = parseIt nfa "AABA"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AABA"

    let r = parseIt nfa "AABD"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AAB"

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
    let nfa = rgxToNFA <|  ((RGP("\n", [Token.NewLine]) + RGP("A", [Token.``c-printable``])) ||| (RGO("B\n", [Token.``c-printable``;Token.NewLine]) + RGP("X", [Token.``c-printable``])))
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
``Simple Or with concat before - match string``()
``Simple Or with concat after - match string``()
``Complex Or with various nested concats - match string``()
``Complex Or with deep nested concats - match string``()
``Conflicting Plain/OneOf within Or with simple concat - match string``()


let nfa = 
    rgxToNFA <|  ((RGP("\n", [Token.NewLine]) + RGP("A", [Token.``nb-json``])) ||| (RGO("B\n", [Token.``nb-json``;Token.NewLine]) + RGP("X", [Token.``nb-json``])))


PrintIt nfa


