module Legivel.ThompsonParser

open Legivel.Utilities.RegexDSL

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


type SinglePathPointer = | SinglePathPointer of StateId
    with
        static member Create i = SinglePathPointer i
        member this.Id 
            with get() = 
                let (SinglePathPointer i) = this
                i
        member this.StatePointer = StatePointer.SinglePathPointer(this)

and MultiPathPointer  = | MultiPathPointer  of StateId
    with
        static member Create i = MultiPathPointer i
        member this.Id 
            with get() = 
                let (MultiPathPointer i) = this
                i
        member this.StatePointer = StatePointer.MultiPathPointer(this)

and StatePointer =
    |   SinglePathPointer of SinglePathPointer
    |   MultiPathPointer  of MultiPathPointer
    member this.Id  
        with get() =
            match this with
            | SinglePathPointer i -> i.Id
            | MultiPathPointer  i -> i.Id
    member this.StatePointer 
        with get() = 
            match this with
            | SinglePathPointer i -> i.StatePointer
            | MultiPathPointer  i -> i.StatePointer

    member this.SinglePathPointerValue 
        with get() =
            match this with
            | SinglePathPointer i -> i
            | MultiPathPointer  i -> failwith "MultiPathPointer has no SinglePathPointer"
    member this.IsSinglePath
        with get() =
            match this with
            | SinglePathPointer i -> true
            | MultiPathPointer  i -> false

            

type RepeatId = | RepeatId of System.UInt32
    with
        member this.Id 
            with get() = 
                let (RepeatId i) = this
                i


type SinglePath = {
        Id         : StateId
        State      : SingleCharMatch
        NextState  : StatePointer 
    }
    with
        static member Create id mt nx = { Id = id; State = mt; NextState = nx }
        member this.LinkTo i = { this with NextState = i}
        member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
        member this.SinglePathPointer = SinglePathPointer.Create this.Id


type MultiPath = {
        Id         : StateId
        States     : SinglePathPointer list
    }
    with
        static member Create id mt = { Id = id; States = mt }
        member this.StatePointer = MultiPathPointer(MultiPathPointer.Create this.Id)


type RepeatStart = {
        Id          : StateId
        RepeatId    : RepeatId
        NextState   : SinglePathPointer 
    }
    with
        static member Create id ri nx = { Id = id; RepeatId = ri; NextState = nx }
        member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
        member this.SinglePathPointer = SinglePathPointer.Create this.Id


type RepeatExit = {
        Id          : StateId
        RepeatId    : RepeatId
        NextState   : StatePointer 
    }
    with
        static member Create id ri nx = { Id = id; RepeatId = ri; NextState = nx }
        member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
        member this.SinglePathPointer = SinglePathPointer.Create this.Id


type RepeatIterate = {
        Id          : StateId
        RepeatId    : RepeatId
        IterateState: StatePointer
        NextState   : StatePointer
    }
    with
        static member Create id it ri nx = { Id = id; RepeatId = ri; IterateState = it; NextState = nx }
        member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
        member this.SinglePathPointer = SinglePathPointer.Create this.Id



type RepeatState = {
        RepeatId  : RepeatId
        Iteration : int
        Min       : int
        Max       : int
    }
    with
        static member Create i n x = {RepeatId = i; Iteration = 0; Min = n; Max = x}
        member this.Iterate() = { this with Iteration = this.Iteration + 1}
        member this.CanExit() = this.Iteration >= this.Min
        member this.MustExit() = this.Iteration >= this.Max


type EmptyPath = {
        Id         : StateId
        NextState  : StatePointer 
    }
    with
        static member Create id mt nx = { Id = id; State = mt; NextState = nx }
        member this.LinkTo i = { this with NextState = i}
        member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
        member this.SinglePathPointer = SinglePathPointer.Create this.Id


type StateNode =
    |   SinglePath of SinglePath
    |   MultiPath  of MultiPath
    |   EmptyPath  of EmptyPath
    |   RepeatStart of RepeatStart
    |   RepeatExit of RepeatExit
    |   RepeatIterate of RepeatIterate
    with
        member this.Id 
            with get() =
                match this with
                |   SinglePath d -> d.Id
                |   MultiPath  d -> d.Id
                |   EmptyPath  d -> d.Id
                |   RepeatStart d -> d.Id
                |   RepeatExit  d -> d.Id
                |   RepeatIterate d -> d.Id

        member this.IsEmptyPathValue 
            with get() =
                match this with
                |   EmptyPath _ -> true
                |   _ -> false
         
        member this.NextState 
            with get() =
                match this with
                |   SinglePath d -> d.NextState.Id
                |   EmptyPath  d -> d.NextState.Id
                |   RepeatStart d -> d.NextState.Id
                |   RepeatExit  d -> d.NextState.Id
                |   RepeatIterate d -> d.NextState.Id
                |   MultiPath  _ -> failwith "Multipath has no single nextstate"

        member this.SetNextState i =
                match this with
                |   SinglePath d -> SinglePath { d with NextState = i }
                |   EmptyPath  d -> EmptyPath { d with NextState = i }
                |   RepeatExit  d -> RepeatExit  { d with NextState = i }
                |   RepeatIterate d -> RepeatIterate { d with NextState = i }
                |   _ -> failwith "Illegal to set nextstate"

        member this.SetNextState i =
                match this with
                |   RepeatStart d -> RepeatStart { d with NextState = i }
                |   _ -> this.SetNextState (i.StatePointer)

        member this.StatePointer  
            with get() =
                match this with
                |   SinglePath d -> d.StatePointer
                |   MultiPath  d -> d.StatePointer
                |   EmptyPath  d -> d.StatePointer
                |   RepeatStart d -> d.StatePointer
                |   RepeatExit  d -> d.StatePointer
                |   RepeatIterate d -> d.StatePointer

        member this.SinglePathPointer
            with get() =
                match this with
                |   SinglePath d -> d.SinglePathPointer
                |   EmptyPath  d -> d.SinglePathPointer
                |   RepeatStart d -> d.SinglePathPointer
                |   RepeatExit  d -> d.SinglePathPointer
                |   RepeatIterate d -> d.SinglePathPointer
                |   MultiPath  d -> failwith "Multipath has no SinglePathPointer"

let PointerToStateFinal = SinglePathPointer (SinglePathPointer.Create 0u) 


type NFAMachine = {
        Start   : StatePointer
        States  : StateNode list
        Repeats : RepeatState list
    }
    with
        static member Create (i, s, r) = { States = s; Start = i; Repeats = r}


let mutable currentId = 0u
let mutable currentRepeatId = 0u
let getNewId() =
    currentId <- (currentId + 1u)
    currentId

let getNewRepeatId() =
    currentRepeatId <- (currentRepeatId + 1u)
    RepeatId currentRepeatId


let qcOneInSet ls = ls |> List.fold(fun s i -> s ||| uint32(i)) 0u
    
let createSpp i = SinglePathPointer (SinglePathPointer.Create i)

let getSingle rgx =
    let id = getNewId() 
    let idsp = id |> createSpp
    match rgx with
    |   Plain       d ->
        idsp, [SinglePath (SinglePath.Create id (ExactMatch({ Char = d.``fixed``.[0]; ListCheck = d.Token |> List.map(uint32) })) PointerToStateFinal)], []
    |   OneInSet    d -> 
        let listCheck = d.Token'.Force() |> List.map(uint32) 
        let quickCheck = qcOneInSet listCheck
        idsp, [SinglePath (SinglePath.Create id (OneInSetMatch({ QuickCheck = quickCheck; ListCheck = listCheck})) PointerToStateFinal)], []
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


let getSinglePathValues sil =
    sil 
    |>  List.map(
        function
        |   SinglePath sp -> Some sp
        |   _  -> None
    )
    |>  List.filter(fun e -> e<>None)
    |>  List.map(fun e -> e.Value)  

let getExactMatchValues sil =
    sil
    |>  getSinglePathValues
    |>  List.filter(fun e -> 
        match e.State with
        |   ExactMatch _ -> true
        |   _ -> false
        )
    |>  List.map(fun e -> 
        let (ExactMatch ch) = e.State
        {| Char = ch; IdSp = e.SinglePathPointer; Next = e.NextState |}
    )   //  only exactmatch values 



let wsUpdate (n:StateNode) (ws:Map<StateId, StateNode>) = ws.Remove(n.Id).Add(n.Id, n)


let appendStateIdToAllFinalPathNodes2 (entryStartId:StatePointer) (entryStateList:StateNode list) (concatPtr:StatePointer) =
    let workingSet = entryStateList |> List.map(fun n -> n.Id, n) |> Map.ofList
    let passedNodes = Set.empty<StateId>
    
    let rec traverse (prev:StatePointer) (current:StatePointer) (workingSet:Map<StateId, StateNode>) (passedNodes:Set<StateId>) (concatPtr:StatePointer) =
        if  current.Id = 0u || passedNodes.Contains (current.Id) then workingSet 
        else
            let node = workingSet.[current.Id]
            match node with
            |   MultiPath  d -> 
                d.States 
                |> List.fold(fun ws stid -> traverse d.StatePointer (stid.StatePointer) ws (passedNodes.Add d.Id) concatPtr) workingSet
            |   EmptyPath  d -> 
                match workingSet.[prev.Id] with
                |   SinglePath sp -> 
                    let p = SinglePath({ sp with NextState = concatPtr})
                    wsUpdate p workingSet
                |   MultiPath  _ -> workingSet
                |   RepeatExit   re  -> 
                    let p = RepeatExit({ re with NextState = concatPtr})
                    wsUpdate p workingSet
                |   _ -> failwith "Not implemented yet"
            |   RepeatExit    d -> traverse d.StatePointer (d.NextState) workingSet (passedNodes.Add (d.Id)) concatPtr
            |   RepeatStart   d -> traverse d.StatePointer (d.NextState.StatePointer) workingSet (passedNodes.Add (d.Id)) concatPtr
            |   RepeatIterate d -> traverse d.StatePointer (d.NextState) workingSet (passedNodes.Add (d.Id)) concatPtr
            |   SinglePath d ->
                if d.NextState.Id = 0u then
                    let p = SinglePath({ d with NextState = concatPtr})
                    wsUpdate p workingSet
                else
                    traverse d.StatePointer (d.NextState) workingSet (passedNodes.Add (d.Id)) concatPtr

    traverse PointerToStateFinal entryStartId workingSet passedNodes concatPtr
    |> Map.toList |> List.map(snd)


let rec refactorCommonPlains (sil:SinglePathPointer list, snl:StateNode list) =
    let stMap = snl |> List.map(fun e -> e.Id, e) |> Map.ofList

    //  holds all plain (exact char matches) for which there are multiple occurences in ``sil``
    let stnl = 
        sil 
        |>  List.map(fun sp -> stMap.[sp.Id])
        |>  getSinglePathValues
        |>  List.filter(fun e -> 
            match e.State with
            |   ExactMatch _ -> true
            |   _ -> false
            )
        |>  List.map(fun e -> 
            let (ExactMatch ch) = e.State
            {| Char = ch; IdSp = e.SinglePathPointer; Next = e.NextState |}
        )   //  only exactmatch values 
        |>  List.groupBy(fun e -> e.Char)
        |>  List.filter(fun (ch,lst) -> List.length lst > 1)    // only with more than one occurrence (ie only ambigious situations)

    if stnl.Length = 0 then
        (sil, snl)
    else
        let rec refactorPlains (sil:SinglePathPointer list) (snl:StateNode list) (stnl:(ExactChar * {| Char : ExactChar; IdSp : SinglePathPointer; Next : StatePointer |} list) list) =
            match stnl with
            |   []  -> (sil, snl)
            |   hd :: tail ->
                let target = hd |> snd |> List.map(fun e -> (e.IdSp, e.Next))
                let primary = 
                    let id = fst target.Head
                    snl |>  List.find(fun e -> e.Id = id.Id)
                let (filterIds, nextIds) = target |> List.unzip
                let silNew, snlNew = 
                    let (siln, snln) = refactorCommonPlains (nextIds |> List.map(fun e -> e.SinglePathPointerValue), snl)
                    let snlnMap = snln |> List.map(fun e -> e.Id, e) |> Map.ofList
                    let filtIdSet = filterIds |> List.map(fun e -> e.Id) |> Set.ofList

                    let silNew = primary.SinglePathPointer :: (sil |> List.filter(fun e -> not(filtIdSet.Contains e.Id)))
                    if siln.Length = 1 then
                        silNew, (primary.SetNextState siln.Head) :: (snln |>  List.filter(fun e -> filterIds |> List.exists(fun x -> x.Id = e.Id) |> not))
                    else 
                        let isAllEmpty = 
                            siln 
                            |> List.forall(fun e -> 
                                let nd = snl |> List.find(fun sn -> sn.Id = e.Id)
                                nd.IsEmptyPathValue
                            )
                        if isAllEmpty then
                            let link = snl.Head
                            silNew, (primary.SetNextState link.StatePointer) :: link :: (snln |>  List.filter(fun e -> filterIds |> List.exists(fun x -> x.Id = e.Id) |> not))
                        else
                            let silnSorted =
                                siln
                                |>  List.map(fun sp -> snlnMap.[sp.Id])
                                |>  SortStateNodes
                                
                            let bundle = MultiPath(MultiPath.Create (getNewId()) (silnSorted|>List.map(fun sn -> sn.SinglePathPointer)))
                            silNew, (primary.SetNextState bundle.StatePointer) :: bundle :: (snln |>  List.filter(fun e -> filterIds |> List.exists(fun x -> x.Id = e.Id) |> not))
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


let removeTokenFromOneInSet cht (nodes:StateNode list) =
    nodes
    |>  List.map(fun n ->
        match n with
        |   SinglePath sp -> 
            let (OneInSetMatch ois) = sp.State
            let newLC = ois.ListCheck |> List.filter(fun t -> t <> cht)
            if newLC.Length > 0 then
                let newQC = qcOneInSet newLC
                SinglePath({ sp with State = (OneInSetMatch({ QuickCheck = newQC; ListCheck = newLC}))})
            else 
                EmptyPath({Id = 0u; NextState = PointerToStateFinal})
        |   _ -> failwith "Not implemented - this should never happen"
    )
    |>  List.filter(fun e -> not(e.IsEmptyPathValue))



let refactorConflictingCharacterSets (sil:SinglePathPointer list, snl:StateNode list) =
    let stMap = snl |> List.map(fun e -> e.Id, e) |> Map.ofList

    let sp = 
        sil 
        |>  List.map(fun sp -> stMap.[sp.Id])
        |>  getSinglePathValues

    let stnl =
        sp
        |>  List.filter(fun e -> 
            match e.State with
            |   OneInSetMatch _ -> true
            |   _ -> false
            )
        |>  List.map(fun e -> 
            let (OneInSetMatch sm) = e.State 
            sm.ListCheck
            |>  List.map(fun t -> {| Token = t; QuickCheck = sm.QuickCheck; IdSp = e.SinglePathPointer; Next = e.NextState |})
        )
        |>  List.collect(id)
        |>  List.filter(fun e -> nonGenericTokens |> Set.contains e.Token)
        |>  List.groupBy(fun e -> e.Token)
        |>  List.filter(fun (t, ls) -> List.length ls > 1)
    if List.length stnl = 0 then
        (sil, snl)
    else
        //  Combine duplicate tokens
        let rec refactorCharacterSets (sil:SinglePathPointer list) (snl:StateNode list) (stnl:(uint32*({| Token : uint32; QuickCheck : uint32; IdSp: SinglePathPointer; Next: StatePointer|} list)) list) =
            let stMap = snl |> List.map(fun e -> e.Id, e) |> Map.ofList
            match stnl with
            |   []  -> (sil, snl)
            |   (cht, lst) :: tail ->
                let toRemove, allNextIds =
                    lst
                    |>  List.map(fun e -> e.IdSp.Id, (e.Next.SinglePathPointerValue))
                    |>  List.unzip

                let allRefactoredOneInSet = 
                    let oisids = lst |> List.map(fun e -> e.IdSp)
                    let (nodes:StateNode list) = lst |> List.map(fun i -> stMap.[i.IdSp.Id])
                    removeTokenFromOneInSet cht nodes

                let snln =
                    snl 
                    |>  List.filter(fun e -> toRemove |> List.exists(fun x -> x = e.Id) |> not)
                    |>  List.append allRefactoredOneInSet

                let bundle = MultiPath(MultiPath.Create (getNewId()) allNextIds)
                let sp = SinglePath (SinglePath.Create (getNewId()) (OneInSetMatch({ ListCheck = [cht]; QuickCheck = cht})) (bundle.StatePointer))

                let silNew = 
                    sil 
                    |>  List.filter(fun i -> toRemove |> List.contains(i.Id) |> not)
                    |>  List.append (allRefactoredOneInSet |> List.map(fun i -> i.SinglePathPointer))
                let snlNew = sp :: bundle :: snln
                refactorCharacterSets (sp.SinglePathPointer :: silNew) snlNew tail
        refactorCharacterSets sil snl stnl


let refacorConflictingPlainWithCharacterSets (sil:SinglePathPointer list, snl:StateNode list) =
    let stMap = snl |> List.map(fun e -> e.Id, e) |> Map.ofList

    let sp = 
        sil 
        |>  List.map(fun id -> stMap.[id.Id])
        |>  getSinglePathValues

    let stnl =
        sp
        |>  List.filter(fun e -> 
            match e.State with
            |   OneInSetMatch _ -> true
            |   _ -> false
            )
        |>  List.map(fun e -> 
            let (OneInSetMatch sm) = e.State
            sm.ListCheck
            |>  List.map(fun t -> {| Token = t; QuickCheck = sm.QuickCheck; IdSp = e.StatePointer; Next = e.NextState |})
        )
        |>  List.collect(id)
        |>  List.filter(fun e -> nonGenericTokens |> Set.contains e.Token)
        |>  List.groupBy(fun e -> e.Token)

    // try to combine with exactchar
    let ec = 
        let targetTokens = stnl |> List.map(fst)
        sp
        |>  List.filter(fun e  -> 
            match e.State with
            |   ExactMatch ec -> ec.ListCheck.Length = 1 && ec.ListCheck |> List.exists(fun x -> targetTokens |> List.contains(x))
            |   _ -> false
        )
        |>  List.map(fun e -> 
            let (ExactMatch ch) = e.State
            {| Token = ch.ListCheck.Head; ExactChar = ch; IdSp = e.StatePointer; Next = e.NextState |})
        |>  List.groupBy(fun e -> e.Token)
        |>  List.fold(fun st (el,elst) ->
            let (cht, clst) = stnl |> List.find(fun (cht, lst) -> cht = el)
            (cht, clst, elst) :: st
        ) []
    if List.length ec = 0 then
        (sil, snl)
    else
        let rec refactorPlainsWithCharSets (sil:SinglePathPointer list) (snl:StateNode list) (stnl:(uint32*({| Token : uint32; QuickCheck : uint32; IdSp : StatePointer; Next : StatePointer |} list)*({| Token : uint32; ExactChar : ExactChar; IdSp :StatePointer; Next : StatePointer|} list)) list) =
            let stMap = snl |> List.map(fun e -> e.Id, e) |> Map.ofList
            match stnl with
            |   []  -> (sil, snl)
            |   (cht, clst, elst)  :: tail -> 
                let primary = elst |> List.head |> fun i -> stMap.[i.IdSp.Id]
                let toRemove, allNextIds =
                    clst
                    |>  List.map(fun e -> e.IdSp, e.Next.SinglePathPointerValue)
                    |>  List.append (elst |> List.map(fun e -> e.IdSp, e.Next.SinglePathPointerValue))
                    |>  List.distinct
                    |>  List.unzip

                let allRefactoredOneInSet = 
                    let (nodes:StateNode list) = clst |> List.map(fun e -> stMap.[e.IdSp.Id])
                    removeTokenFromOneInSet cht nodes

                let snln =
                    snl 
                    |>  List.filter(fun e -> toRemove |> List.exists(fun x -> x.Id = e.Id) |> not)
                    |>  List.append allRefactoredOneInSet

                let bundle = MultiPath(MultiPath.Create (getNewId()) allNextIds)

                let silNew = 
                    sil 
                    |>  List.filter(fun i -> toRemove |> List.exists(fun x -> x.Id = i.Id) |> not)
                    |>  List.append ( primary.SinglePathPointer :: (allRefactoredOneInSet |> List.map(fun i -> i.SinglePathPointer)))
                let snlNew = (primary.SetNextState bundle.StatePointer) :: bundle :: snln
                refactorPlainsWithCharSets silNew snlNew tail
        refactorPlainsWithCharSets sil snl ec


type RefactorResult =
    |   Refactored of StatePointer
    |   Unrefactored of StatePointer * StatePointer

let refacorRepeater (start:StatePointer, nodes:StateNode list, repeaters) =
    let stMap = nodes |> List.map(fun e -> e.Id, e) |> Map.ofList

    let rec refactorPlains (iterPtr : StatePointer) (nextPtr : StatePointer) (exitPtr:StatePointer) (stMap:Map<StateId, StateNode>) =
        let sps =
            [stMap.[iterPtr.Id]; stMap.[nextPtr.Id]]
            |>  getExactMatchValues
        if(sps.Length<2) then
            Unrefactored(iterPtr, nextPtr), stMap
        else
            let [EMIter; EMNxt] = sps
            match (EMIter.Char = EMNxt.Char) with
            |   false ->   Unrefactored(iterPtr, nextPtr), stMap
            |   true  ->
                let (refactored,stMapNew) = refactorPlains (EMIter.Next) (EMNxt.Next) exitPtr stMap
                match refactored with
                |   Refactored single    ->  //  was refactored
                    let ndIter = stMapNew.[EMIter.IdSp.Id].SetNextState single
                    Refactored(ndIter.StatePointer), stMapNew.Remove(EMNxt.IdSp.Id) |> wsUpdate ndIter
                |   Unrefactored (da,db)     ->  //  was not refactored
                    let newExit = stMapNew.[exitPtr.Id].SetNextState db
                    let bundle = MultiPath(MultiPath.Create (getNewId()) [da.SinglePathPointerValue;newExit.SinglePathPointer])
                    let ndIter = stMapNew.[EMIter.IdSp.Id].SetNextState bundle.StatePointer
                    Refactored(ndIter.StatePointer), stMapNew.Add(bundle.Id, bundle) |> wsUpdate ndIter |> wsUpdate newExit


    let refactor (ri:RepeatIterate) =
        match stMap.[ri.NextState.Id] with
        |   RepeatExit re ->
            if not(ri.IterateState.IsSinglePath && re.NextState.IsSinglePath) then
                (start, nodes, repeaters)
            else
                let (refac,stNew) = refactorPlains (ri.IterateState.StatePointer) (re.NextState.StatePointer) (re.StatePointer) stMap
                match refac with
                |   Refactored single    ->  //  was refactored
                    let p = RepeatIterate { ri with IterateState = single.StatePointer; NextState = single.StatePointer }
                    let nodes = stNew |> wsUpdate p |> Map.toList |> List.map(snd)
                    (start, nodes, repeaters)
                |   Unrefactored (da,db)     ->  //  was not refactored
                    (start, nodes, repeaters)
        | _ -> (start, nodes, repeaters)

    match stMap.[start.Id] with
    |   RepeatStart d -> 
        let (RepeatIterate ri) = stMap.[d.NextState.Id]
        refactor ri
    |   _ -> (start, nodes, repeaters)


let removeUnused (nfa:NFAMachine) =
    let stMap = nfa.States |> List.map(fun e -> e.Id, e) |> Map.ofList

    let rec traverse (current:StatePointer) (passedNodes:Set<StateId>) (used:StateId list) =
        if  current.Id = 0u || passedNodes.Contains (current.Id) then current.Id::used
        else
            let node = stMap.[current.Id]
            match node with
            |   SinglePath sp -> traverse (sp.NextState) (passedNodes.Add (sp.Id)) (sp.Id::used)
            |   MultiPath  mp -> 
                let newPassed = passedNodes.Add(mp.Id)
                mp.States 
                |> List.fold(fun u stid -> traverse stid.StatePointer newPassed u) (mp.Id::used)
            |   EmptyPath  ep -> traverse (ep.NextState) (passedNodes.Add (ep.Id)) (ep.Id::used)
            |   RepeatStart rs -> traverse (rs.NextState.StatePointer) (passedNodes.Add (rs.Id)) (rs.Id::used)
            |   RepeatIterate ri -> 
                traverse (ri.IterateState) (passedNodes.Add (ri.Id)) (ri.Id::used) 
                |>  traverse (ri.NextState) (passedNodes.Add (ri.Id))
            |   RepeatExit   re  -> traverse (re.NextState) (passedNodes.Add (re.Id)) (re.Id::used) 
    let usedLst =
        traverse nfa.Start Set.empty []
        |>  List.distinct
        |>  Set.ofList
    let nodes = nfa.States |> List.filter(fun n -> usedLst.Contains n.Id)
    {nfa with States = nodes}


let rgxToNFA rgx =
    currentId <- 0u
    currentRepeatId <- 0u
    let rec processConversion rgx : NFAMachine =
        let convert rg : NFAMachine =
            match rg with
            |   Plain pl   -> if pl.``fixed``.Length > 1 then processConversion rg else getSingle rg |> NFAMachine.Create
            |   OneInSet _ -> getSingle rg |> NFAMachine.Create
            |   _ -> processConversion rg
            
        let emptyState() = EmptyPath({ Id = getNewId(); NextState = PointerToStateFinal})

        match rgx with
        |   Plain  pl ->
            if pl.``fixed``.Length > 1 then
                pl.OptimizeOnce()
                processConversion (Concat (pl.optimized))
            else
                failwith "Uncontained plain - not implemented yet"
        |   Concat l -> 
            let linkState = emptyState()
            let converts =
                l
                |>  List.map(convert)
            let nodes = converts |> List.map(fun c -> c.States) |> List.collect id
            let repeats = converts |> List.map(fun c -> c.Repeats) |> List.collect id

            converts
            |>  List.map(fun c -> c.Start)
            |>  List.fold(fun (concatPtr:StatePointer, nodes:StateNode list, r) (entryStartId:StatePointer) ->
                    let newNodes = appendStateIdToAllFinalPathNodes2 entryStartId nodes concatPtr
                    (entryStartId, newNodes, repeats)
                    |>  refacorRepeater
                    ) (linkState.StatePointer, linkState::nodes, repeats)
            |>  NFAMachine.Create
        |   Or     l -> 
            l
            |>  List.map(convert)
            |>  List.fold(fun (sil, snl) nfa ->
                    let mpl = 
                        nfa.States
                        |>  List.find(fun e -> e.Id = nfa.Start.Id)
                        |>  function
                            |   MultiPath  mp -> mp.States
                            |   _             -> []
                    ((nfa.Start.SinglePathPointerValue :: sil) @ mpl), (snl @ nfa.States)
            ) ([],[])
            |>  refactorConflictingCharacterSets
            |>  refacorConflictingPlainWithCharacterSets
            |>  refactorCommonPlains
            |>  fun (idlst, snlst) -> 
                if idlst.Length = 1 then
                    let id = idlst.Head
                    id.StatePointer, snlst, []
                else
                    let id = getNewId()
                    (MultiPathPointer.Create id).StatePointer, MultiPath(MultiPath.Create id (idlst)) :: snlst, []
            |>  NFAMachine.Create
        |   Optional    o ->
            let linkState = emptyState()
            let repPath = convert o
            let repState = RepeatState.Create (getNewRepeatId()) 0 1
            let repExit = RepeatExit({Id = getNewId(); RepeatId = repState.RepeatId; NextState = linkState.StatePointer})
            let repItr = RepeatIterate({Id = getNewId(); RepeatId = repState.RepeatId; IterateState = repPath.Start; NextState = repExit.StatePointer})
            
            let repeatedStates = appendStateIdToAllFinalPathNodes2 repPath.Start repPath.States repItr.StatePointer

            let repStart = RepeatStart({ Id = getNewId(); RepeatId = repState.RepeatId; NextState = repItr.SinglePathPointer})
            NFAMachine.Create (repStart.StatePointer, repStart :: repItr :: repExit :: linkState :: repeatedStates, repState :: repPath.Repeats)

        |   _ -> failwith "Not Implemented Yet"
    processConversion rgx
    |>  removeUnused


type ParseResult = {
    IsMatch     : bool
    FullMatch   : char list
}


type LevelType = 
    |   Concat = 0
    |   Multi   = 1
    |   RepeatIter = 4
    |   RepeatExit = 2
    |   LoopStart = 3


let PrintIt (nfa:NFAMachine) =
    let stMap = nfa.States |> List.map(fun e -> e.Id,e) |> Map.ofList
    let rsMap = nfa.Repeats |> List.map(fun e -> e.RepeatId,e) |> Map.ofList
    let passedNodes = Set.empty<StateId>

    let rec printLine (hist : LevelType list) (current: StatePointer) (passedNodes:Set<StateId>) =
        hist
        |>  List.rev
        |>  List.iter(fun i ->
            match i with
            |   LevelType.Concat    -> printf "         "
            |   LevelType.Multi     -> printf "|    "
            |   LevelType.RepeatExit-> printf " |X>>--"
            |   LevelType.RepeatIter-> printf " |I    "
            |   LevelType.LoopStart -> printf "               "
        )

        let rec printLineRest (hist : LevelType list) (current: StatePointer) (passedNodes:Set<StateId>) =
            if current.Id = 0u then
                printf "-*\n"
            else
                match stMap.[current.Id] with
                |   EmptyPath  ep   ->  
                    printf "~"
                    printLineRest (LevelType.Concat :: hist) ep.NextState (passedNodes.Add ep.Id)
                |   SinglePath sp   -> 
                    match sp.State with
                    |   ExactMatch c    -> printf "-(%2d:\"%s\")" sp.Id (Regex.Escape(c.Char.ToString()))
                    |   OneInSetMatch o -> printf "-(%2d:[@])" sp.Id
                    printLineRest (LevelType.Concat :: hist) sp.NextState (passedNodes.Add sp.Id)
                |   MultiPath mp    ->
                    let h::t = mp.States
                    printf "|(%2d)" mp.Id
                    printLineRest (LevelType.Multi :: hist) h.StatePointer (passedNodes.Add mp.Id)
                    t |> List.iter(fun e -> printLine (LevelType.Multi :: hist) e.StatePointer passedNodes)
                |   RepeatStart rs ->
                    let rt = rsMap.[rs.RepeatId]
                    printf "->>(%2d:<%2d,%2d>)" rs.Id rt.Min rt.Max
                    printLineRest (LevelType.LoopStart :: hist) (rs.NextState.StatePointer) (passedNodes.Add rs.Id)
                |   RepeatIterate ri ->
                    if not(passedNodes.Contains ri.Id) then
                        printf "-|I(%2d)" ri.Id
                        printLineRest (LevelType.RepeatIter :: hist) ri.IterateState (passedNodes.Add ri.Id)
                        printLine (LevelType.RepeatExit :: hist) ri.NextState.StatePointer (passedNodes.Add ri.Id)
                    else
                        printf "-Next(%2d)\n" ri.Id
                |   RepeatExit re ->
                    printf "-Exit(%2d)" re.Id
                    printLineRest (LevelType.Concat :: hist) re.NextState (passedNodes.Add re.Id)

        printLineRest hist current passedNodes
    printLine [] nfa.Start passedNodes


let parseIt (nfa:NFAMachine) yaml =
    let stMap = nfa.States |> List.fold(fun (m:Map<_,_>) i -> m.Add(i.Id, i)) Map.empty<StateId, StateNode>
    let stRepeat = nfa.Repeats |> List.fold(fun (m:Map<_,_>) i -> m.Add(i.RepeatId, i)) Map.empty<RepeatId, RepeatState>
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
    let runningLoops = Map.empty<RepeatId, RepeatState>


    let rec processStr cs acc rollback (runningLoops : Map<RepeatId, RepeatState>) =
        let NoMatch = { IsMatch = false ; FullMatch = [] }
        if cs = PointerToStateFinal then
            { IsMatch = true; FullMatch = acc |> List.rev }
        else
            let st = stMap.[cs.Id]
            match st with
            |   SinglePath p ->
                let nxt = p.NextState
                let chk = stream.Get()
                if (p.State.Match chk) then
                    processStr nxt (chk.Source.[0] :: acc) rollback runningLoops
                else 
                    NoMatch
            |   MultiPath p ->
                let chk = stream.Get()
                p.States
                |>  List.tryFind(fun t -> 
                    match stMap.[t.Id] with
                    |   SinglePath st -> st.State.Match chk
                    |   EmptyPath  _  -> true
                    |   RepeatExit _  -> true
                    |   _ -> failwith "Not implemented yet"
                    )
                |>  function
                    |   Some v -> 
                        match stMap.[v.Id] with
                        |   EmptyPath  st -> processStr (st.NextState) acc (rollback+1) runningLoops
                        |   SinglePath st -> processStr (st.NextState) (chk.Source.[0] :: acc) rollback runningLoops
                        |   MultiPath  _  -> failwith "Not implemented yet"
                        |   RepeatExit re -> 
                            stream.Position <- stream.Position - 1
                            processStr re.NextState acc rollback runningLoops
                    |   None -> NoMatch 
            |   EmptyPath p -> processStr p.NextState acc rollback runningLoops
            |   RepeatStart r ->
                let rlnew =
                    if runningLoops.ContainsKey r.RepeatId then
                        runningLoops.Remove(r.RepeatId).Add(r.RepeatId, stRepeat.[r.RepeatId])
                    else
                        runningLoops.Add(r.RepeatId, stRepeat.[r.RepeatId])
                processStr r.NextState.StatePointer acc rollback rlnew
            |   RepeatIterate r ->
                let rs = runningLoops.[r.RepeatId]
                if rs.MustExit() then
                    processStr r.NextState acc rollback runningLoops
                else
                    let rlNew = runningLoops.Remove(r.RepeatId).Add(r.RepeatId, rs.Iterate())
                    let tryIterate = processStr r.IterateState acc rollback rlNew
                    if r.IterateState<>r.NextState && not(tryIterate.IsMatch) then
                        stream.Position <- stream.Position - 1  // rollback should be exactly 1, all simlarities in both paths should be refactored away
                        processStr r.NextState acc rollback rlNew
                    else
                        tryIterate
            |   RepeatExit r ->
                if runningLoops.[r.RepeatId].CanExit() then
                    processStr r.NextState acc rollback (runningLoops.Remove(r.RepeatId))
                else
                    NoMatch

    processStr nfa.Start [] 0 runningLoops

let clts (cl:char list) = System.String.Concat(cl)

module ParseResult =
    let IsMatch pr = pr.IsMatch
    let FullMatch pr = pr.FullMatch


