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
        ListCheck  : Token list
    }


type OneOfChar = {
        QuickCheck : uint32
        ListCheck  : Token list
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
                    sm.ListCheck |> List.exists(fun e -> e=t.Token)
                else
                    (sm.QuickCheck &&& uint32(t.Token) > 0u)


type StateId = System.UInt32                


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


let mutable currentId = 0u
let mutable currentRepeatId = 0u
let CreateNewId() =
    currentId <- (currentId + 1u)
    currentId

let CreateNewRepeatId() =
    currentRepeatId <- (currentRepeatId + 1u)
    RepeatId currentRepeatId


type SinglePath = {
        Id         : StateId
        State      : SingleCharMatch
        NextState  : StatePointer 
    }
    with
        static member Create mt nx = { Id = CreateNewId(); State = mt; NextState = nx }
        member this.LinkTo i = { this with NextState = i}
        member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
        member this.SinglePathPointer = SinglePathPointer.Create this.Id


type MultiPath = {
        Id         : StateId
        States     : SinglePathPointer list
    }
    with
        static member Create mt = { Id = CreateNewId(); States = mt }
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


type RepeatIterateOrExit = {
        Id          : StateId
        RepeatId    : RepeatId
        IterateState: StatePointer
        NextState   : StatePointer 
    }
    with
        static member Create id it ri nx = { Id = id; IterateState = it; RepeatId = ri; NextState = nx }
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
        member this.CanExit() = this.Iteration >= this.Min && (this.Max <= 0 || this.Iteration < this.Max)
        member this.MustExit() = this.Max > 0 && this.Iteration >= this.Max


type EmptyPath = {
        Id         : StateId
        NextState  : StatePointer 
    }
    with
        static member Create id nx = { Id = id; NextState = nx }
        member this.LinkTo i = { this with NextState = i}
        member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
        member this.SinglePathPointer = SinglePathPointer.Create this.Id

let PointerToStateFinal = SinglePathPointer (SinglePathPointer.Create 0u) 


type StateNode =
    |   SinglePath of SinglePath
    |   MultiPath  of MultiPath
    |   EmptyPath  of EmptyPath
    |   RepeatStart of RepeatStart
    |   RepeatIterOrExit of RepeatIterateOrExit
    |   NoMatch of StateId
    with
        member this.Id 
            with get() =
                match this with
                |   SinglePath d -> d.Id
                |   MultiPath  d -> d.Id
                |   EmptyPath  d -> d.Id
                |   RepeatStart d -> d.Id
                |   RepeatIterOrExit  d -> d.Id
                |   NoMatch d -> d

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
                |   RepeatIterOrExit  d -> d.NextState.Id
                |   MultiPath  _ -> failwith "Multipath has no single nextstate"
                |   NoMatch _ -> failwith "NoMatch has no single nextstate"

        member this.NextStatePtr 
            with get() =
                match this with
                |   SinglePath d -> d.NextState
                |   EmptyPath  d -> d.NextState
                |   RepeatStart d -> d.NextState.StatePointer
                |   RepeatIterOrExit  d -> d.NextState
                |   MultiPath  _ -> failwith "Multipath has no single nextstate"
                |   NoMatch _ -> failwith "NoMatch has no single nextstate"


        member this.SetNextState i =
                match this with
                |   SinglePath d -> SinglePath { d with NextState = i }
                |   EmptyPath  d -> EmptyPath { d with NextState = i }
                |   RepeatIterOrExit  d -> RepeatIterOrExit  { d with NextState = i }
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
                |   RepeatIterOrExit  d -> d.StatePointer
                |   NoMatch d -> SinglePathPointer (SinglePathPointer.Create d)

        member this.SinglePathPointer
            with get() =
                match this with
                |   SinglePath d -> d.SinglePathPointer
                |   EmptyPath  d -> d.SinglePathPointer
                |   RepeatStart d -> d.SinglePathPointer
                |   RepeatIterOrExit  d -> d.SinglePathPointer
                |   MultiPath  d -> failwith "Multipath has no SinglePathPointer"
                |   NoMatch d -> SinglePathPointer.Create d


type NFAMachine = {
        Start   : StatePointer
        States  : StateNode list
        Repeats : RepeatState list
    }
    with
        static member Create (i, s, r) = { States = s; Start = i; Repeats = r}


let qcOneInSet ls = ls |> List.fold(fun s i -> s ||| uint32(i)) 0u
    
let createSpp i = SinglePathPointer (SinglePathPointer.Create i)

let createSinglePathFromRgx rgx =
    match rgx with
    |   Plain       d ->
        let sp = SinglePath.Create (ExactMatch({ Char = d.``fixed``.[0]; ListCheck = d.Token })) PointerToStateFinal
        sp.StatePointer, [SinglePath sp], []
    |   OneInSet    d -> 
        let listCheck = d.Token'.Force()
        let quickCheck = qcOneInSet listCheck
        let sp = SinglePath.Create (OneInSetMatch({ QuickCheck = quickCheck; ListCheck = listCheck})) PointerToStateFinal
        sp.StatePointer, [SinglePath sp], []
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


let nonGenericTokens =
    [   Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark``; Token.``t-colon``;
        Token.``t-dot``; Token.``t-square-bracket-start``; Token.``t-square-bracket-end``; Token.``t-curly-bracket-start``; Token.``t-curly-bracket-end``;
        Token.``t-hash``; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``; Token.``t-gt``;
        Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``; Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``;
        Token.``ns-dec-digit``; Token.``c-escape``; Token.``t-comma``]
    |>   Set.ofList



type NormalizedExactChar = { Token : Token; IdSp : StatePointer; Next : StatePointer ; ExactChar : char }
type NormalizedOneInSet  = { Token : Token; IdSp : StatePointer; Next : StatePointer }

type NormalizedForRefactoring =
    |   ExactMatchItem of NormalizedExactChar
    |   OneInSetItem of NormalizedOneInSet


let normalizeForRefactoring (stNodes:StateNode list) =
    stNodes
    |>  List.fold(fun s e ->
        match e with
        |   SinglePath sp -> 
            match sp.State with
            |   ExactMatch ch       ->  [ExactMatchItem { Token = ch.ListCheck.Head; IdSp = sp.StatePointer; Next = sp.NextState ; ExactChar = ch.Char}]
            |   OneInSetMatch ois   ->  
                ois.ListCheck
                |>  List.map(fun t  ->   OneInSetItem { Token = t; IdSp = sp.StatePointer; Next = sp.NextState })
            :: s
        |   _  -> s
    ) []
    |>  List.collect id


type RefactoringCanditate =
    |   PlainMerge          of char  * (NormalizedExactChar list)
    |   OneInSetOverlap     of Token * (NormalizedOneInSet list)
    |   CrossTypeOverlap    of Token * (NormalizedExactChar list) * (NormalizedOneInSet list)


let getPlainMerges (lst:NormalizedForRefactoring list) =
    lst
    |>  List.fold(fun s e -> 
        match e with
        |   ExactMatchItem e -> e :: s
        |   _ -> s
    ) []
    |>  List.groupBy(fun e -> e.ExactChar)


let getOneInSetOverlap (lst:NormalizedForRefactoring list) =
    lst
    |>  List.fold(fun s e ->
        match e with
        |   OneInSetItem e -> e :: s
        |   _ -> s
    ) []
    |>  List.groupBy(fun e -> e.Token)


let getCrossTypeOverlap (lst:NormalizedForRefactoring list) =
    lst
    |>  List.groupBy(fun e ->
        match e with
        |   ExactMatchItem i -> i.Token
        |   OneInSetItem   i -> i.Token
    )
    |>  List.filter(fun (t, _) -> nonGenericTokens |> Set.contains t)
    |>  List.fold(fun s (t, nlst) ->
        let (ecl, osl) =
            nlst
            |>  List.fold(fun (s1,s2) e ->
                match e with
                |   ExactMatchItem d -> (d::s1, s2)
                |   OneInSetItem   d -> (s1, d::s2)
            ) ([],[])
        (t,ecl, osl) :: s
    ) []


let wsUpdate (n:StateNode) (ws:Map<StateId, StateNode>) = ws.Remove(n.Id).Add(n.Id, n)
let wsAdd (n:StateNode) (ws:Map<StateId, StateNode>) = ws.Add(n.Id, n)


let appendStateIdToAllFinalPathNodes (entryStartId:StatePointer) (entryStateList:StateNode list) (concatPtr:StatePointer) =
    let workingSet = entryStateList |> List.map(fun n -> n.Id, n) |> Map.ofList
    let passedNodes = Set.empty<StateId>
    
    let rec traverse (prev:StatePointer) (current:StatePointer) (workingSet:Map<StateId, StateNode>) (passedNodes:Set<StateId>) (concatPtr:StatePointer) =
        if  current.Id = 0u || current.Id = concatPtr.Id || passedNodes.Contains (current.Id) then workingSet 
        else
            let node = workingSet.[current.Id]
            match node with
            |   MultiPath  d -> 
                d.States 
                |> List.fold(fun ws stid -> traverse d.StatePointer (stid.StatePointer) ws (passedNodes.Add d.Id) concatPtr) workingSet
            |   EmptyPath  d  when d.NextState.Id = PointerToStateFinal.Id -> 
                match workingSet.[prev.Id] with
                |   SinglePath sp -> 
                    let p = SinglePath({ sp with NextState = concatPtr})
                    wsUpdate p workingSet
                |   MultiPath  _ -> workingSet
                |   RepeatIterOrExit   re  -> 
                    let p = RepeatIterOrExit({ re with NextState = concatPtr})
                    wsUpdate p workingSet
                |   _ -> failwith "Not implemented yet"
            |   EmptyPath     d -> traverse d.StatePointer (d.NextState) workingSet (passedNodes.Add (d.Id)) concatPtr
            |   RepeatIterOrExit    d -> traverse d.StatePointer (d.NextState) workingSet (passedNodes.Add (d.Id)) concatPtr
            |   RepeatStart   d -> traverse d.StatePointer (d.NextState.StatePointer) workingSet (passedNodes.Add (d.Id)) concatPtr
            |   SinglePath d ->
                if d.NextState.Id = 0u then
                    let p = SinglePath({ d with NextState = concatPtr})
                    wsUpdate p workingSet
                else
                    traverse d.StatePointer (d.NextState) workingSet (passedNodes.Add (d.Id)) concatPtr
            |   NoMatch _ -> workingSet
    traverse PointerToStateFinal entryStartId workingSet passedNodes concatPtr
    |> Map.toList |> List.map(snd)


let rec refactorCommonPlains (sil:SinglePathPointer list, snl:StateNode list) =
    let stMap = snl |> List.map(fun e -> e.Id, e) |> Map.ofList

    //  holds all plain (exact char matches) for which there are multiple occurences in ``sil``
    let stnl = 
        sil 
        |>  List.map(fun sp -> stMap.[sp.Id])
        |>  normalizeForRefactoring
        |>  getPlainMerges
        |>  List.filter(fun (_,lst) -> List.length lst > 1)    // only with more than one occurrence (ie only ambigious situations)

    if stnl.Length = 0 then
        (sil, snl)
    else
        let rec refactorPlains (sil:SinglePathPointer list) (snl:StateNode list) (stnl:(char * (NormalizedExactChar list)) list) =
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
                                
                            let bundle = MultiPath(MultiPath.Create (silnSorted|>List.map(fun sn -> sn.SinglePathPointer)))
                            silNew, (primary.SetNextState bundle.StatePointer) :: bundle :: (snln |>  List.filter(fun e -> filterIds |> List.exists(fun x -> x.Id = e.Id) |> not))
                refactorPlains silNew snlNew tail
        refactorPlains sil snl stnl
   


let removeTokenFromOneInSet (chtLst: Token list) (nodes:StateNode list) =
    let tks = Set.ofList chtLst
    nodes
    |>  List.map(fun n ->
        match n with
        |   SinglePath sp -> 
            let (OneInSetMatch ois) = sp.State
            let newLC = ois.ListCheck |> List.filter(fun t -> not(tks.Contains t))
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

    let stnl =
        sil
        |>  List.map(fun sp -> stMap.[sp.Id])
        |>  normalizeForRefactoring
        |>  getOneInSetOverlap
        |>  List.filter(fun (t, ls) -> List.length ls > 1)

    if List.length stnl = 0 then
        (sil, snl)
    else
        //  Combine duplicate tokens
        let rec refactorCharacterSets (sil:SinglePathPointer list) (snl:StateNode list) (stnl:(Token*(NormalizedOneInSet list)) list) =
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
                    removeTokenFromOneInSet [cht] nodes

                let snln =
                    snl 
                    |>  List.filter(fun e -> toRemove |> List.exists(fun x -> x = e.Id) |> not)
                    |>  List.append allRefactoredOneInSet

                let bundle = MultiPath(MultiPath.Create allNextIds)
                let sp = SinglePath (SinglePath.Create (OneInSetMatch({ ListCheck = [cht]; QuickCheck = uint32(cht)})) (bundle.StatePointer))

                let silNew = 
                    sil 
                    |>  List.filter(fun i -> toRemove |> List.contains(i.Id) |> not)
                    |>  List.append (allRefactoredOneInSet |> List.map(fun i -> i.SinglePathPointer))
                let snlNew = sp :: bundle :: snln
                refactorCharacterSets (sp.SinglePathPointer :: silNew) snlNew tail
        refactorCharacterSets sil snl stnl


let refacorConflictingPlainWithCharacterSets (sil:SinglePathPointer list, snl:StateNode list) =
    let stMap = snl |> List.map(fun e -> e.Id, e) |> Map.ofList

    let stnl =
        sil 
        |>  List.map(fun id -> stMap.[id.Id])
        |>  normalizeForRefactoring
        |>  getCrossTypeOverlap
        |>  List.filter(fun (_, l1, l2) -> l1.Length > 0 && l2.Length >0)

    if List.length stnl = 0 then
        (sil, snl)
    else
        let rec refactorPlainsWithCharSets (sil:SinglePathPointer list) (snl:StateNode list) (stnl:(Token*(NormalizedExactChar list)*(NormalizedOneInSet list)) list) =
            let stMap = snl |> List.map(fun e -> e.Id, e) |> Map.ofList
            match stnl with
            |   []  -> (sil, snl)
            |   (cht, elst, clst)  :: tail -> 
                let primary = elst |> List.head |> fun i -> stMap.[i.IdSp.Id]
                let toRemove, allNextIds =
                    clst
                    |>  List.map(fun e -> e.IdSp, e.Next.SinglePathPointerValue)
                    |>  List.append (elst |> List.map(fun e -> e.IdSp, e.Next.SinglePathPointerValue))
                    |>  List.distinct
                    |>  List.unzip

                let allRefactoredOneInSet = 
                    let (nodes:StateNode list) = clst |> List.map(fun e -> stMap.[e.IdSp.Id])
                    removeTokenFromOneInSet [cht] nodes

                let snln =
                    snl 
                    |>  List.filter(fun e -> toRemove |> List.exists(fun x -> x.Id = e.Id) |> not)
                    |>  List.append allRefactoredOneInSet

                let bundle = MultiPath(MultiPath.Create allNextIds)

                let silNew = 
                    sil 
                    |>  List.filter(fun i -> toRemove |> List.exists(fun x -> x.Id = i.Id) |> not)
                    |>  List.append ( primary.SinglePathPointer :: (allRefactoredOneInSet |> List.map(fun i -> i.SinglePathPointer)))
                let snlNew = (primary.SetNextState bundle.StatePointer) :: bundle :: snln
                refactorPlainsWithCharSets silNew snlNew tail
        refactorPlainsWithCharSets sil snl stnl


type RefactorResult =
    |   Refactored of StatePointer
    |   Unrefactored 

let refacorRepeater (start:StatePointer, nodes:StateNode list, repeaters) =
    //  Repeat is used for option, zero-or-more, once-or-more and repeat-between-min-and-max.
    //  Repeat has two outgoing paths: iteration, and exit.
    //  When iter and exit paths have something in common, it is difficult to determine
    //  in which path the parser is supposed to be. To solve this, the commonalties are "joined" together in both paths
    //  and where the paths split, the choice is made to continue the iter-path, or follow the exit path.
    //
    //  The exit-paths must have a "Repeat-Exit" state, which during parsing, checks the "min" and "max" constraint 
    //  in a range-repeat and removes the repeater from the "running loops" list.
    let stMap = nodes |> List.map(fun e -> e.Id, e) |> Map.ofList

    let rec refactorRepeaterRec (iterPtr : StatePointer) (nextPtr : StatePointer) (exitPtr:StatePointer) (repeatId:RepeatId) (stMap:Map<StateId, StateNode>) : RefactorResult * Map<StateId, StateNode> =
        let NextStp (sp:StatePointer) = stMap.[stMap.[sp.Id].NextState].StatePointer
        
        let lst =
            [iterPtr; nextPtr]
            |>  List.map(fun e -> stMap.[e.Id])
            |>  normalizeForRefactoring

        let refactorPlainvsOneInSetOverlap mlst =
            getCrossTypeOverlap mlst
            |>  List.filter(fun (_, ncl, nol) -> ncl.Length > 0 && nol.Length > 0)
            |>  function
            |   []      -> Unrefactored, stMap
            |   [oislst] -> 
                let EMIter = NextStp iterPtr 
                let EMNxt  = NextStp nextPtr

                let (refactored,stMapNew) = refactorRepeaterRec (EMIter) (EMNxt) exitPtr repeatId stMap
                match refactored with
                |   Refactored single    ->  //  was refactored
                    //  Already in a refactoring chain.
                    //  The decision whether the current state chooses the iter- or exit-path of the repeat comes later.
                    //  Here is assumed that part has been dealt with, later in the chain.
                    //  Here we can join all the options into one MultiPath.
                    //  Because the next of both iter- and next-paths point to "single", this could be
                    //  created by storing current iter- and next-step in the multipath, without cleaning up the
                    //  overlap. However cleanup may benefit us later (don't know yet), and it is what these funcs are
                    //  all about. The overlap between an ExactChar and OneInSet token, is moved completely to the ExactChar
                    //  and the OneInSet is cleared of this token.
                    let (allTokens, allExactChar, allOneInSet) = oislst

                    let ec = allExactChar.Head
                    let ois = allOneInSet.Head
                    let oisSt = stMapNew.[ois.IdSp.Id]
                    let oisCl = removeTokenFromOneInSet [allTokens] [oisSt] |> List.head

                    let bundle = MultiPath(MultiPath.Create [ec.IdSp.SinglePathPointerValue; oisCl.SinglePathPointer])

                    let stmr = 
                        stMapNew
                        |>  Map.add bundle.Id bundle
                        |>  wsUpdate oisCl
                    Refactored(bundle.StatePointer), stmr

                |   Unrefactored    ->  //  was not refactored
                    //  Here is the start of a refactoring chain.
                    //  Two state-types are relevant after the refactoring of this point:
                    //  1 - ExactChar which takes over the OneInSet common-token
                    //  2 - OneInSet which lost the overlapping Token 
                    //
                    //  So this:    (scenario 1)
                    //  Repeat Start -> |I -> A    -> D -> Next()
                    //                  |X -> [AC] -> G -> 2
                    //
                    //  Becomes this:
                    //  Repeat Start -> |A -> |I -> D -> Next()
                    //                        |X -> G -> 2
                    //                  |[C] -> G -> 2              (match [C] sure exits loop)
                    //                   
                    //
                    //  And this:   (scenario 2)
                    //  Repeat Start -> |I -> [AC] -> D -> Next()
                    //                  |X -> A    -> G -> 2
                    //
                    //  Becomes this:
                    //  Repeat Start -> |[C]  -> |I -> D -> Next()   RepeatIterOrExit is Required to check iteration-constraints (min/max).
                    //                  |        |X -> NoMatch       ie. string "CDCY" does not match, while "CDCD.." match depends on iteration constraints
                    //                  |A    -> |I -> D -> Next()
                    //                           |X -> G -> 2


                    let (allOverlappingTokens, allExactChar, allOneInSet) = oislst

                    let ec     = stMap.[allExactChar.Head.IdSp.Id]
                    let oisOld = stMap.[allOneInSet.Head.IdSp.Id]
                    
                    let cleanedStates = removeTokenFromOneInSet [allOverlappingTokens] [oisOld]
                    let oisCleaned = cleanedStates |> List.find(fun i -> i.Id = oisOld.Id)

                    let iterNext = stMap.[iterPtr.Id].NextStatePtr
                    let exitNext = stMap.[nextPtr.Id].NextStatePtr

                    let newExit = RepeatIterateOrExit.Create (CreateNewId()) iterNext repeatId exitNext |> RepeatIterOrExit
                    let ecNew = ec.SetNextState newExit.StatePointer


                    if iterPtr.Id = ec.Id then  // scenario 1

                        let bundle = MultiPath(MultiPath.Create [ecNew.SinglePathPointer; oisCleaned.SinglePathPointer])

                        let stMapRet =
                            stMapNew
                            |>  Map.add bundle.Id bundle
                            |>  Map.add newExit.Id newExit
                            |>  wsUpdate ecNew
                            |>  wsUpdate oisCleaned
                        Refactored(bundle.StatePointer), stMapRet
                    else    // scenario 2
                        let noMatch = NoMatch (CreateNewId())
                        let iterCheck = RepeatIterateOrExit.Create (CreateNewId()) iterNext repeatId noMatch.StatePointer |> RepeatIterOrExit
                        let oisNew = oisCleaned.SetNextState iterCheck.StatePointer
                        let bundle = MultiPath(MultiPath.Create [ecNew.SinglePathPointer; oisCleaned.SinglePathPointer])

                        let stMapRet =
                            stMapNew
                            |>  Map.add bundle.Id bundle
                            |>  Map.add newExit.Id newExit
                            |>  Map.add iterCheck.Id iterCheck
                            |>  Map.add noMatch.Id noMatch
                            |>  wsUpdate ecNew
                            |>  wsUpdate oisNew
                        Refactored(bundle.StatePointer), stMapRet
            |   _ -> failwith "More than one overlapping at this point should never happen"

        let refactorOneInSetOverlap mlst =
            getOneInSetOverlap mlst
            |>  List.filter(fun (_, nd) -> nd.Length > 1)
            |>  function
            |   []      -> Unrefactored, stMap
            |   oislst -> 
                let EMIter = NextStp iterPtr 
                let EMNxt  = NextStp nextPtr

                let (refactored,stMapNew) = refactorRepeaterRec (EMIter) (EMNxt) exitPtr repeatId stMap
                match refactored with
                |   Refactored single    ->  //  was refactored
                    //  Already in a refactoring chain.
                    //  The decision whether the current state chooses the iter- or exit-path of the repeat comes later.
                    //  Here is assumed that part has been dealt with, later in the chain.
                    //  Here we can join all the options into one single step.
                    //  Create a new combination, and remove the origins.
                    //  To clarify, it is possible to distinguish iter-path and exit-path or overlapping tokens,
                    //  but it doesn't matter at this point, because all need the same next-step, which is "single"
                    let allTokens = oislst |>   List.map(fst)
                    let allIdt    = oislst |>   List.map(fun (_,n) -> n |> List.map(fun nd -> nd.IdSp.Id)) |> List.collect id |> List.distinct
                    let quickCheck = qcOneInSet allTokens
                    let sp = SinglePath (SinglePath.Create (OneInSetMatch({ QuickCheck = quickCheck; ListCheck = allTokens})) single.StatePointer)
                    let stNew = allIdt |> List.fold(fun (s:Map<StateId, StateNode>) i -> s.Remove i) stMapNew
                    Refactored(sp.StatePointer), stNew.Add(sp.Id,sp)
                |   Unrefactored    ->  //  was not refactored
                    //  Here is the start of a refactoring chain.
                    //  Three state-types must exist after the refactoring of this point:
                    //  1 - OneInSet matches that enters into the iteration path of the repeat
                    //  2 - OneInSet matches that enters into the exit path of the repeat
                    //  3 - OneInSet matches that may enter either path, ie this decision is made in the next step
                    //
                    //  We already have type 1 and 2, (iterPtr, nextPtr), of which overlapping Tokens
                    //  need to be moved to a (new) type 3 state.
                    //  In the iter-path, there must be a choice between a type 1 and type 3
                    //  In the exit-path, there must be a choice between a type 2 and type 3
                    //
                    //  So this
                    //  Repeat Start -> |I -> [AB] -> D -> Next()
                    //                  |X -> [AC] -> G -> 2
                    //
                    //  Becomes this (note that the t3's (=type 3) are equal):
                    //  Repeat Start -> |I -> |[B] ->  D -> Next()
                    //               (t3:)    |[A] -> |D -> Next()
                    //                                |G -> 2
                    //
                    //                  |X -> |[C] ->  G -> 2
                    //               (t3:)    |[A] -> |G -> 2
                    //                                |D -> Next()
                    //
                    //  Simplified to:
                    //  Repeat Start -> |[A] -> |I -> |D -> Next()
                    //                  |       |X -> |G -> 2
                    //                  ||I -> |[B] ->  D -> Next()
                    //                  ||X -> |[C] ->  G -> 2
                    //


                    let allOverlappingTokens = 
                        oislst 
                        |>  List.filter(fun (_, nd) -> nd.Length > 1)  // only candidate if they can be merged
                        |>  List.map(fst)

                    let cleanedStates = removeTokenFromOneInSet allOverlappingTokens [stMap.[iterPtr.Id]; stMap.[nextPtr.Id]]
                    let t1 = cleanedStates |> List.find(fun i -> i.Id = iterPtr.Id)
                    let t2 = cleanedStates |> List.find(fun i -> i.Id = nextPtr.Id)

                    let t1Next = t1.NextStatePtr
                    let t2Next = t2.NextStatePtr

                    let t3IterExit = RepeatIterateOrExit.Create (CreateNewId()) t1Next repeatId t2Next |> RepeatIterOrExit

                    let t3 = 
                        let newQC = qcOneInSet allOverlappingTokens
                        SinglePath({Id = CreateNewId(); State = (OneInSetMatch({ QuickCheck = newQC; ListCheck = allOverlappingTokens})); NextState = t3IterExit.StatePointer})


                    let bundle = MultiPath(MultiPath.Create [t3.SinglePathPointer; exitPtr.SinglePathPointerValue])

                    let stMapRet =
                        stMapNew
                        |>  Map.add t3IterExit.Id t3IterExit
                        |>  Map.add t3.Id t3
                        |>  Map.add bundle.Id bundle
                        |>  wsUpdate t1
                        |>  wsUpdate t2
                    Refactored(bundle.StatePointer), stMapRet 
            |   _ -> failwith "Not Implemented yet"


        let refactorPlainMerges mlst =
            getPlainMerges mlst
            |>  List.filter(fun (_, nd) -> nd.Length > 1)  // only candidate if they can be merged
            |>  function
            |   []      -> Unrefactored, stMap
            |   (_, lst) :: _ -> // expect max one entry 
                let EMIter = lst |> List.find(fun e -> e.IdSp.Id = iterPtr.Id) 
                let EMNxt  = lst |> List.find(fun e -> e.IdSp.Id = nextPtr.Id)

                let (refactored,stMapNew) = refactorRepeaterRec (EMIter.Next) (EMNxt.Next) exitPtr repeatId stMap
                match refactored with
                |   Refactored single    ->  //  was refactored
                    //  The current Plains can be merged into one state. The next simply points to "single"
                    let ndIter = stMapNew.[EMIter.IdSp.Id].SetNextState single
                    Refactored(ndIter.StatePointer), stMapNew.Remove(EMNxt.IdSp.Id) |> wsUpdate ndIter
                |   Unrefactored     ->  //  was not refactored
                    //  This is the start of a refactoring chain, the decision point whether the iter-path or 
                    //  exit-path will be chosen. The current plains are merged into one. The next points
                    //  to a Multipath in which the decision is made to iter or exit. The match makes the decision.
                    //  t1 = type 1 = iter path
                    //  t2 = type 2 = exit path
                    //
                    //  So starting with this:
                    //  Repeat Start -> |I -> A -> B -> Next()
                    //                  |X -> A -> C -> 2
                    //
                    //  Becomes this:
                    //  Repeat Start -> A -> |I -> B -> Next()
                    //                       |X -> C -> 2
                    //  
                    //  If "B" is a MultiPath with choices (B1,B2) then nothing extra needs to be done
                    //  Repeat Start -> A -> |I -> |B1 -> Next()
                    //                             |B2 -> F -> Next()
                    //                       |X -> C -> 2

                    let t1 = stMapNew.[EMIter.IdSp.Id]
                    let t2 = stMapNew.[EMNxt.IdSp.Id]

                    let t1Next = t1.NextStatePtr
                    let t2Next = t2.NextStatePtr

                    let (RepeatIterOrExit stExit) = stMapNew.[exitPtr.Id]
                    let newExit = RepeatIterOrExit { stExit with IterateState = t1Next.StatePointer;  NextState = t2Next.StatePointer }

                    let stm =
                        stMapNew
                        |>  Map.remove t2.Id
                        |>  wsUpdate newExit
                        |>  wsUpdate (t1.SetNextState newExit.StatePointer)

                    Refactored(t1.StatePointer), stm
            |   _ -> failwith "Not Implemented yet"

        
        let refactorMultiPath splst =
            true

            //  These scenario are refactor candidates:
            //  (mp = multipath, sp = singlepath)
            //  
            //  Scenario 1:
            //  Repeat Start -> |I  -> sp -> Next()
            //                  |X  -> mp -> End
            //
            //  Scenario 2:
            //  Repeat Start -> |I  -> mp -> Next()
            //                  |X  -> sp -> End
            //
            //  Scenario 3:
            //  Repeat Start -> |I  -> mp1 -> Next()
            //                  |X  -> mp2 -> End
            //
            //  Splitting up the commonalities in the I- and X- path, in the RepeatIterOrExit step
            //  results into these generic step-types
            //
            //  st1:    a construct


        refactorPlainMerges lst
        |>  function
            |   (Unrefactored _, _) -> 
                refactorOneInSetOverlap lst
                |>  function
                | (Unrefactored _, _) -> refactorPlainvsOneInSetOverlap lst
                | x -> x
            |   x -> x


    let refactor (ep:EmptyPath) =
        match stMap.[ep.NextState.Id] with
        |   RepeatIterOrExit re ->
            if not(re.IterateState.IsSinglePath && re.NextState.IsSinglePath) then
                (start, nodes, repeaters)
            else
                let (refac,stNew) = refactorRepeaterRec (re.IterateState.StatePointer) (re.NextState.StatePointer) (re.StatePointer) re.RepeatId stMap
                match refac with
                |   Refactored single    ->  //  was refactored
                    let p = EmptyPath { ep with NextState = single.StatePointer }
                    let nodes = stNew |> wsUpdate p |> Map.toList |> List.map(snd)
                    (start, nodes, repeaters)
                |   Unrefactored     ->  //  was not refactored
                    (start, nodes, repeaters)
        | _ -> (start, nodes, repeaters)

    match stMap.[start.Id] with
    |   RepeatStart d -> 
        let (EmptyPath ep) = stMap.[d.NextState.Id]
        refactor ep
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
            |   RepeatIterOrExit   re  -> 
                let ri =traverse (re.IterateState) (passedNodes.Add (re.Id)) (re.Id::used) 
                traverse (re.NextState) (passedNodes.Add (re.Id)) (re.Id::ri)
            |   NoMatch d -> current.Id::used
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
            |   Plain pl   -> if pl.``fixed``.Length > 1 then processConversion rg else createSinglePathFromRgx rg |> NFAMachine.Create
            |   OneInSet _ -> createSinglePathFromRgx rg |> NFAMachine.Create
            |   _ -> processConversion rg
            
        let emptyState() = EmptyPath({ Id = CreateNewId(); NextState = PointerToStateFinal})

        let createRepeat o min max =
            let linkState = emptyState()
            let repPath = convert o
            let repState = RepeatState.Create (CreateNewRepeatId()) min max
            let repExit = RepeatIterOrExit <| RepeatIterateOrExit.Create (CreateNewId())  repPath.Start repState.RepeatId linkState.StatePointer
            let repeatLoopStart = EmptyPath <| EmptyPath.Create (CreateNewId()) repExit.StatePointer
            
            let repeatedStates = appendStateIdToAllFinalPathNodes repPath.Start repPath.States repeatLoopStart.StatePointer

            let repStart = RepeatStart <| RepeatStart.Create (CreateNewId()) repState.RepeatId repeatLoopStart.SinglePathPointer
            NFAMachine.Create (repStart.StatePointer, repStart :: repExit :: repeatLoopStart :: linkState :: repeatedStates, repState :: repPath.Repeats)


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
                    let newNodes = appendStateIdToAllFinalPathNodes entryStartId nodes concatPtr
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
                    let mp = MultiPath.Create (idlst)
                    mp.StatePointer, (MultiPath mp) :: snlst, []
            |>  NFAMachine.Create
        |   Optional    r -> createRepeat r 0 1
        |   ZeroOrMore  r -> createRepeat r 0 0
        |   OneOrMore   r -> createRepeat r 1 0
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
    |   Empty     = 5


let PrintIt (nfa:NFAMachine) =
    let stMap = nfa.States |> List.map(fun e -> e.Id,e) |> Map.ofList
    let rsMap = nfa.Repeats |> List.map(fun e -> e.RepeatId,e) |> Map.ofList
    let passedNodes = Set.empty<StateId>

    let rec printLine (hist : LevelType list) (current: StatePointer) (passedNodes:Set<StateId>) =
        let printPrefix hist =
            hist
            |>  List.rev
            |>  List.iter(fun i ->
                match i with
                |   LevelType.Concat    -> printf "         "
                |   LevelType.Empty     -> printf "     "
                |   LevelType.Multi     -> printf "|    "
                |   LevelType.RepeatExit-> printf " |X    "
                |   LevelType.RepeatIter-> printf " |I    "
                |   LevelType.LoopStart -> printf "               "
            )

        printPrefix hist
        let rec printLineRest (hist : LevelType list) (current: StatePointer) (passedNodes:Set<StateId>) =
            if current.Id = 0u then
                printf "-*\n"
            else
                if not(passedNodes.Contains current.Id) then
                    match stMap.[current.Id] with
                    |   EmptyPath  ep   ->  
                        printf "~(%2d)" ep.Id
                        printLineRest (LevelType.Empty :: hist) ep.NextState (passedNodes.Add ep.Id)
                    |   SinglePath sp   -> 
                        match sp.State with
                        |   ExactMatch c    -> printf "-(%2d:\"%s\")" sp.Id (Regex.Escape(c.Char.ToString()))
                        |   OneInSetMatch o -> printf "-(%2d:[@])" sp.Id
                        printLineRest (LevelType.Concat :: hist) sp.NextState (passedNodes.Add sp.Id)
                    |   MultiPath mp    ->
                        let h::t = mp.States
                        printf "|(%2d)" mp.Id
                        printLineRest (LevelType.Multi :: hist) h.StatePointer (passedNodes.Add mp.Id)
                        t |> List.iter(fun e -> printLine (LevelType.Multi :: hist) e.StatePointer (passedNodes.Add mp.Id))
                    |   RepeatStart rs ->
                        let rt = rsMap.[rs.RepeatId]
                        printf "->>(%2d:<%2d,%2d>)" rs.Id rt.Min rt.Max
                        printLineRest (LevelType.LoopStart :: hist) (rs.NextState.StatePointer) (passedNodes.Add rs.Id)
                    |   RepeatIterOrExit ri ->
                        printf "-|I(%2d)" ri.Id
                        printLineRest (LevelType.RepeatIter :: hist) ri.IterateState (passedNodes.Add ri.Id)

                        if ri.IterateState.Id <> ri.NextState.Id then
                            printPrefix hist
                            printf "-|X(%2d)" ri.Id
                            printLineRest (LevelType.RepeatExit :: hist) ri.NextState (passedNodes.Add ri.Id)
                        else
                            printPrefix hist
                            printf "|X(%2d) :::^^^\n" ri.NextState.Id
                    |   NoMatch d -> printf "-Err(%2d)\n" d
                else
                    printf "-Loop(%2d)\n" current.Id

        printLineRest hist current passedNodes
    printLine [] nfa.Start passedNodes


let parseIt (nfa:NFAMachine) yaml =
    let stMap = nfa.States |> List.fold(fun (m:Map<_,_>) i -> m.Add(i.Id, i)) Map.empty<StateId, StateNode>
    let stRepeat = nfa.Repeats |> List.fold(fun (m:Map<_,_>) i -> m.Add(i.RepeatId, i)) Map.empty<RepeatId, RepeatState>
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
    let runningLoops = Map.empty<RepeatId, RepeatState>


    let rec processStr currentChar cs acc rollback (runningLoops : Map<RepeatId, RepeatState>) =
        let processCurrentChar = processStr currentChar
        let processNextChar cs acc rollback (runningLoops : Map<RepeatId, RepeatState>)= 
            let chk = (stream.Get())
            processStr chk cs acc rollback runningLoops 

        let NoMatch = { IsMatch = false ; FullMatch = [] }
        if cs = PointerToStateFinal then
            { IsMatch = true; FullMatch = acc |> List.rev }
        else
            let st = stMap.[cs.Id]
            match st with
            |   SinglePath p ->
                let nxt = p.NextState
                if (p.State.Match currentChar) then
                    processNextChar nxt (currentChar.Source.[0] :: acc) rollback runningLoops
                else 
                    NoMatch
            |   MultiPath p ->
                p.States
                |>  List.tryFind(fun t -> 
                    match stMap.[t.Id] with
                    |   SinglePath st -> st.State.Match currentChar
                    |   EmptyPath  _  -> true
                    |   RepeatIterOrExit _  -> true
                    |   _ -> failwith "Not implemented yet"
                    )
                |>  function
                    |   Some v -> 
                        match stMap.[v.Id] with
                        |   EmptyPath  st -> processCurrentChar (st.NextState) acc (rollback+1) runningLoops
                        |   SinglePath st -> processNextChar (st.NextState) (currentChar.Source.[0] :: acc) rollback runningLoops
                        |   MultiPath  _  -> failwith "Not implemented yet"
                        |   RepeatIterOrExit re -> processCurrentChar re.StatePointer acc rollback runningLoops
                    |   None -> NoMatch 
            |   EmptyPath p -> processCurrentChar p.NextState acc rollback runningLoops
            |   RepeatStart r ->
                let rlnew =
                    if runningLoops.ContainsKey r.RepeatId then
                        runningLoops.Remove(r.RepeatId).Add(r.RepeatId, stRepeat.[r.RepeatId])
                    else
                        runningLoops.Add(r.RepeatId, stRepeat.[r.RepeatId])
                processCurrentChar r.NextState.StatePointer acc rollback rlnew
            |   RepeatIterOrExit r ->
                let rs = runningLoops.[r.RepeatId]
                if rs.MustExit() then
                    processCurrentChar r.NextState acc rollback runningLoops
                else
                    let rlNew = runningLoops.Remove(r.RepeatId).Add(r.RepeatId, rs.Iterate())
                    let tryIterate = processCurrentChar r.IterateState acc rollback rlNew
                    if tryIterate.IsMatch then
                        tryIterate
                    else
                        if rs.CanExit() && r.IterateState<>r.NextState then
                            processCurrentChar r.NextState acc rollback (rlNew.Remove rs.RepeatId) // end loop by removing it from running loops
                        else
                            NoMatch
            |   NoMatch _ -> NoMatch

    processStr (stream.Get()) nfa.Start [] 0 runningLoops

let clts (cl:char list) = System.String.Concat(cl)

module ParseResult =
    let IsMatch pr = pr.IsMatch
    let FullMatch pr = pr.FullMatch


