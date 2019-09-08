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


type SinglePath = {
        Id         : StateId
        State      : SingleCharMatch
        NextState  : StatePointer 
    }
    with
        static member Create i mt nx = { Id = i; State = mt; NextState = nx }
        member this.LinkTo i = { this with NextState = i}
        member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
        member this.SinglePathPointer = SinglePathPointer.Create this.Id
        member this.Duplicate i = {this with Id = i}


type MultiPath = {
        Id         : StateId
        States     : SinglePathPointer list
    }
    with
        static member Create i mt = { Id = i; States = mt }
        member this.StatePointer = MultiPathPointer(MultiPathPointer.Create this.Id)
        member this.Duplicate i = {this with Id = i}

type RepeatInit = {
        Id          : StateId
        RepeatId    : RepeatId
        NextState   : StatePointer
    }
    with
        static member Create i ri nx = { Id = i; RepeatId = ri; NextState = nx }
        member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
        member this.SinglePathPointer = SinglePathPointer.Create this.Id
        member this.Duplicate i = {this with Id = i}

type RepeatIterateOrExit = {
        Id          : StateId
        RepeatId    : RepeatId
        IterateState: StatePointer
        NextState   : StatePointer 
    }
    with
        static member Create i it ri nx = { Id = i; IterateState = it; RepeatId = ri; NextState = nx }
        member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
        member this.SinglePathPointer = SinglePathPointer.Create this.Id
        member this.Duplicate i = {this with Id = i}

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
        member this.Duplicate i = {this with Id = i}

let PointerToStateFinal = SinglePathPointer (SinglePathPointer.Create 0u) 


type StateNode =
    |   SinglePath of SinglePath
    |   MultiPath  of MultiPath
    |   EmptyPath  of EmptyPath
    |   RepeatInit of RepeatInit
    |   RepeatStart of EmptyPath
    |   RepeatIterOrExit of RepeatIterateOrExit
    |   NoMatch of StateId
    with
        member this.Id 
            with get() =
                match this with
                |   SinglePath d -> d.Id
                |   MultiPath  d -> d.Id
                |   EmptyPath  d -> d.Id
                |   RepeatInit d -> d.Id
                |   RepeatStart d -> d.Id
                |   RepeatIterOrExit  d -> d.Id
                |   NoMatch d -> d

        member this.IsEmptyPathValue 
            with get() =
                match this with
                |   EmptyPath _ -> true
                |   _ -> false
        
        member this.IsRepeatStartValue
            with get() =
                match this with
                |   RepeatStart _ -> true
                |   _ -> false

        member this.NextState 
            with get() =
                match this with
                |   SinglePath d -> d.NextState.Id
                |   EmptyPath  d -> d.NextState.Id
                |   RepeatInit d -> d.NextState.Id
                |   RepeatStart d -> d.NextState.Id
                |   RepeatIterOrExit  d -> d.NextState.Id
                |   MultiPath  _ -> failwith "Multipath has no single nextstate"
                |   NoMatch _ -> failwith "NoMatch has no single nextstate"

        member this.NextStatePtr 
            with get() =
                match this with
                |   SinglePath d -> d.NextState
                |   EmptyPath  d -> d.NextState
                |   RepeatInit d -> d.NextState.StatePointer
                |   RepeatStart d -> d.NextState
                |   RepeatIterOrExit  d -> d.NextState
                |   MultiPath  _ -> failwith "Multipath has no single nextstate"
                |   NoMatch _ -> failwith "NoMatch has no single nextstate"


        member this.SetNextState i =
                match this with
                |   SinglePath d -> SinglePath { d with NextState = i }
                |   EmptyPath  d -> EmptyPath { d with NextState = i }
                |   RepeatStart d -> RepeatStart { d with NextState = i }
                |   RepeatIterOrExit  d -> RepeatIterOrExit  { d with NextState = i }
                |   RepeatInit d -> RepeatInit { d with NextState = i }
                |   _ -> failwith "Illegal to set nextstate"

        member this.StatePointer  
            with get() =
                match this with
                |   SinglePath d -> d.StatePointer
                |   MultiPath  d -> d.StatePointer
                |   EmptyPath  d -> d.StatePointer
                |   RepeatInit d -> d.StatePointer
                |   RepeatStart d -> d.StatePointer
                |   RepeatIterOrExit  d -> d.StatePointer
                |   NoMatch d -> SinglePathPointer (SinglePathPointer.Create d)

        member this.SinglePathPointer
            with get() =
                match this with
                |   SinglePath d -> d.SinglePathPointer
                |   EmptyPath  d -> d.SinglePathPointer
                |   RepeatInit d -> d.SinglePathPointer
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


module MT = //    Match Tree
    let mutable private currentId = 0u
    let mutable private currentRepeatId = 0u
    let private CreateNewId() =
        currentId <- (currentId + 1u)
        currentId
    
    let private CreateNewRepeatId() =
        currentRepeatId <- (currentRepeatId + 1u)
        RepeatId currentRepeatId

    let mutable private allNodes = Map.empty<StateId, StateNode>
    let mutable private allRepeats = new System.Collections.Generic.List<RepeatState>()

    let Init() =
        currentId       <- 0u
        currentRepeatId <- 0u
        allNodes        <- Map.empty<StateId, StateNode>
        allRepeats      <- new System.Collections.Generic.List<RepeatState>()

    let addAndReturn (nd:StateNode) =
        allNodes <- allNodes.Add(nd.Id, nd)
        nd.StatePointer

    let updateAndReturn (nd:StateNode) =
        allNodes <- allNodes.Remove(nd.Id).Add(nd.Id, nd)
        nd.StatePointer

    let createRepeatState min max = 
        let st = RepeatState.Create (CreateNewRepeatId()) min max
        allRepeats.Add(st)
        st

    let getNode i = allNodes.[i]

    let createSinglePath mt nx = SinglePath.Create (CreateNewId()) mt nx |> SinglePath |> addAndReturn
    let createMultiPath mt = MultiPath.Create (CreateNewId()) mt |> MultiPath |> addAndReturn
    let createEmptyPath  nx = EmptyPath.Create (CreateNewId()) nx |> EmptyPath |> addAndReturn
    let createRepeatInit ri nx = RepeatInit.Create (CreateNewId()) ri nx |> RepeatInit |> addAndReturn
    let createRepeatStart  nx = EmptyPath.Create (CreateNewId()) nx |> RepeatStart |> addAndReturn
    let createRepeatIterOrExit it ri nx = RepeatIterateOrExit.Create (CreateNewId()) it ri nx |> RepeatIterOrExit |> addAndReturn
    let noMatch() = NoMatch (CreateNewId()) |> addAndReturn

    let inline lookup< ^a when ^a:(member Id : uint32)> sp = getNode (^a : (member Id  : uint32) sp)
    let inline setNextState (nx:StatePointer) sp =
        let nd = lookup sp
        nd.SetNextState nx |> updateAndReturn

    let inline Next sp = 
        let nd = lookup sp
        nd.NextStatePtr

    let updateNode nd = nd |> updateAndReturn
    
    let ToNFAMachine st = NFAMachine.Create (st, (allNodes |> Map.toList |> List.map(snd)), (allRepeats |> Seq.toList))

    let duplicate (currPtr : StatePointer) =
        match lookup currPtr with
        |   EmptyPath   p -> createEmptyPath p.NextState
        |   SinglePath  p -> createSinglePath p.State p.NextState
        |   RepeatStart p -> createRepeatStart p.NextState
        |   RepeatInit  p -> createRepeatInit p.RepeatId p.NextState
        |   RepeatIterOrExit p -> createRepeatIterOrExit p.IterateState p.RepeatId p.NextState

    let duplicateAndLinkToNext (next:StatePointer) (currPtr : StatePointer) =
        match lookup currPtr with
        |   EmptyPath   _ -> createEmptyPath next
        |   SinglePath  p -> createSinglePath p.State next
        |   RepeatStart _ -> createRepeatStart next
        |   RepeatInit  p -> createRepeatInit p.RepeatId next
        |   RepeatIterOrExit p -> createRepeatIterOrExit p.IterateState p.RepeatId next



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
            |   (RepeatInit _, SinglePath _) -> 1
            |   (SinglePath _, RepeatInit _) -> -1
            |   (RepeatInit _, EmptyPath _) ->  -1
            |   (EmptyPath _, RepeatInit _) -> 1
            |   (RepeatIterOrExit _, SinglePath _) -> 1
            |   (SinglePath _, RepeatIterOrExit _) -> -1
            |   (RepeatIterOrExit _, EmptyPath _) ->  -1
            |   (EmptyPath _, RepeatIterOrExit _) -> 1
            |   _ -> 0
        )


    let createAndSimplifyMultiPath (lst:StatePointer list) : StatePointer =
        if lst |> List.length = 1 then (lst |> List.head).StatePointer
        else 
            let sorted =
                lst
                |>  List.map lookup
                |>  SortStateNodes
                |>  List.map(fun i -> i.SinglePathPointer)
            let mn = createMultiPath sorted
            mn


    let createAndSimplifyMultiPathSp (lst:SinglePathPointer list) : StatePointer  =
        createAndSimplifyMultiPath (lst |> List.map(fun i -> i.StatePointer))

    let getRepeatState (ri:RepeatId) = allRepeats |> Seq.find(fun e -> e.RepeatId = ri)

    let getSinglePathPointers pt =
        match pt with 
        |   SinglePathPointer p -> [pt.SinglePathPointerValue]
        |   MultiPathPointer  p -> 
            let (MultiPath mp) = lookup pt
            mp.States


let qcOneInSet ls = ls |> List.fold(fun s i -> s ||| uint32(i)) 0u

let createSinglePathFromRgx rgx =
    match rgx with
    |   Plain       d -> MT.createSinglePath (ExactMatch({ Char = d.``fixed``.[0]; ListCheck = d.Token })) PointerToStateFinal
    |   OneInSet    d -> 
        let listCheck = d.Token'.Force()
        let quickCheck = qcOneInSet listCheck
        MT.createSinglePath (OneInSetMatch({ QuickCheck = quickCheck; ListCheck = listCheck})) PointerToStateFinal
    |   _ -> failwith "Not a single char match"



let nonGenericTokens =
    [   Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark``; Token.``t-colon``;
        Token.``t-dot``; Token.``t-square-bracket-start``; Token.``t-square-bracket-end``; Token.``t-curly-bracket-start``; Token.``t-curly-bracket-end``;
        Token.``t-hash``; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``; Token.``t-gt``;
        Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``; Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``;
        Token.``ns-dec-digit``; Token.``c-escape``; Token.``t-comma``]
    |>   Set.ofList



type NormalizedSPExactChar = { Token : Token; IdSp : StatePointer; Next : StatePointer ; ExactChar : char }
type NormalizedSPOneInSet  = { Token : Token; IdSp : StatePointer; Next : StatePointer }

type NormalizedForSPRefactoring =
    |   ExactMatchItem of NormalizedSPExactChar
    |   OneInSetItem of NormalizedSPOneInSet


let normalizeForSPRefactoring (stNodes:StateNode list) =
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
    |   PlainMerge          of char  * (NormalizedSPExactChar list)
    |   OneInSetOverlap     of Token * (NormalizedSPOneInSet list)
    |   CrossTypeOverlap    of Token * (NormalizedSPExactChar list) * (NormalizedSPOneInSet list)


let getPlainMerges (lst:NormalizedForSPRefactoring list) =
    lst
    |>  List.fold(fun s e -> 
        match e with
        |   ExactMatchItem e -> e :: s
        |   _ -> s
    ) []
    |>  List.groupBy(fun e -> e.ExactChar)


let getOneInSetOverlap (lst:NormalizedForSPRefactoring list) =
    lst
    |>  List.fold(fun s e ->
        match e with
        |   OneInSetItem e -> e :: s
        |   _ -> s
    ) []
    |>  List.groupBy(fun e -> e.Token)


let getCrossTypeOverlap (lst:NormalizedForSPRefactoring list) =
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

let getRefactoringCanditates (lst:NormalizedForSPRefactoring list) =
    let l1 =
        lst
        |>  getPlainMerges
        |>  List.filter(fun (c,l) -> l.Length > 1)
        |>  List.map(fun e -> PlainMerge e)
    let l2 =
        lst
        |>  getOneInSetOverlap
        |>  List.filter(fun (c,l) -> l.Length > 1)
        |>  List.map(fun e -> OneInSetOverlap e)
    let l3 =
        lst
        |>  getCrossTypeOverlap
        |>  List.filter(fun (c,l1,l2) -> l1.Length > 0 && l2.Length > 0)
        |>  List.map(fun e -> CrossTypeOverlap e)
    l1 @ l2 @ l3



let appendStateIdToAllFinalPathNodes (entryStartId:StatePointer) (concatPtr:StatePointer) =
    let passedNodes = Set.empty<StateId>
    
    let rec traverse (prev:StatePointer) (current:StatePointer) (passedNodes:Set<StateId>) (concatPtr:StatePointer) =
        if  current.Id = 0u || current.Id = concatPtr.Id || passedNodes.Contains (current.Id) then current 
        else
            let node = MT.lookup current
            match node with
            |   MultiPath  d -> 
                d.States |> List.map(fun stid -> traverse d.StatePointer (stid.StatePointer) (passedNodes.Add d.Id) concatPtr) |> ignore
                current
            |   EmptyPath  d  when d.NextState.Id = PointerToStateFinal.Id -> 
                match MT.lookup prev with
                |   SinglePath _ ->  MT.setNextState concatPtr prev
                |   MultiPath  _ -> current
                |   RepeatIterOrExit  _ -> MT.setNextState concatPtr prev 
                |   _ -> failwith "Not implemented yet"
            |   EmptyPath     d -> traverse d.StatePointer (d.NextState) (passedNodes.Add (d.Id)) concatPtr
            |   RepeatIterOrExit    d -> traverse d.StatePointer (d.NextState) (passedNodes.Add (d.Id)) concatPtr
            |   RepeatInit   d -> traverse d.StatePointer (d.NextState.StatePointer) (passedNodes.Add (d.Id)) concatPtr
            |   RepeatStart d -> traverse d.StatePointer (d.NextState) (passedNodes.Add (d.Id)) concatPtr
            |   SinglePath d ->
                if d.NextState.Id = 0u then
                    MT.setNextState concatPtr current
                else
                    traverse d.StatePointer (d.NextState) (passedNodes.Add (d.Id)) concatPtr
            |   NoMatch _ -> current
    traverse PointerToStateFinal entryStartId passedNodes concatPtr |> ignore


let duplicateStructureAndLinkToNext (passed:StatePointer list) (premapped:(StatePointer*StatePointer) list) (entryStartId:StatePointer) (concatPtr:StatePointer) =
    let passedNodes = 
        passed
        |>  List.map(fun i -> i.Id)
        |>  List.distinct
        |>  Set.ofList
        
    let preMappedNodes = Map.ofList premapped

    let dup (pm:Map<StatePointer, StatePointer>) c n = 
        let tr = 
            if pm.ContainsKey n then pm.[n]
            else n
        MT.duplicateAndLinkToNext tr c

    let rec traverse (current:StatePointer) (passedNodes:Set<StateId>) (pm:Map<StatePointer, StatePointer>)=
        if  current.Id = 0u || current.Id = concatPtr.Id || passedNodes.Contains (current.Id) then current 
        else
            let node = MT.lookup current
            match node with
            |   MultiPath d -> 
                d.States 
                |> List.fold(fun npt stid -> (traverse (stid.StatePointer) (passedNodes.Add d.Id) pm) ::  npt) []
                |>  MT.createAndSimplifyMultiPath
            |   EmptyPath d  when d.NextState.Id = PointerToStateFinal.Id -> 
                MT.duplicateAndLinkToNext concatPtr current
            |   EmptyPath d -> 
                traverse (d.NextState) (passedNodes.Add (d.Id)) pm |> dup pm current
            |   RepeatIterOrExit d ->
                traverse (d.NextState) (passedNodes.Add (d.Id)) pm |> dup pm current
            |   RepeatInit d -> 
                traverse (d.NextState.StatePointer) (passedNodes.Add (d.Id)) pm |> dup pm current
            |   RepeatStart d -> 
                let dp = dup pm current PointerToStateFinal
                let nx = traverse (d.NextState) (passedNodes.Add (d.Id)) (pm.Add(current, dp))
                MT.setNextState nx dp
            |   SinglePath d ->
                if d.NextState.Id = 0u then
                    MT.setNextState concatPtr current
                else
                    traverse (d.NextState) (passedNodes.Add (d.Id)) pm |> dup pm current
            |   NoMatch _ -> current
    traverse entryStartId passedNodes preMappedNodes


let rec refactorCommonPlains (sil:SinglePathPointer list) =
    //  holds all plain (exact char matches) for which there are multiple occurences in ``sil``
    let stnl = 
        sil 
        |>  List.map(fun sp -> MT.lookup sp)
        |>  normalizeForSPRefactoring
        |>  getPlainMerges
        |>  List.filter(fun (_,lst) -> List.length lst > 1)    // only with more than one occurrence (ie only ambigious situations)

    if stnl.Length = 0 then
        sil
    else
        let rec refactorPlains (sil:SinglePathPointer list) (stnl:(char * (NormalizedSPExactChar list)) list) =
            match stnl with
            |   []  -> sil
            |   hd :: tail ->
                let target = hd |> snd |> List.map(fun e -> (e.IdSp, e.Next))
                let primary = fst target.Head
                let (filterIds, nextIds) = target |> List.unzip
                let silNew = 
                    let siln = refactorCommonPlains (nextIds |> List.map(MT.getSinglePathPointers) |> List.collect id)
                    let filtIdSet = filterIds |> List.map(fun e -> e.Id) |> Set.ofList

                    let silNew = primary.SinglePathPointerValue :: (sil |> List.filter(fun e -> not(filtIdSet.Contains e.Id)))
                    if siln.Length = 1 then
                        MT.setNextState siln.Head.StatePointer primary |> ignore
                        silNew
                    else 
                        let isAllEmpty = 
                            siln 
                            |> List.forall(fun e -> 
                                let nd = MT.lookup e
                                nd.IsEmptyPathValue
                            )
                        if isAllEmpty then
                            let link = siln.Head
                            MT.setNextState siln.Head.StatePointer primary |> ignore
                            silNew
                        else
                            let silnSorted =
                                siln
                                |>  List.map(fun sp -> MT.lookup sp.StatePointer)
                                |>  MT.SortStateNodes
                                |>List.map(fun sn -> sn.SinglePathPointer)

                            let bundle = MT.createMultiPath (silnSorted)
                            MT.setNextState bundle primary |> ignore
                            silNew
                refactorPlains silNew tail
        refactorPlains sil stnl
   


let removeTokenFromOneInSet (chtLst: Token list) (nodes:StatePointer list) =
    let tks = Set.ofList chtLst
    let oldNewMp = nodes |> List.map(fun n -> n, MT.duplicate n)
    oldNewMp
    |>  List.fold(fun s (o,n) ->
        let nd = MT.lookup n
        match nd with
        |   SinglePath sp -> 
            let (OneInSetMatch ois) = sp.State
            let newLC = ois.ListCheck |> List.filter(fun t -> not(tks.Contains t))
            if newLC.Length > 0 then
                let newQC = qcOneInSet newLC
                (o, (MT.updateNode <| SinglePath({ sp with State = (OneInSetMatch({ QuickCheck = newQC; ListCheck = newLC}))}))) :: s
            else 
                s
        |   _ -> failwith "Not implemented - this should never happen"
    ) []



let refactorConflictingCharacterSets (sil:SinglePathPointer list) =
    let stnl =
        sil
        |>  List.map(fun sp -> MT.lookup sp)
        |>  normalizeForSPRefactoring
        |>  getOneInSetOverlap
        |>  List.filter(fun (t, ls) -> List.length ls > 1)

    if List.length stnl = 0 then
        sil
    else
        //  Combine duplicate tokens
        let rec refactorCharacterSets (sil:SinglePathPointer list) (stnl:(Token*(NormalizedSPOneInSet list)) list) =
            match stnl with
            |   []  -> sil
            |   (cht, lst) :: tail ->
                let toRemove, allNextIds =
                    lst
                    |>  List.map(fun e -> e.IdSp.Id, (MT.getSinglePathPointers e.Next))
                    |>  List.unzip

                let allRefactoredOneInSet = removeTokenFromOneInSet [cht] (lst |> List.map(fun i ->  i.IdSp)) 

                let tailNew =
                    let mpOoldToNew = Map.ofList allRefactoredOneInSet
                    let mapOrOld r = if mpOoldToNew.ContainsKey r then mpOoldToNew.[r] else r
                    tail
                    |>  List.map(fun (t,nois) -> 
                        t, nois |> List.map(fun ns -> { ns with IdSp = mapOrOld ns.IdSp })
                    )

                let bundle = MT.createMultiPath (allNextIds |> List.collect id)
                let sp = MT.createSinglePath (OneInSetMatch({ ListCheck = [cht]; QuickCheck = uint32(cht)})) bundle

                let silNew = 
                    sil 
                    |>  List.filter(fun i -> toRemove |> List.contains(i.Id) |> not)
                    |>  List.append (allRefactoredOneInSet |> List.map snd |> List.map(fun i -> i.SinglePathPointerValue))
                refactorCharacterSets (sp.SinglePathPointerValue :: silNew) tailNew
        refactorCharacterSets sil stnl


let refacorConflictingPlainWithCharacterSets (sil:SinglePathPointer list) =
    let stnl =
        sil 
        |>  List.map(fun i -> MT.lookup i)
        |>  normalizeForSPRefactoring
        |>  getCrossTypeOverlap
        |>  List.filter(fun (_, l1, l2) -> l1.Length > 0 && l2.Length >0)

    if List.length stnl = 0 then
        sil
    else
        let rec refactorPlainsWithCharSets (sil:SinglePathPointer list) (stnl:(Token*(NormalizedSPExactChar list)*(NormalizedSPOneInSet list)) list) =
            match stnl with
            |   []  -> sil
            |   (cht, elst, clst)  :: tail -> 
                let primary = elst |> List.head |> fun i -> i.IdSp
                let toRemove, allNextIds =
                    clst
                    |>  List.map(fun e -> e.IdSp, e.Next.SinglePathPointerValue)
                    |>  List.append (elst |> List.map(fun e -> e.IdSp, e.Next.SinglePathPointerValue))
                    |>  List.distinct
                    |>  List.unzip

                let allRefactoredOneInSet = removeTokenFromOneInSet [cht] (clst |> List.map(fun e -> e.IdSp))

                let tailNew =
                    let mpOoldToNew = Map.ofList allRefactoredOneInSet
                    let mapOrOld r = if mpOoldToNew.ContainsKey r then mpOoldToNew.[r] else r
                    tail
                    |>  List.map(fun (t, ne ,nois) -> 
                        t,ne, nois |> List.map(fun ns -> { ns with IdSp = mapOrOld ns.IdSp })
                    )

                let bundle = MT.createMultiPath allNextIds
                MT.setNextState bundle.StatePointer primary |> ignore

                let silNew = 
                    sil 
                    |>  List.filter(fun i -> toRemove |> List.exists(fun x -> x.Id = i.Id) |> not)
                    |>  List.append ( primary.SinglePathPointerValue :: (allRefactoredOneInSet |> List.map snd |> List.map(fun i -> i.SinglePathPointerValue)))
                
                refactorPlainsWithCharSets silNew tailNew
        refactorPlainsWithCharSets sil stnl


type RefactorResult =
    |   Refactored of StatePointer
    |   Unrefactored 

let refactorRepeaterStateCollisions (start:StatePointer) =
    //  Repeat is used for option, zero-or-more, once-or-more and repeat-between-min-and-max.
    //  Repeat has two outgoing paths: iteration, and exit.
    //  When iter and exit paths have something in common - collisions, it is difficult to determine
    //  in which path the parser is supposed to be. To solve this, the collissions are joined together in both paths.
    //  The choice whether to follow the iter- or exit-paths is moved to a new location, where this choice is non-ambigiuos.
    //
    //  All repeater involved paths must pass the "RepeatIterOrExit" state, which during parsing, checks the "min" and "max" constraint.
    //  This constraint is essential in a range-repeat. It also removes the repeat-state (iteration count) from the "running loops" list.

    let filterStatePointers (st1:SinglePathPointer list, st3:SinglePathPointer list) = 
        let m = st3 |> List.map(fun i -> i.Id) |> Set.ofList
        st1 |> List.filter(fun i -> not(m.Contains i.Id))

    let createAndSimplifyMultiPath (lst:StatePointer list) : StatePointer =
        if lst |> List.length = 1 then (lst |> List.head).StatePointer
        else 
            let sorted =
                lst
                |>  List.map(MT.lookup)
                |>  MT.SortStateNodes
                |>  List.map(fun i -> i.SinglePathPointer)
            let mn = MT.createMultiPath sorted
            mn

    let createAndSimplifyMultiPathSp (lst:SinglePathPointer list) : StatePointer  =
        createAndSimplifyMultiPath (lst |> List.map(fun i -> i.StatePointer))
    
    let setNextStateForSet (st3:SinglePathPointer list) (next:StatePointer) = 
        st3 |>  List.map(MT.setNextState next) 

    let getExistingOneInSetT3 (st3lst:(SinglePathPointer*(StatePointer list)) list) = 
        st3lst
        |>  List.tryPick(fun (i, lst) -> 
            match MT.lookup i with
            |   SinglePath sp ->
                match sp.State with
                |   OneInSetMatch ois -> Some (sp, ois.ListCheck, lst)
                |   _ -> None
            |   _ -> None
        )
    
    let splitToIOrX (st1:SinglePathPointer list) (st2:SinglePathPointer list) (idlst:StateId list) = 
        let st1s = st1 |> List.map(fun i-> i.Id) |> Set.ofList
        let st2s = st2 |> List.map(fun i-> i.Id) |> Set.ofList
        idlst
        |>  List.fold(fun (s1:Set<StateId>,s2:Set<StateId>) i ->
            match st1s.Contains i, st2s.Contains i with
            |   (true, false) -> (s1.Add i, s2)
            |   (false, true) -> (s1, s2.Add i)
            |   (true,true)   -> (s1.Add i, s2.Add i) // should never happen?
            |   (false,false) -> (s1,s2)             //  filter
        ) (Set.empty,Set.empty)
        

    let optimizeOis (st3andIAndX: (SinglePathPointer * StatePointer list * StatePointer list) list) =
        let oisMap = 
            st3andIAndX
            |>  List.fold(fun s (i, _,_) -> 
                match MT.lookup i with
                |   SinglePath sp ->
                    match sp.State with
                    |   OneInSetMatch ois -> Map.add sp.Id ois.ListCheck s
                    |   _ -> s
                |   _ -> s
            ) Map.empty

        let oisOptLst =
            st3andIAndX
            |>  List.filter(fun (i, _,_) -> oisMap.ContainsKey i.Id)
            |>  List.groupBy(fun (_, st1, st2) -> st1,st2)
            |>  List.filter(fun (k, v) -> v |> List.length > 1) //  only with joined I- and X-paths

        let stn3optimized =
            oisOptLst
            |>  List.map(fun ((s1,s2), lst) ->
                let tokens =
                    lst
                    |>  List.map(fun (i, _,_) -> oisMap.[i.Id])
                    |>  List.collect id
                let sp = MT.createSinglePath (OneInSetMatch { QuickCheck = qcOneInSet tokens; ListCheck = tokens}) PointerToStateFinal
                sp, (s1,s2)
            )


        let remove = stn3optimized |> List.map(fun (i,_) -> i.Id) |> Set.ofList
        let ret =
            st3andIAndX
            |>  List.filter(fun (i,_,_) -> not(remove.Contains i.Id))
        ret @ (stn3optimized |> List.map(fun (i,(s1,s2)) -> i.SinglePathPointerValue, s1,s2))


    let refactorAndSeperateSinglePaths (st1:SinglePathPointer list) (st2:SinglePathPointer list) = 
        //  This function merges and separates singlepaths.
        //  There are two types of single paths:
        //      1 - Plain ie "A"
        //      2 - One In Set ie [AB]
        //
        //  These can be three type of collisions:
        //      1 - Plain vs Plain, ie "A" vs "A"
        //      2 - One in Set vs One In Set, ie [AC] vs [AD]
        //      3 - Plain vs One in Set, ie "A" vs [AC]
        // 
        //  The input for this func are two lists, st1 and st2. Assumptions:
        //      - Each element in st1 has no collision with any other element in st1
        //      - Each element in st2 has no collision with any other element in st2
        //  
        //  The func output is: (st1clean, st2clean, st3collisions, stMap) with
        //      st1clean        - st1 with all colliding elements removed
        //      st2clean        - st2 with all colliding elements removed
        //      st3collisions   - all merged singlepaths, in the form (sp, s1n list, s2n list) with:
        //              sp  - pointer to new created merge product
        //              s1n - pointer to next node, originating from st1
        //              s2n - pointer to next node, originating from st2
        //      sMap - updated input stMap with all changes processed
        st1 @ st2
        |>  List.map(MT.lookup)
        |>  normalizeForSPRefactoring
        |>  getRefactoringCanditates
        |>  List.fold(fun (st1lst, st2lst, st3lst) rc ->
            match rc with
            |   PlainMerge (c, cl) ->
                let dup = cl.Head
                let stNew = MT.createSinglePath (ExactMatch {Char = dup.ExactChar; ListCheck = [dup.Token]}) PointerToStateFinal
                let (ilstorig, xlstorig) = splitToIOrX st1lst st2lst (cl |> List.map(fun i -> i.IdSp.Id))
                let ilst = cl |> List.filter(fun i -> ilstorig.Contains i.IdSp.Id) |> List.map(fun i -> i.Next)
                let xlst = cl |> List.filter(fun i -> xlstorig.Contains i.IdSp.Id) |> List.map(fun i -> i.Next)
                let st3 = (stNew.SinglePathPointerValue, ilst, xlst) :: st3lst
                let removeIds = cl |> List.map(fun e -> e.IdSp.SinglePathPointerValue)
                filterStatePointers (st1lst, removeIds), filterStatePointers (st2lst, removeIds), st3

            |   OneInSetOverlap (t, ls) ->
                let allOverlappingTokens = [t]

                let cleanedNodes = removeTokenFromOneInSet allOverlappingTokens (ls |> List.map(fun i ->  i.IdSp)) |> List.map snd

                let removeIds =
                    let orig = ls |> List.map(fun i -> i.IdSp.Id) |> Set.ofList
                    let proj = cleanedNodes |> List.map(fun i -> i.Id) |> Set.ofList
                    Set.difference orig proj
                    |>  Set.toList
                    |>  List.map(fun i -> SinglePathPointer.Create i)

                let lst = allOverlappingTokens
                let t3 = MT.createSinglePath (OneInSetMatch { QuickCheck = qcOneInSet lst; ListCheck = lst}) PointerToStateFinal

                let (ilstorig, xlstorig) = splitToIOrX st1lst st2lst (ls |> List.map(fun i -> i.IdSp.Id))
                let ilst = ls |> List.filter(fun i -> ilstorig.Contains i.IdSp.Id) |> List.map(fun i -> i.Next)
                let xlst = ls |> List.filter(fun i -> xlstorig.Contains i.IdSp.Id) |> List.map(fun i -> i.Next)

                let st3New = (t3.SinglePathPointerValue, ilst, xlst):: st3lst
                filterStatePointers (st1lst, removeIds), filterStatePointers (st2lst, removeIds), st3New
            |   CrossTypeOverlap (t, ecl, oisl) ->
                let allOverlappingTokens = [t]

                let cleanedNodes = removeTokenFromOneInSet allOverlappingTokens 

                //  Current assumption, there can only be one plain in here.
                //  if not, then another optimization has failed (ie multipath optimization)
                //  you can have multiple different plains in a plain-to-oneinset collision here, but not multiple times the same plain
                let mtcChr = ecl |> List.head 
                let t3 = MT.createSinglePath (ExactMatch { Char = mtcChr.ExactChar ; ListCheck = [mtcChr.Token]}) PointerToStateFinal

                let allNextPtrs = (ecl |> List.map(fun i -> i.IdSp.Id, i.Next)) @ (oisl |> List.map(fun i -> i.IdSp.Id, i.Next))

                let (ilstorig, xlstorig) = splitToIOrX st1lst st2lst (allNextPtrs  |> List.map(fun (i,_)-> i))
                let ilst = allNextPtrs |> List.filter(fun (i,_) -> ilstorig.Contains i) |> List.map(fun (_,i) -> i)
                let xlst = allNextPtrs |> List.filter(fun (i,_) -> xlstorig.Contains i) |> List.map(fun (_,i) -> i)
                let st3 = (t3.SinglePathPointerValue, ilst, xlst) :: st3lst
                let removeIds = ecl |> List.map(fun e -> e.IdSp.SinglePathPointerValue)
                filterStatePointers (st1lst, removeIds), filterStatePointers (st2lst, removeIds), st3
        ) (st1, st2, [])


    let refactorCollissionsForPathType (iterPtr : StatePointer) (nextPtr : StatePointer) (callBack: (SinglePathPointer list) -> (SinglePathPointer list) -> RefactorResult) =
        let iterNode = MT.lookup iterPtr
        let nextNode = MT.lookup nextPtr

        match (iterNode, nextNode) with
        |   (MultiPath mp1, MultiPath mp2)   -> callBack mp1.States mp2.States
        |   (MultiPath mp, SinglePath sp)    -> callBack mp.States [(sp.SinglePathPointer)] 
        |   (SinglePath sp, MultiPath mp)    -> callBack [(sp.SinglePathPointer)] mp.States 
        |   (SinglePath sp1, SinglePath sp2) -> callBack [(sp1.SinglePathPointer)] [(sp2.SinglePathPointer)] 
        |   _ -> Unrefactored



    let rec refactorRepeaterPathsRec (iterPtr : StatePointer) (nextPtr : StatePointer) (exitPtr:StatePointer) (repeatId:RepeatId) : RefactorResult =

        let refactorCollisionsGeneric (st1:SinglePathPointer list) (st2:SinglePathPointer list) =
            //  These scenario are refactor candidates:
            //  (mp = multipath, sp = singlepath, ns = next step)
            //  
            //  Scenario 1:
            //  Repeat Start -> |I  -> sp -> ns1 -> Next()
            //                  |X  -> mp -> ns2 -> End
            //
            //  Scenario 2:
            //  Repeat Start -> |I  -> mp -> -> ns3 -> Next()
            //                  |X  -> sp -> -> ns4 -> End
            //
            //  Scenario 3:
            //  Repeat Start -> |I  -> mp1 -> ns5 -> Next()
            //                  |X  -> mp2 -> ns6 -> End
            //
            //  Splitting up the commonalities in the I- and X- path, in the RepeatIterOrExit step
            //  results into these generic step-types:
            //
            //  st1: a stripped sp/mp that continues the I-path, stripped from overlapping singlepaths, stripped-empty's removed
            //  st2: a stripped sp/mp that continues the X-path, stripped from overlapping singlepaths, stripped-empty's removed
            //  st3: a new construct with merged sp's, that continues to a next I/X choice
            //
            //  Note: all step types are paths or collections of paths; this detail is invisible in the following.
            //
            //  The generic construct after refactoring becomes this, when not(st1.IsEmpty) and not(st2.IsEmpty):
            //  Repeat Start -> |st3 -> |I -> (ns1|ns3|ns5)  :1   
            //                  |       |X -> (ns2|ns4|ns6)  :2
            //                  ||I  -> st1 -> (ns1|ns3|ns5) :3
            //                  ||X  -> st2 -> (ns2|ns4|ns6) :4
            //
            //  This construct must be simplifie for the following cases:
            //
            //  If st1.IsEmpty and st2.IsEmpty then :3 and :4 are discarded:
            //  Repeat Start -> |st3 -> |I -> (ns1|ns3|ns5)  :1   
            //                  |       |X -> (ns2|ns4|ns6)  :2
            //  
            //  If st1.IsEmpty and not(st2.IsEmpty) then :3 is discarded:
            //  Repeat Start -> |st3 -> |I -> (ns1|ns3|ns5)  :1   
            //                  |       |X -> (ns2|ns4|ns6)  :2
            //                  |st2 -> (ns2|ns4|ns6)        :4
            //
            //  If not(st1.IsEmpty) and st2.IsEmpty then the tree is rearranged as follows
            //  Repeat Start -> |st1 -> |I -> (ns1|ns3|ns5)  :1     The I/X choice is added to check min/max loop constraints 
            //                  |       |X -> NoMatch        :2     on the I-path, while the X-path is invalid.
            //                  |st3 -> |I  -> (ns1|ns3|ns5) :3
            //                  |       |X  -> (ns2|ns4|ns6) :4
            //
            //  And, in those cases st1/st2/st3 are multipaths on the same level, these are combined to one.
            //  
            //  Scenario's where plain-to-plain is merged, or oneinset-to-oneinset is merged, fit nice into the above scenario's,
            //  because they can easily (lineair) be split into donator and receiving states.
            //
            //  Where plain-to-oneinset merges happen, it becomes more complex, because there can be multiple receivers states.
            //
            //  The following scenario's apply for plain-to-oneinset merges:
            //  Scenario 1, merge canditate (A):
            //  Repeat Start -> |I -> A    -> np1 -> Next()
            //                  |X -> [AC] -> np2
            //
            //  This is the st1.IsEmpty & not (st2.IsEmpty) scenario from above, it refactors to:
            //  (st3)  Repeat Start -> |A -> |I -> np1 -> Next()
            //  (st3)                  |     |X -> np2
            //  (st2)                  |[C] -> np2      (match [C] sure exits loop)
            //                   
            //
            //  Scenario 2, , merge canditate (A):
            //  Repeat Start -> |I -> [AC] -> np1 -> Next()
            //                  |X -> A    -> np2
            //
            //  This is the not(st1.IsEmpty) & st2.IsEmpty scenario from above, it refactors to:
            //  (st1) Repeat Start -> |[C]  -> |I -> np1 -> Next()   RepeatIterOrExit is Required to check iteration-constraints (min/max).
            //  (st1)                 |        |X -> NoMatch         
            //  (st3)                 |A    -> |I -> np1 -> Next()
            //                                 |X -> np2
            //
            //  Scenario 3, I/X crosswise overlap , merge canditates (A,B):
            //  Repeat Start -> |I -> |[AC] -> np1 -> Next()
            //                  |     |B    -> np2 
            //                  |X -> |A    -> np3 
            //                  |     |[BD] -> np4
            //
            //  This is the not(st1.IsEmpty) & not(st2.IsEmpty) scenario from above, it refactors to:
            //  (st3)  Repeat Start -> |A    -> |I -> np1 -> Next()
            //  (st3)                  |        |X -> np3
            //  (st3)                  |B    -> |I -> np2
            //  (st3)                  |        |X -> np4
            //  (st1)                  ||I   -> |[C]  -> np1 -> Next()
            //  (st2)                  ||X   -> |[D]  -> np4                

            //
            //  Scenario 4 adds a unaffected path to scenario 3:
            //  Repeat Start -> |I -> |[AC] -> np1 -> Next()
            //                  |     |B    -> np2 
            //                  |X -> |A    -> np3 
            //                  |     |[BD] -> np4
            //                  |     |F    -> np5      unaffected!
            //
            //  This is the not(st1.IsEmpty) & not(st2.IsEmpty) scenario from above, it refactors to:
            //  (st3)  Repeat Start -> |A    -> |I -> np1 -> Next()
            //  (st3)                  |        |X -> np3
            //  (st3)                  |B    -> |I -> np2
            //  (st3)                  |        |X -> np4
            //  (st1)                  ||I   -> |[C] -> np1 -> Next()
            //  (st2)                  ||X   -> |[D] -> np4                
            //  (st2)                  |        |F   -> np5      unaffected!
            //
            //  Scenario 5 adds a unaffected path to scenario 4:
            //  Repeat Start -> |I -> |[AC] -> np1 -> Next()
            //                  |     |B    -> np2 
            //                  |     |G    -> np6      unaffected! 
            //                  |X -> |A    -> np3 
            //                  |     |[BD] -> np4
            //                  |     |F    -> np5      unaffected!
            //
            //  This is the not(st1.IsEmpty) & not(st2.IsEmpty) scenario from above, it refactors to:
            //  (st3)  Repeat Start -> |A    -> |I -> np1 -> Next()
            //  (st3)                  |        |X -> np3
            //  (st3)                  |B    -> |I -> np2
            //  (st3)                  |        |X -> np4
            //  (st1)                  ||I   -> |[C] -> np1 -> Next()
            //  (st1)                  |        |G   -> np6     unaffected!
            //  (st2)                  ||X   -> |[D] -> np4                
            //  (st2)                  |        |F   -> np5     unaffected!
            //  
            //  This shows that st3 has a special property: 
            //      Each element in st3 must know it's successor I- and X-path for refactoring, wich comes from its origin.
            //
            let (st1clean, st2clean, st3andIAndX) = refactorAndSeperateSinglePaths st1 st2

            if st3andIAndX.Length = 0 then 
                Unrefactored
            else
                let st1Next = st1 |> List.map(fun i -> MT.lookup i |> MT.Next) |> List.distinct
                let st2Next = st2 |> List.map(fun i -> MT.lookup i |> MT.Next) |> List.distinct

                let st3andIAndX = optimizeOis st3andIAndX

                let st3 =
                    st3andIAndX
                    |>  List.fold(fun st3New (i, ilst,xlst) ->
                        let RIPath = createAndSimplifyMultiPath ilst
                        let RXPath = createAndSimplifyMultiPath xlst

                        let repIoE = 
                            let potNxt = refactorRepeaterPathsRec RIPath RXPath exitPtr repeatId
                            match potNxt with
                            |   Unrefactored -> MT.createRepeatIterOrExit RIPath repeatId RXPath
                            |   Refactored single -> single
                        let i = MT.setNextState repIoE i.StatePointer
                        i.SinglePathPointerValue::st3New
                    ) []

                match (st1clean.Length, st2clean.Length) with
                |   (0, 0) ->  
                    //  Repeat Start -> |st3 -> |I -> (ns1|ns3|ns5)  :1   
                    //                  |       |X -> (ns2|ns4|ns6)  :2
                    let root = createAndSimplifyMultiPathSp st3
                    Refactored root
                |   (0, _) ->
                    //  Repeat Start -> |st3 -> |I -> (ns1|ns3|ns5)  :1   
                    //                  |       |X -> (ns2|ns4|ns6)  :2
                    //                  |st2 -> (ns2|ns4|ns6)        :4
                    let root = createAndSimplifyMultiPathSp (st3 @ st2clean)

                    Refactored root
                |   (_,0)   ->
                    //  Repeat Start -> |st1 -> |I -> (ns1|ns3|ns5)  :1   
                    //                  |       |X -> NoMatch        :2
                    //                  |st3 -> |I  -> (ns1|ns3|ns5) :3
                    //                  |       |X  -> (ns2|ns4|ns6) :4
                    
                    let st1NextState = createAndSimplifyMultiPath st1Next
                    let repIoNm = MT.createRepeatIterOrExit st1NextState repeatId (MT.noMatch())

                    let root = createAndSimplifyMultiPathSp (st3 @ st1clean)
                    Refactored (root.StatePointer)
                |   (_,_)   ->
                    //  Repeat Start -> |st3 -> |I -> (ns1|ns3|ns5)  :1   
                    //                  |       |X -> (ns2|ns4|ns6)  :2
                    //                  ||I  -> st1 -> (ns1|ns3|ns5) :3
                    //                  ||X  -> st2 -> (ns2|ns4|ns6) :4

                    let st1Cleared = createAndSimplifyMultiPathSp st1clean
                    let st2Cleared = createAndSimplifyMultiPathSp st2clean
                    let repOld =  MT.createRepeatIterOrExit st1Cleared.StatePointer repeatId st2Cleared.StatePointer

                    let root = createAndSimplifyMultiPathSp (repOld.SinglePathPointerValue::st3)
                    Refactored root

        refactorCollissionsForPathType iterPtr nextPtr refactorCollisionsGeneric

    let refactorRepeaterPaths (ep:EmptyPath) =
        match MT.lookup ep.NextState with
        |   RepeatIterOrExit re -> refactorRepeaterPathsRec (re.IterateState.StatePointer) (re.NextState.StatePointer) (re.StatePointer) re.RepeatId
        | _ -> Unrefactored


    let rec refactorNestedRepeatersRec (iterPtr : StatePointer) (nextPtr : StatePointer) (exitPtr:StatePointer) (repeatIdouterLoop:RepeatId) : RefactorResult =

        let rec refactorInnerLoopIterPathCollisions (repeatInnerLoop:RepeatIterateOrExit) (st1:SinglePathPointer list) (st2:SinglePathPointer list) =
            let (st1clean, st2clean, st3andIAndX) = refactorAndSeperateSinglePaths st1 st2 
            if st3andIAndX.Length > 0 then
                let st3refactored =
                    st3andIAndX |>
                    List.map(fun (st3, st3NextILst, st3NextXLst) ->
                        let st3NextI = createAndSimplifyMultiPath st3NextILst
                        let st3NextX = createAndSimplifyMultiPath st3NextXLst

                        let iPathInnerLoopCheck = MT.createRepeatIterOrExit st3NextI repeatInnerLoop.RepeatId (MT.noMatch())
                        let iPathOuterLoopCheck = MT.createRepeatIterOrExit iPathInnerLoopCheck.StatePointer repeatIdouterLoop st3NextX

                        let st3node = MT.setNextState iPathOuterLoopCheck st3

                        let xPthNxt = MT.lookup repeatInnerLoop.NextState

                        let xPathInnerLoopCheck = MT.createRepeatIterOrExit (MT.noMatch()) repeatInnerLoop.RepeatId xPthNxt.NextStatePtr
                        let xPathOuterLoopCheck = MT.createRepeatIterOrExit xPathInnerLoopCheck repeatIdouterLoop (MT.noMatch())
                        
                        let xPthDup = MT.duplicateAndLinkToNext xPathOuterLoopCheck repeatInnerLoop.NextState
                        [st3node; xPthDup]
                    )
                    
                let st1Refactored =
                    st1clean
                    |>  List.map(fun st1 ->
                        let st1Next = MT.Next st1

                        let xPathInnerLoopCheck = MT.createRepeatIterOrExit st1Next repeatInnerLoop.RepeatId (MT.noMatch())
                        let xPathOuterLoopCheck = MT.createRepeatIterOrExit xPathInnerLoopCheck repeatIdouterLoop (MT.noMatch())

                        let st1Dup = MT.duplicateAndLinkToNext xPathOuterLoopCheck st1.StatePointer
                        [st1Dup]
                    )
                let st2Refactored =
                    st2clean
                    |>  List.map(fun st2 ->
                        let st2Next = MT.Next st2

                        let xPathOuterLoopCheck = MT.createRepeatIterOrExit (MT.noMatch()) repeatIdouterLoop  st2Next

                        let st1Dup = MT.duplicateAndLinkToNext xPathOuterLoopCheck st2.StatePointer
                        [st1Dup]
                    )

                let allRefactoredNode = st3refactored @ st1Refactored @ st2Refactored |> List.collect id
                Refactored(createAndSimplifyMultiPath allRefactoredNode)
            else
                Unrefactored


        let rec refactorInnerLoopExitPathCollisions (repeatInnerLoop:RepeatIterateOrExit) (st1:SinglePathPointer list) (st2:SinglePathPointer list) =
            let (st1clean, st2clean, st3andIAndX) = refactorAndSeperateSinglePaths st1 st2 
            if st3andIAndX.Length > 0 then
                let st3refactored =
                    st3andIAndX |>
                    List.map(fun (st3, st3NextILst, st3NextXLst) ->
                        let st3NextI = createAndSimplifyMultiPath st3NextILst
                        let st3NextX = createAndSimplifyMultiPath st3NextXLst

                        let iPathInnerLoopCheck = MT.createRepeatIterOrExit (MT.noMatch()) repeatInnerLoop.RepeatId st3NextI
                        let iPathOuterLoopCheck = MT.createRepeatIterOrExit iPathInnerLoopCheck.StatePointer repeatIdouterLoop st3NextX

                        let st3node = MT.setNextState iPathOuterLoopCheck st3

                        let iPthNxt = MT.lookup repeatInnerLoop.IterateState

                        let iPathInnerLoopCheck = MT.createRepeatIterOrExit iPthNxt.NextStatePtr repeatInnerLoop.RepeatId (MT.noMatch()) 
                        let iPathOuterLoopCheck = MT.createRepeatIterOrExit iPathInnerLoopCheck repeatIdouterLoop (MT.noMatch())
                        
                        let iPthDup = MT.duplicateAndLinkToNext iPathOuterLoopCheck repeatInnerLoop.IterateState
                        [st3node; iPthDup]
                    )
                    
                let st1Refactored =
                    st1clean
                    |>  List.map(fun st1 ->
                        let st1Next = MT.Next st1

                        let xPathInnerLoopCheck = MT.createRepeatIterOrExit (MT.noMatch()) repeatInnerLoop.RepeatId st1Next 
                        let xPathOuterLoopCheck = MT.createRepeatIterOrExit xPathInnerLoopCheck repeatIdouterLoop (MT.noMatch())

                        let st1Dup = MT.duplicateAndLinkToNext xPathOuterLoopCheck st1.StatePointer
                        [st1Dup]
                    )

                let st2Refactored =
                    st2clean
                    |>  List.map(fun st2 ->
                        let st2Next = MT.Next st2

                        let xPathOuterLoopCheck = MT.createRepeatIterOrExit (MT.noMatch()) repeatIdouterLoop  st2Next
                        let st1Dup = MT.duplicateAndLinkToNext xPathOuterLoopCheck st2.StatePointer
                        [st1Dup]
                    )

                let allRefactoredNode = st3refactored @ st1Refactored @ st2Refactored |> List.collect id
                Refactored(createAndSimplifyMultiPath allRefactoredNode)
            else
                Unrefactored

        let refactorCollisionsGeneric (st1:SinglePathPointer list) (st2:SinglePathPointer list)  =
            let (st1clean, st2clean, st3andIAndX) = refactorAndSeperateSinglePaths st1 st2 
            if st3andIAndX.Length = 0 then 
                Unrefactored
            else
                
                Unrefactored

        match MT.lookup iterPtr with
        |   RepeatIterOrExit roi2 ->
            let rfri = refactorCollissionsForPathType roi2.IterateState nextPtr (refactorInnerLoopIterPathCollisions roi2)
            match rfri with
            |   Unrefactored ->
                refactorCollissionsForPathType roi2.NextState nextPtr (refactorInnerLoopExitPathCollisions roi2)
            |   Refactored _ -> rfri
        |   _ -> Unrefactored
        


    let refactorNestedRepeater (outerLoop:RepeatInit) (ep:EmptyPath) =
        match MT.lookup ep.NextState with
        |   RepeatIterOrExit reOuterloop ->
            match MT.lookup reOuterloop.IterateState with
            |   RepeatInit innerLoop ->  
                match MT.lookup innerLoop.NextState with
                |   RepeatStart rs2 ->
                    refactorNestedRepeatersRec (rs2.NextState.StatePointer) (reOuterloop.NextState.StatePointer) (reOuterloop.StatePointer) reOuterloop.RepeatId
                    |>  function
                        |   Refactored single -> 
                            MT.setNextState single reOuterloop.IterateState |> ignore
                            Refactored reOuterloop.IterateState
                        |   Unrefactored -> Unrefactored
                |   _ -> Unrefactored
            |   _ -> Unrefactored
        | _ -> Unrefactored


    match MT.lookup start with
    |   RepeatInit repInit -> 
        let (RepeatStart ep) = MT.lookup repInit.NextState
        let refac = refactorRepeaterPaths ep
        match refac with
        |   Refactored single    ->  //  was refactored
            MT.setNextState single ep |> ignore
            start
        |   Unrefactored     ->  //  was not refactored
            let refac = refactorNestedRepeater repInit ep
            match refac with
            |   Refactored single    ->  //  was refactored
                MT.setNextState single ep |> ignore
                start
            |   Unrefactored     ->  //  was not refactored
                start
    |   _ -> start



let convertRepeaterToExplicitTree (start:StatePointer) =

    let convertRepeaterToExplicitTree (ri:RepeatInit) (rs:EmptyPath) (rioe:RepeatIterateOrExit) (rt:RepeatState) =
        if rt.Max > 0 then
            [rt.Max .. -1 .. 1]
            |>  List.fold(fun nxt cur ->
                let iterPath = duplicateStructureAndLinkToNext [rs.StatePointer] [(rs.StatePointer, nxt)] rioe.IterateState nxt

                if cur > rt.Min then
                    (MT.getSinglePathPointers iterPath) @ (MT.getSinglePathPointers rioe.NextState)
                    |>  refactorConflictingCharacterSets
                    |>  refacorConflictingPlainWithCharacterSets
                    |>  refactorCommonPlains
                    |>  MT.createAndSimplifyMultiPathSp
                else
                    iterPath
            ) rioe.NextState
        else
            let finalPath =
                let loopStart = MT.createRepeatStart PointerToStateFinal
                let iterDup = duplicateStructureAndLinkToNext [rs.StatePointer] [(rs.StatePointer, loopStart)] rioe.IterateState loopStart
                let mpo =
                    [iterDup.SinglePathPointerValue;rioe.NextState.SinglePathPointerValue]
                    |>  refactorConflictingCharacterSets
                    |>  refacorConflictingPlainWithCharacterSets
                    |>  refactorCommonPlains
                    |>  MT.createAndSimplifyMultiPathSp
                MT.setNextState mpo loopStart

            [rt.Min .. -1 .. 1]
            |>  List.fold(fun nxt _ ->
                duplicateStructureAndLinkToNext [rs.StatePointer] [(rs.StatePointer, nxt)] rioe.IterateState nxt
            ) finalPath
        

    match MT.lookup start with
    |   RepeatInit repInit -> 
        let (RepeatStart ep) = MT.lookup repInit.NextState
        match MT.lookup ep.NextState with
        |   RepeatIterOrExit re -> 
            let nxt = convertRepeaterToExplicitTree repInit ep re (MT.getRepeatState repInit.RepeatId)
            //MT.setNextState nxt repInit.NextState
            //repInit.NextState
            nxt
        | _ -> start
    |   _ -> start


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
            |   RepeatInit rs -> traverse (rs.NextState.StatePointer) (passedNodes.Add (rs.Id)) (rs.Id::used)
            |   RepeatIterOrExit   re  -> 
                let ri = traverse (re.IterateState) (passedNodes.Add (re.Id)) (re.Id::used) 
                traverse (re.NextState) (passedNodes.Add (re.Id)) (re.Id::ri)
            |   RepeatStart ep -> traverse (ep.NextState) (passedNodes.Add (ep.Id)) (ep.Id::used)
            |   NoMatch d -> current.Id::used
    let usedLst =
        traverse nfa.Start Set.empty []
        |>  List.distinct
        |>  Set.ofList
    let nodes = nfa.States |> List.filter(fun n -> usedLst.Contains n.Id)
    {nfa with States = nodes}


let rgxToNFA rgx =
    MT.Init()

    let rec processConversion rgx : StatePointer =
        let convert rg : StatePointer =
            match rg with
            |   Plain pl   -> if pl.``fixed``.Length > 1 then processConversion rg else createSinglePathFromRgx rg
            |   OneInSet _ -> createSinglePathFromRgx rg
            |   _ -> processConversion rg

        let createRepeat o min max =
            let linkState = MT.createEmptyPath PointerToStateFinal
            let repPath = convert o
            let repState = MT.createRepeatState min max
            let repExit = MT.createRepeatIterOrExit repPath repState.RepeatId linkState
            let repeatLoopStart = MT.createRepeatStart repExit
            
            appendStateIdToAllFinalPathNodes repPath repeatLoopStart

            let repStart = MT.createRepeatInit repState.RepeatId repeatLoopStart
            repStart 

        match rgx with
        |   Plain  pl ->
            if pl.``fixed``.Length > 1 then
                pl.OptimizeOnce()
                processConversion (Concat (pl.optimized))
            else
                failwith "Uncontained plain - not implemented yet"
        |   Concat l -> 
            let linkState = MT.createEmptyPath PointerToStateFinal
            let converts =
                l
                |>  List.map(convert)

            converts
            |>  List.fold(fun (concatPtr:StatePointer) (entryStart:StatePointer) ->
                    appendStateIdToAllFinalPathNodes entryStart concatPtr
                    convertRepeaterToExplicitTree entryStart
                    ) linkState
        |   Or     l -> 
            l
            |>  List.map(convert)
            |>  List.map(fun sp -> sp.SinglePathPointerValue)
            |>  List.fold(fun sil sp ->
                    let mpl = 
                        MT.lookup sp
                        |>  function
                            |   MultiPath  mp -> mp.States
                            |   _             -> []
                    (sp :: sil) @ mpl
            ) []
            |>  refactorConflictingCharacterSets
            |>  refacorConflictingPlainWithCharacterSets
            |>  refactorCommonPlains
            |>  MT.createAndSimplifyMultiPathSp
        |   Optional    r -> createRepeat r 0 1
        |   ZeroOrMore  r -> createRepeat r 0 0
        |   OneOrMore   r -> createRepeat r 1 0
        |   _ -> failwith "Not Implemented Yet"
    processConversion rgx
    |>  MT.ToNFAMachine
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
                    |   RepeatInit rs ->
                        let rt = rsMap.[rs.RepeatId]
                        printf "->>(%2d:<%2d,%2d>)" rs.Id rt.Min rt.Max
                        printLineRest (LevelType.LoopStart :: hist) (rs.NextState.StatePointer) (passedNodes.Add rs.Id)
                    |   RepeatStart rs ->
                        printf "L(%2d)" rs.Id
                        printLineRest (LevelType.Empty :: hist) rs.NextState (passedNodes.Add rs.Id)
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
                    |   NoMatch d -> printf "-NoMatch(%2d)\n" d
                else
                    printf "-Loop(%2d)\n" current.Id

        printLineRest hist current passedNodes
    printLine [] nfa.Start passedNodes


let parseIt (nfa:NFAMachine) yaml =
    let stMap = nfa.States |> List.fold(fun (m:Map<_,_>) i -> m.Add(i.Id, i)) Map.empty<StateId, StateNode>
    let stRepeat = nfa.Repeats |> List.fold(fun (m:Map<_,_>) i -> m.Add(i.RepeatId, i)) Map.empty<RepeatId, RepeatState>
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
    let runningLoops = Map.empty<RepeatId, RepeatState>

    let NoMatch = { IsMatch = false ; FullMatch = [] }

    let rec processStr currentChar cs acc rollback (runningLoops : Map<RepeatId, RepeatState>) =
        let processCurrentChar = processStr currentChar
        let processNextChar cs acc rollback (runningLoops : Map<RepeatId, RepeatState>)= 
            let chk = (stream.Get())
            processStr chk cs acc rollback runningLoops 

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
                let pos = stream.Position
                let rec parseMultiPath (sptrLst:SinglePathPointer list) =
                    match sptrLst with 
                    |   []      -> NoMatch 
                    |   h::tail ->
                        match stMap.[h.Id] with
                        |   SinglePath st -> 
                            if st.State.Match currentChar then
                                processNextChar (st.NextState) (currentChar.Source.[0] :: acc) rollback runningLoops
                            else
                                NoMatch
                        |   EmptyPath  st  -> processCurrentChar (st.NextState) acc (rollback+1) runningLoops
                        |   RepeatStart st  -> processCurrentChar (st.NextState) acc (rollback+1) runningLoops
                        |   RepeatIterOrExit re  -> processCurrentChar re.StatePointer acc rollback runningLoops
                        |   _ -> failwith "Not implemented yet"
                        |>  fun res -> 
                            if not(res.IsMatch) && stream.Position = pos  then
                                parseMultiPath tail
                            else res

                parseMultiPath p.States
            |   EmptyPath p -> processCurrentChar p.NextState acc rollback runningLoops
            |   RepeatStart p -> processCurrentChar p.NextState acc rollback runningLoops
            |   RepeatInit r ->
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
                    let pos = stream.Position
                    let rlNew = runningLoops.Remove(r.RepeatId).Add(r.RepeatId, rs.Iterate())
                    let tryIterate = processCurrentChar r.IterateState acc rollback rlNew
                    if tryIterate.IsMatch then
                        tryIterate
                    else
                        if stream.Position = pos && rs.CanExit() && r.IterateState<>r.NextState then
                            processCurrentChar r.NextState acc rollback (rlNew.Remove rs.RepeatId) // end loop by removing it from running loops
                        else
                            NoMatch
            |   NoMatch _ -> NoMatch

    processStr (stream.Get()) nfa.Start [] 0 runningLoops

let clts (cl:char list) = System.String.Concat(cl)

module ParseResult =
    let IsMatch pr = pr.IsMatch
    let FullMatch pr = pr.FullMatch


