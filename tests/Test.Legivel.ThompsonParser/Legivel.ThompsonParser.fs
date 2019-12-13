module Legivel.ThompsonParser

open Legivel.Utilities.RegexDSL

open Legivel.Tokenizer
open System.Drawing
open System.Diagnostics
open NUnit.Framework
open FsUnitTyped
open TestUtils
open System.Text.RegularExpressions
open System.Text


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


type GroupId = | GroupId of System.UInt32
with
    member this.Id 
        with get() = 
            let (GroupId i) = this
            i

type GosubId = | GosubId of System.UInt32
with
    member this.Id
        with get() =
            let (GosubId i) = this
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


type RepeatStateRef = {
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

type GroupState = {
    Id         : StateId
    NextState  : StatePointer
    GroupId    : GroupId
}
with
    static member Create id nx gid = { Id = id; NextState = nx; GroupId = gid }
    member this.LinkTo i = { this with NextState = i}
    member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
    member this.SinglePathPointer = SinglePathPointer.Create this.Id
    member this.Duplicate i = {this with Id = i}


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


type GoSubPath = {
    Id          : StateId
    NextState   : StatePointer 
    ReturnState : StatePointer
    GosubId     : GosubId
}
with
    static member Create id nx rs gsi = { Id = id; NextState = nx; ReturnState = rs; GosubId = gsi }
    member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
    member this.SinglePathPointer = SinglePathPointer.Create this.Id
    member this.Duplicate i = {this with Id = i}


type ReturnPath = {
    Id          : StateId
    GosubId     : GosubId
}
with
    static member Create id gsi = { Id = id; GosubId = gsi }
    member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
    member this.SinglePathPointer = SinglePathPointer.Create this.Id
    member this.Duplicate i = {this with Id = i}


let PointerToStateFinal = SinglePathPointer (SinglePathPointer.Create 0u) 


[<StructuredFormatDisplay("{AsString}")>]
type StateNode =
    |   SinglePath of SinglePath
    |   StartLinePath of EmptyPath
    |   MultiPath  of MultiPath
    |   EmptyPath  of EmptyPath
    |   RepeatInit of RepeatStateRef
    |   RepeatStart of RepeatStateRef
    |   RepeatIterOrExit of RepeatIterateOrExit
    |   GroupStart of GroupState
    |   GroupEnd of GroupState
    |   GoSub of GoSubPath
    |   ReturnSub of ReturnPath
    |   NoMatch of StateId
    with
        member this.Id 
            with get() =
                match this with
                |   SinglePath d -> d.Id
                |   StartLinePath d -> d.Id
                |   MultiPath  d -> d.Id
                |   EmptyPath  d -> d.Id
                |   RepeatInit d -> d.Id
                |   RepeatStart d -> d.Id
                |   RepeatIterOrExit  d -> d.Id
                |   GroupStart  d -> d.Id
                |   GroupEnd    d -> d.Id
                |   GoSub       d -> d.Id
                |   ReturnSub   d -> d.Id
                |   NoMatch     d -> d

        member this.IsEmptyPathValue 
            with get() =
                match this with
                |   EmptyPath _ -> true
                |   _ -> false

        member this.NextState 
            with get() =
                match this with
                |   SinglePath d -> d.NextState.Id
                |   StartLinePath d -> d.NextState.Id
                |   EmptyPath  d -> d.NextState.Id
                |   RepeatInit d -> d.NextState.Id
                |   RepeatStart d -> d.NextState.Id
                |   RepeatIterOrExit  d -> d.NextState.Id
                |   GroupStart  d -> d.NextState.Id
                |   GroupEnd    d -> d.NextState.Id
                |   GoSub       d -> d.ReturnState.Id
                |   ReturnSub  _ -> failwith "ReturnSub has no single nextstate"
                |   MultiPath  _ -> failwith "Multipath has no single nextstate"
                |   NoMatch _ -> failwith "NoMatch has no single nextstate"

        member this.NextStatePtr 
            with get() =
                match this with
                |   SinglePath      d -> d.NextState
                |   StartLinePath   d -> d.NextState
                |   EmptyPath       d -> d.NextState
                |   RepeatInit      d -> d.NextState.StatePointer
                |   RepeatStart     d -> d.NextState
                |   RepeatIterOrExit d -> d.NextState
                |   GroupStart      d -> d.NextState
                |   GroupEnd        d -> d.NextState
                |   GoSub           d -> d.ReturnState
                |   ReturnSub  _ -> failwith "ReturnSub has no single nextstate"
                |   MultiPath  _ -> failwith "Multipath has no single nextstate"
                |   NoMatch    _ -> failwith "NoMatch has no single nextstate"


        member this.SetNextState i =
                match this with
                |   SinglePath      d -> SinglePath { d with NextState = i }
                |   StartLinePath   d -> StartLinePath { d with NextState = i }
                |   EmptyPath       d -> EmptyPath { d with NextState = i }
                |   RepeatStart     d -> RepeatStart { d with NextState = i }
                |   RepeatIterOrExit d -> RepeatIterOrExit  { d with NextState = i }
                |   RepeatInit      d -> RepeatInit { d with NextState = i }
                |   GroupStart      d -> GroupStart { d with NextState = i }
                |   GoSub           d -> GoSub { d with ReturnState = i }
                |   GroupEnd        d -> GroupEnd { d with NextState = i }
                |   _ -> failwith "Illegal to set nextstate"

        member this.StatePointer  
            with get() =
                match this with
                |   SinglePath d -> d.StatePointer
                |   StartLinePath d -> d.StatePointer
                |   MultiPath  d -> d.StatePointer
                |   EmptyPath  d -> d.StatePointer
                |   RepeatInit d -> d.StatePointer
                |   RepeatStart d -> d.StatePointer
                |   RepeatIterOrExit  d -> d.StatePointer
                |   GroupStart  d -> d.StatePointer
                |   GroupEnd    d -> d.StatePointer
                |   GoSub       d -> d.StatePointer
                |   ReturnSub   d -> d.StatePointer
                |   NoMatch d -> SinglePathPointer (SinglePathPointer.Create d)

        member this.SinglePathPointer
            with get() =
                match this with
                |   SinglePath d -> d.SinglePathPointer
                |   StartLinePath d -> d.SinglePathPointer
                |   EmptyPath  d -> d.SinglePathPointer
                |   RepeatInit d -> d.SinglePathPointer
                |   RepeatStart d -> d.SinglePathPointer
                |   RepeatIterOrExit  d -> d.SinglePathPointer
                |   GroupStart  d -> d.SinglePathPointer
                |   GroupEnd    d -> d.SinglePathPointer
                |   GoSub       d -> d.SinglePathPointer
                |   ReturnSub   d -> d.SinglePathPointer
                |   MultiPath  d -> failwith "Multipath has no SinglePathPointer"
                |   NoMatch d -> SinglePathPointer.Create d


        member this.AsString =
            match this with
            |   SinglePath d -> 
                match d.State with
                |   ExactMatch    em -> sprintf "Id: %d, SinglePath Exact: '%c', Next: %d" d.Id em.Char d.NextState.Id
                |   OneInSetMatch _  -> sprintf "Id: %d, SinglePath OiS, Next: %d" d.Id d.NextState.Id
            |   StartLinePath d -> sprintf "Id: %d, StartLinePath Next: %d" d.Id d.NextState.Id
            |   MultiPath  d -> sprintf "Id: %d, MultiPath, States: %O" d.Id (d.States |> List.map(fun i -> i.Id))
            |   EmptyPath  d -> sprintf "Id: %d, EmptyPath Next: %d" d.Id d.NextState.Id
            |   RepeatInit d -> sprintf "Id: %d, RepeatInit Next: %d" d.Id d.NextState.Id
            |   RepeatStart d -> sprintf "Id: %d, RepeatStart Next: %d" d.Id d.NextState.Id
            |   RepeatIterOrExit  d -> sprintf "Id: %d, RepeatIterOrExit, I: %d, E: %d" d.Id d.IterateState.Id d.NextState.Id
            |   GroupStart  d -> sprintf "Id: %d, GroupStart Next: %d" d.Id d.NextState.Id
            |   GroupEnd    d -> sprintf "Id: %d, GroupEnd Next: %d" d.Id d.NextState.Id
            |   NoMatch     d -> sprintf "Id: %d, NoMatch" d
            |   GoSub       d -> sprintf "Id: %d, GoSub Next: %d, Return: %d" d.Id d.NextState.Id d.ReturnState.Id
            |   ReturnSub   d -> sprintf "Id: %d, ReturnSub" d.Id


type NFAMachine = {
    Start   : StatePointer
    States  : StateNode list
    Repeats : RepeatState list
}
with
    static member Create (i, s, r) = { States = s; Start = i; Repeats = r}


type GroupParse = {
    RunningGroups : GroupId list
    GroupMatches  : Map<GroupId, char list>
}
with
    static member Create() = { RunningGroups = []; GroupMatches = Map.empty}

    member this.AddChar c =
        let ng =
            this.RunningGroups
            |>  List.fold(fun gm gi ->
                gm
                |>  Map.remove gi
                |>  Map.add gi (c :: gm.[gi])
            ) this.GroupMatches
        {this with GroupMatches = ng}

    member this.Start gi =
        { this with RunningGroups = gi :: this.RunningGroups; GroupMatches = this.GroupMatches |> Map.add gi []}

    member this.Stop gi =
        { this with RunningGroups = this.RunningGroups |> List.filter(fun i -> i<>gi)}


module MT = //    Match Tree
    let mutable private currentId = 0u
    let mutable private currentRepeatId = 0u
    let mutable private currentGroupId = 0u
    let mutable private currentGosubId = 0u

    let private CreateNewId() =
        currentId <- (currentId + 1u)
        currentId
    
    let private CreateNewRepeatId() =
        currentRepeatId <- (currentRepeatId + 1u)
        RepeatId currentRepeatId

    let private CreateGroupId() =
        currentGroupId <- (currentGroupId + 1u)
        GroupId currentGroupId 

    let CreateGosubId() =
        currentGosubId <- (currentGosubId + 1u)
        GosubId currentGosubId

    let mutable private allNodes = Map.empty<StateId, StateNode>
    let mutable private allRepeats = new System.Collections.Generic.List<RepeatState>()

    let Init() =
        currentId       <- 0u
        currentRepeatId <- 0u
        currentGroupId  <- 0u
        currentGosubId  <- 0u
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

    let createGroup() = CreateGroupId()

    let getNode i = allNodes.[i]

    let createSinglePath mt nx = SinglePath.Create (CreateNewId()) mt nx |> SinglePath |> addAndReturn
    let createStartLinePath nx = EmptyPath.Create (CreateNewId()) nx |> StartLinePath |> addAndReturn
    let createMultiPath mt = MultiPath.Create (CreateNewId()) mt |> MultiPath |> addAndReturn
    let createEmptyPath nx = EmptyPath.Create (CreateNewId()) nx |> EmptyPath |> addAndReturn
    let createRepeatInit ri nx = RepeatStateRef.Create (CreateNewId()) ri nx |> RepeatInit |> addAndReturn
    let createRepeatStart ri nx = RepeatStateRef.Create (CreateNewId()) ri nx |> RepeatStart |> addAndReturn
    let createRepeatIterOrExit it ri nx = RepeatIterateOrExit.Create (CreateNewId()) it ri nx |> RepeatIterOrExit |> addAndReturn
    let createGroupStart gid nx = GroupState.Create (CreateNewId()) nx gid |> GroupStart  |> addAndReturn
    let createGroupEnd gid nx = GroupState.Create (CreateNewId()) nx gid |> GroupEnd  |> addAndReturn
    let createGoSub nx rs rid = GoSubPath.Create (CreateNewId()) nx rs rid |> GoSub |> addAndReturn
    let createRetSub rid = ReturnPath.Create (CreateNewId()) rid |> ReturnSub |> addAndReturn
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
        |   StartLinePath p -> createStartLinePath p.NextState
        |   RepeatStart p -> createRepeatStart p.RepeatId p.NextState
        |   RepeatInit  p -> createRepeatInit p.RepeatId p.NextState
        |   RepeatIterOrExit p -> createRepeatIterOrExit p.IterateState p.RepeatId p.NextState
        |   GroupStart  p -> createGroupStart p.GroupId p.NextState 
        |   GroupEnd    p -> createGroupEnd   p.GroupId p.NextState
        |   GoSub       p -> createGoSub      p.NextState p.ReturnState p.GosubId
        |   ReturnSub   p -> createRetSub     p.GosubId
        |   MultiPath   p -> createMultiPath  p.States
        |   NoMatch     _ -> currPtr


    let duplicateAndLinkToNext (next:StatePointer) (currPtr : StatePointer) =
        match lookup currPtr with
        |   EmptyPath   _ -> createEmptyPath next
        |   SinglePath  p -> createSinglePath p.State next
        |   StartLinePath p -> createStartLinePath next
        |   RepeatStart p -> createRepeatStart p.RepeatId next
        |   RepeatInit  p -> createRepeatInit p.RepeatId next
        |   RepeatIterOrExit p -> createRepeatIterOrExit p.IterateState p.RepeatId next
        |   GroupStart  p -> createGroupStart p.GroupId next
        |   GroupEnd    p -> createGroupEnd   p.GroupId next
        |   GoSub       p -> createGoSub      p.NextState next p.GosubId
        |   ReturnSub   p -> createRetSub     p.GosubId
        |   NoMatch     _ -> currPtr
        |   MultiPath   _ -> failwith "duplicateAndLinkToNext cannot be applied to MultiPath"
        

    let SortStateNodes lst =
        let rec sortOrder c1 c2 =
            match (c1, c2) with
            |   (EmptyPath _,  MultiPath _) -> 1
            |   (StartLinePath _, _) -> -1
            |   (_, StartLinePath _) -> 1
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
            |   (GroupStart gs1, GroupStart gs2) -> sortOrder (gs1.NextState |> lookup) (gs2.NextState |> lookup)
            |   (GroupStart gs, something) -> sortOrder (gs.NextState |> lookup) something
            |   (MultiPath _,  GoSub _) -> 1
            |   (GoSub _,      MultiPath _) -> -1
            |   (MultiPath _,  ReturnSub _) -> 1
            |   (ReturnSub _,  MultiPath _) -> -1
            |   (SinglePath _, ReturnSub _) -> -1
            |   (ReturnSub _,  SinglePath _) -> 1
            |   (something,    GroupStart gs) -> sortOrder something (gs.NextState |> lookup)
            |   _ -> 0
        lst
        |>  List.sortWith sortOrder


    let cleanupEmptyPaths pt =
        let rec searchStart cp =
            match lookup cp with
            |   EmptyPath p when p.NextState.Id <> PointerToStateFinal.Id -> searchStart p.NextState
            |   _  -> cp

        searchStart pt


    let simplifyMultiPathStates (lst:StatePointer list) =
        lst 
        |> List.groupBy id 
        |> List.map fst
        |> List.filter(fun e -> e.Id <> PointerToStateFinal.Id)


    let getSinglePathPointers pt =
        match pt with 
        |   SinglePathPointer p -> [pt.SinglePathPointerValue]
        |   MultiPathPointer  p -> 
            match lookup pt with
            |   MultiPath mp -> mp.States
            |   _ -> failwith "Expected MultiPath"

    let createAndSimplifyMultiPath (lst:StatePointer list) : StatePointer =
        match lst.Length with
        |   0 -> createEmptyPath PointerToStateFinal
        |   1 -> (lst |> List.head).StatePointer
        |   _ ->
            let sorted =
                lst
                |>  List.map cleanupEmptyPaths
                |>  List.map lookup
                |>  SortStateNodes
                |>  List.map(fun e -> e.StatePointer)
                |>  List.map getSinglePathPointers
                |>  List.collect id
            let mn = createMultiPath sorted
            mn


    let createAndSimplifyMultiPathSp (lst:SinglePathPointer list) : StatePointer  =
        createAndSimplifyMultiPath (lst |> List.map(fun i -> i.StatePointer))

    let getRepeatState (ri:RepeatId) = allRepeats |> Seq.find(fun e -> e.RepeatId = ri)




let qcOneInSet ls = ls |> List.fold(fun s i -> s ||| uint32(i)) 0u

let createSinglePathFromRgx rgx =
    match rgx with
    |   Plain d when d.``fixed`` = "^" && d.Token = [Token.NoToken] -> MT.createStartLinePath PointerToStateFinal
    |   Plain d when d.``fixed`` = "" && d.Token = [Token.EOF] -> 
        let listCheck = [Token.EOF]
        let quickCheck = qcOneInSet listCheck
        MT.createSinglePath (OneInSetMatch({ QuickCheck = quickCheck; ListCheck = listCheck})) PointerToStateFinal
    |   Plain d when d.``fixed`` = "" && d.Token = [] -> MT.createEmptyPath PointerToStateFinal
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


type RefactorResult =
    |   Refactored of StatePointer
    |   Unrefactored


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


module Duplication = 
    type private DuplicateParam = {
        Current:        StatePointer
        PassedNodes:    Set<StateId>
        PreMapped:      Map<StatePointer, StatePointer>
        SourceToTarget : Map<StatePointer, StatePointer>
    }
    with    
        static member Create c pn pm st = { Current = c; PassedNodes = pn; PreMapped = pm; SourceToTarget = st }

    type private DuplicateReturn = {
        Link           : StatePointer
        SourceToTarget : Map<StatePointer, StatePointer>
    }
    with
        static member Create st l = { Link = l; SourceToTarget = st}

    module private DuplicateParam =
        let SetCurrent c (p:DuplicateParam)  = { p with Current = c }
        let SetStT     st (p:DuplicateParam) = { p with SourceToTarget = st}
        let AddPassedNode n (p:DuplicateParam) = { p with PassedNodes = p.PassedNodes |> Set.add n}
        let AddPreMapped  k m (p:DuplicateParam) = { p with PreMapped = p.PreMapped |> Map.add k m}

    module private DuplicateReturn =
        let LinkTargetToSource s (r:DuplicateReturn) = 
            if r.SourceToTarget.ContainsKey s then r 
            else { r with SourceToTarget = r.SourceToTarget |> Map.add s r.Link}

    let duplicateStructureAndLinkToNext (passed:StatePointer list) (premapped:(StatePointer*StatePointer) list) (entryStartId:StatePointer) (concatPtr:StatePointer) =
        let passedNodes = 
            passed
            |>  List.map(fun i -> i.Id)
            |>  List.distinct
            |>  Set.ofList
        
        let preMappedNodes = Map.ofList premapped

        let dup (p:DuplicateParam) (r:DuplicateReturn) = 
            let tr = 
                if p.PreMapped.ContainsKey r.Link then p.PreMapped.[r.Link]
                else r.Link
            MT.duplicateAndLinkToNext tr p.Current
            |>  DuplicateReturn.Create p.SourceToTarget
            |>  DuplicateReturn.LinkTargetToSource p.Current


        let rec traverse (p:DuplicateParam) : DuplicateReturn =
            let inline passthrough (d:^A) =
                p
                |>  DuplicateParam.SetCurrent (^A : (member NextState : StatePointer) d)
                |>  traverse 
                |>  dup p

            if  p.Current.Id = 0u || p.Current.Id = concatPtr.Id || p.PassedNodes.Contains (p.Current.Id) then 
                if p.PreMapped.ContainsKey p.Current then p.PreMapped.[p.Current] else p.Current
                |>  DuplicateReturn.Create p.SourceToTarget
            else
                let node = MT.lookup p.Current
                match node with
                |   MultiPath d -> 
                    d.States 
                    |> List.fold(fun (npt,st) stid -> 
                        p
                        |>  DuplicateParam.SetCurrent stid.StatePointer
                        |>  DuplicateParam.SetStT st
                        |>  traverse
                        |>  fun rt -> (rt.Link :: npt, rt.SourceToTarget)
                        ) ([], p.SourceToTarget)
                    |>  fun (llst, st) ->
                        MT.createAndSimplifyMultiPath llst
                        |>  DuplicateReturn.Create st

                |   EmptyPath d  when d.NextState.Id = PointerToStateFinal.Id -> 
                    MT.duplicateAndLinkToNext concatPtr p.Current
                    |>  DuplicateReturn.Create p.SourceToTarget
                    |>  DuplicateReturn.LinkTargetToSource p.Current
                |   EmptyPath       d -> passthrough d
                |   StartLinePath   d -> passthrough d
                |   RepeatIterOrExit d -> passthrough d
                |   RepeatInit d -> passthrough d
                |   RepeatStart d -> 
                    let rt = DuplicateReturn.Create p.SourceToTarget PointerToStateFinal
                    let dp = dup p rt
                    let nx = 
                        p
                        |>  DuplicateParam.SetCurrent d.NextState
                        |>  DuplicateParam.AddPassedNode d.Id
                        |>  DuplicateParam.AddPreMapped p.Current dp.Link
                        |>  traverse
                    MT.setNextState nx.Link dp.Link
                    |>  DuplicateReturn.Create nx.SourceToTarget

                |   GroupStart d -> passthrough d
                |   GroupEnd   d -> passthrough d
                |   SinglePath d ->
                    if d.NextState.Id = 0u then
                        MT.setNextState concatPtr p.Current
                        |>  DuplicateReturn.Create p.SourceToTarget
                        |>  DuplicateReturn.LinkTargetToSource p.Current
                    else
                        passthrough d
                |   NoMatch _ -> DuplicateReturn.Create p.SourceToTarget p.Current
                |   ReturnSub _ -> DuplicateReturn.Create p.SourceToTarget p.Current |> dup p
                |   GoSub d ->
                    let nwNext = 
                        p
                        |>  DuplicateParam.SetCurrent d.NextState
                        |>  DuplicateParam.AddPassedNode d.Id
                        |>  traverse
                    let gsn = MT.createGoSub nwNext.Link PointerToStateFinal d.GosubId
                    let nwRet  = 
                        p
                        |>  DuplicateParam.SetCurrent d.ReturnState
                        |>  DuplicateParam.AddPassedNode d.Id
                        |>  DuplicateParam.AddPreMapped d.StatePointer gsn.StatePointer
                        |>  DuplicateParam.SetStT nwNext.SourceToTarget
                        |>  traverse
                    MT.setNextState nwRet.Link gsn
                    |>  DuplicateReturn.Create nwRet.SourceToTarget
                    |>  DuplicateReturn.LinkTargetToSource gsn

        DuplicateParam.Create entryStartId passedNodes preMappedNodes Map.empty
        |>  traverse
        |>  fun r -> r.Link


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
                let primary = fst target.Head |> MT.duplicate
                let (filterIds, nextIds) = target |> List.unzip
                let silNew = 
                    let siln = refactorCommonPlains (nextIds |> List.filter(fun e -> e.Id <> PointerToStateFinal.Id) |> List.map(MT.getSinglePathPointers) |> List.collect id)
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

                            let bundle = MT.createAndSimplifyMultiPathSp (silnSorted)
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
            match sp.State with
            |   OneInSetMatch ois -> 
                let newLC = ois.ListCheck |> List.filter(fun t -> not(tks.Contains t))
                if newLC.Length > 0 then
                    let newQC = qcOneInSet newLC
                    (o, (MT.updateNode <| SinglePath({ sp with State = (OneInSetMatch({ QuickCheck = newQC; ListCheck = newLC}))}))) :: s
                else 
                    s
            |  _ -> failwith "Expected OneInSetMatch"
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

                let bundle = 
                    (allNextIds |> List.collect id |> List.map(fun e -> e.StatePointer))
                    |>  MT.simplifyMultiPathStates
                    |>  List.map MT.cleanupEmptyPaths
                    |>  List.map MT.getSinglePathPointers
                    |>  List.collect id
                    |>  MT.createAndSimplifyMultiPathSp 
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
                let primary = elst |> List.head |> fun i -> i.IdSp |> MT.duplicate
                let toRemove, allNextIds =
                    clst
                    |>  List.map(fun e -> e.IdSp, e.Next)
                    |>  List.append (elst |> List.map(fun e -> e.IdSp, e.Next))
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

                let bundle = 
                    allNextIds
                    |>  MT.simplifyMultiPathStates
                    |>  List.map MT.cleanupEmptyPaths
                    |>  List.map MT.getSinglePathPointers
                    |>  List.collect id
                    |>  MT.createAndSimplifyMultiPathSp 

                MT.setNextState bundle.StatePointer primary |> ignore

                let silNew = 
                    sil 
                    |>  List.filter(fun i -> toRemove |> List.exists(fun x -> x.Id = i.Id) |> not)
                    |>  List.append ( primary.SinglePathPointerValue :: (allRefactoredOneInSet |> List.map snd |> List.map(fun i -> i.SinglePathPointerValue)))
                
                refactorPlainsWithCharSets silNew tailNew
        refactorPlainsWithCharSets sil stnl

let refactorMultiPathStates (lst : StatePointer list) =
    lst
    |>  MT.simplifyMultiPathStates
    |>  List.map MT.cleanupEmptyPaths
    |>  List.map MT.getSinglePathPointers
    |>  List.collect id
    |>  refactorConflictingCharacterSets
    |>  refacorConflictingPlainWithCharacterSets
    |>  refactorCommonPlains

    

let refactorMultiPathStatesSp (lst : SinglePathPointer list) =
    lst
    |>  List.map(fun e -> e.StatePointer)
    |>  refactorMultiPathStates



let appendStateIdToAllFinalPathNodes (entryStartId:StatePointer) (concatPtr:StatePointer) =
    let passedNodes = Set.empty<StateId>
    
    let inline setNextState obj nx = MT.setNextState nx obj

    let rec traverse (current:StatePointer) (passedNodes:Set<StateId>) (concatPtr:StatePointer) =
        if  current.Id = 0u || current.Id = concatPtr.Id || passedNodes.Contains (current.Id) then current 
        else
            let node = MT.lookup current
            match node with
            |   MultiPath  d -> 
                d.States 
                |>  List.map(fun stid -> traverse stid.StatePointer (passedNodes(*.Add d.Id*)) concatPtr)
                |>  refactorMultiPathStates
                |>  MT.createAndSimplifyMultiPathSp
            |   EmptyPath  d  when d.NextState.Id = PointerToStateFinal.Id -> 
                MT.setNextState concatPtr node 
            |   EmptyPath     d -> 
                traverse d.NextState (passedNodes(*.Add (d.Id)*)) concatPtr
                |>  setNextState current
                |>  MT.cleanupEmptyPaths
            |   RepeatIterOrExit    d -> 
                traverse d.NextState (passedNodes(*.Add (d.Id)*)) concatPtr
                |>  MT.cleanupEmptyPaths
                |>  setNextState current
            |   RepeatInit  d -> 
                traverse d.NextState.StatePointer (passedNodes(*.Add (d.Id)*)) concatPtr
                |>  setNextState current
            |   RepeatStart d -> 
                traverse d.NextState (passedNodes.Add (d.Id)) concatPtr
                |>  setNextState current
            |   GroupStart  d -> 
                traverse d.NextState (passedNodes(*.Add (d.Id)*)) concatPtr
                |>  setNextState current
            |   GroupEnd    d -> 
                traverse d.NextState (passedNodes(*.Add (d.Id)*)) concatPtr
                |>  setNextState current
            |   SinglePath  d ->
                if d.NextState.Id = PointerToStateFinal.Id then
                    MT.setNextState concatPtr current
                else
                    traverse d.NextState (passedNodes(*.Add (d.Id)*)) concatPtr
                    |>  setNextState current
            |   StartLinePath d -> 
                if d.NextState.Id = 0u then
                    MT.setNextState concatPtr current
                else
                    traverse d.NextState (passedNodes(*.Add (d.Id)*)) concatPtr
                    |>  setNextState current
            |   GoSub d ->
                if d.NextState.Id = 0u then
                    MT.setNextState concatPtr current
                else
                    traverse d.NextState   (passedNodes.Add (d.Id)) concatPtr |> ignore
                    traverse d.ReturnState (passedNodes.Add (d.Id)) concatPtr 
                    |>  setNextState current
            |   ReturnSub d -> current
            |   NoMatch _ -> current
    traverse entryStartId passedNodes concatPtr



let convertRepeaterToExplicitGraph (start:StatePointer) =

    let convertRepeaterToExplicitTree (ri:RepeatStateRef) (rs:RepeatStateRef) (rioe:RepeatIterateOrExit) (rt:RepeatState) =
        if rt.Max > 0 then
            let gosubId = MT.CreateGosubId()
            let rtNode = MT.createRetSub gosubId

            let mandatoryPath = Duplication.duplicateStructureAndLinkToNext [rioe.StatePointer; rs.StatePointer] [(rioe.StatePointer, rtNode);(rs.StatePointer, rtNode)] rioe.IterateState rtNode
            let optionalPath =
                (MT.getSinglePathPointers mandatoryPath) @ (MT.getSinglePathPointers rioe.NextState)
                |>  refactorMultiPathStatesSp
                |>  MT.createAndSimplifyMultiPathSp

            [rt.Max .. -1 .. 1]
            |>  List.fold(fun nxt cur ->
                if cur > rt.Min then
                    MT.createGoSub optionalPath nxt gosubId
                else
                    MT.createGoSub mandatoryPath nxt gosubId
            ) rioe.NextState
        else
            let gosubId = MT.CreateGosubId()
            let rtNode = MT.createRetSub gosubId

            let mandatoryPath = Duplication.duplicateStructureAndLinkToNext [rioe.StatePointer; rs.StatePointer] [(rioe.StatePointer, rtNode);(rs.StatePointer, rtNode)] rioe.IterateState rtNode
            let optionalPath =
                (MT.getSinglePathPointers mandatoryPath) @ (MT.getSinglePathPointers rioe.NextState)
                |>  refactorMultiPathStatesSp
                |>  MT.createAndSimplifyMultiPathSp

            let infiniteLoop =
                MT.createGoSub optionalPath PointerToStateFinal gosubId

            let infiniteLoop =
                MT.setNextState (infiniteLoop.StatePointer) infiniteLoop

            [rt.Min .. -1 .. 1]
            |>  List.fold(fun nxt _ ->
                MT.createGoSub mandatoryPath nxt gosubId
            ) infiniteLoop
        

    match MT.lookup start with
    |   RepeatInit repInit -> 
        match MT.lookup repInit.NextState with
        |   RepeatStart ep -> 
            match MT.lookup ep.NextState with
            |   RepeatIterOrExit re -> 
                let nxt = convertRepeaterToExplicitTree repInit ep re (MT.getRepeatState repInit.RepeatId)
                nxt
            | _ -> start
        |   _ -> start
    |   _ -> start



let removeUnused (nfa:NFAMachine) =
    let stMap = nfa.States |> List.map(fun e -> e.Id, e) |> Map.ofList

    let rec traverse (current:StatePointer) (passedNodes:Set<StateId>) (used:StateId list) =
        if  current.Id = 0u || passedNodes.Contains (current.Id) then current.Id::used
        else
            let node = stMap.[current.Id]
            match node with
            |   SinglePath sp -> traverse (sp.NextState) (passedNodes) (sp.Id::used)
            |   StartLinePath ep -> traverse (ep.NextState) (passedNodes) (ep.Id::used)
            |   MultiPath  mp -> 
                let newPassed = passedNodes
                mp.States 
                |> List.fold(fun u stid -> traverse stid.StatePointer newPassed u) (mp.Id::used)
            |   EmptyPath  ep -> traverse (ep.NextState) (passedNodes) (ep.Id::used)
            |   RepeatInit rs -> traverse (rs.NextState.StatePointer) (passedNodes) (rs.Id::used)
            |   RepeatIterOrExit   re  -> 
                let ri = traverse (re.IterateState) (passedNodes) (re.Id::used) 
                traverse (re.NextState) (passedNodes) (re.Id::ri)
            |   RepeatStart ep -> traverse (ep.NextState) (passedNodes.Add (ep.Id)) (ep.Id::used)
            |   GroupStart  ep  -> traverse (ep.NextState) (passedNodes) (ep.Id::used)
            |   GroupEnd ep  -> traverse (ep.NextState) (passedNodes) (ep.Id::used)
            |   NoMatch d -> current.Id::used
            |   GoSub    gs  -> 
                let ri = traverse (gs.NextState) (passedNodes) (gs.Id::used)
                traverse (gs.ReturnState) (passedNodes.Add gs.Id) (gs.Id::ri)
            |   ReturnSub _ -> current.Id::used
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
            let repPath = o |> convert |>  convertRepeaterToExplicitGraph
            let repState = MT.createRepeatState min max
            let repExit = MT.createRepeatIterOrExit repPath repState.RepeatId linkState
            let repeatLoopStart = MT.createRepeatStart repState.RepeatId repExit
            
            let nx = appendStateIdToAllFinalPathNodes repPath repeatLoopStart

            match MT.lookup repExit with
            |   RepeatIterOrExit ioe ->
                RepeatIterOrExit {ioe with IterateState = nx}
                |>  MT.updateNode
                |>  ignore

                let repStart = MT.createRepeatInit repState.RepeatId repeatLoopStart
                repStart
            |   _ -> failwith "Expecting RepeatIterOrExit"

        match rgx with
        |   Plain  pl ->
            if pl.``fixed``.Length > 1 then
                pl.OptimizeOnce()
                processConversion (Concat (pl.optimized))
            else
                convert rgx
                //failwith "Uncontained plain - not implemented yet"
        |   OneInSet _ -> convert rgx
        |   Concat l -> 
            let linkState = MT.createEmptyPath PointerToStateFinal
            let converts =
                l
                |>  List.map(convert)

            converts
            |>  List.fold(fun (concatPtr:StatePointer) (entryStart:StatePointer) ->
                    appendStateIdToAllFinalPathNodes entryStart concatPtr
                    |>  convertRepeaterToExplicitGraph
                    ) linkState
        |   Or     l -> 
            l
            |>  List.map(convert >> MT.cleanupEmptyPaths >> convertRepeaterToExplicitGraph)
            |>  List.map(fun sp -> sp)
            |>  List.fold(fun sil sp ->
                    let mpl = 
                        MT.lookup sp
                        |>  function
                            |   MultiPath  mp -> mp.States
                            |   _             -> [sp.SinglePathPointerValue]
                    sil @ mpl
            ) []
            |>  refactorMultiPathStatesSp
            //|>  refactorConflictingCharacterSets
            //|>  refacorConflictingPlainWithCharacterSets
            //|>  refactorCommonPlains
            |>  MT.createAndSimplifyMultiPathSp
        |   Optional    r -> createRepeat r 0 1
        |   ZeroOrMore  r -> createRepeat r 0 0
        |   ZeroOrMoreNonGreedy  r -> createRepeat r 0 0
        |   OneOrMore   r -> createRepeat r 1 0
        |   OneOrMoreNonGreedy r -> createRepeat r 1 0
        |   Group       r ->
            let ep = MT.createEmptyPath PointerToStateFinal
            let gp = convert r |> convertRepeaterToExplicitGraph
            let gs = MT.createGroup()
            let ge = MT.createGroupEnd gs ep
            appendStateIdToAllFinalPathNodes gp ge
            |>  MT.createGroupStart gs
        |   IterRange (irx,mxo,mno) ->
            match mno with
            |   Some minVal -> createRepeat irx minVal mxo
            |   None        -> createRepeat irx mxo mxo  
        |   _ -> failwith "Not Implemented Yet"
    processConversion rgx
    |>  convertRepeaterToExplicitGraph
    |>  MT.ToNFAMachine
    |>  removeUnused


type ParseResult = {
    IsMatch     : bool
    FullMatch   : char list
    Groups      : (char list) list
}



type RunningState = {
    RunningLoops    : Map<RepeatId, RepeatState>
    GroupParse      : GroupParse
    GosubMapping    : Map<GosubId, StatePointer>
    LoopToPos       : Map<RepeatId, int>
}
with
    static member Create() = {RunningLoops = Map.empty<_,_>; GroupParse = GroupParse.Create(); GosubMapping = Map.empty<_,_>; LoopToPos = Map.empty<_,_>}
    member this.AddChar ch = {this with GroupParse = this.GroupParse.AddChar ch}
    member this.SetLoop ri rs =
        let lp =
            this.RunningLoops
            |>  Map.remove ri
            |>  Map.add ri rs
        { this with RunningLoops = lp }

    member this.RmLoop ri =
        let lp =
            this.RunningLoops
            |>  Map.remove ri
        { this with RunningLoops = lp }

    member this.SetGosub gi sp=
        let gs =
            this.GosubMapping
            |>  Map.remove gi
            |>  Map.add gi sp
        { this with GosubMapping = gs }

    member this.StartGroup gi = { this with GroupParse = this.GroupParse.Start gi}

    member this.StopGroup gi = { this with GroupParse = this.GroupParse.Stop gi}

    member this.MapLoopToPos ri ps = 
        let lp =
            this.LoopToPos
            |>  Map.remove ri
            |>  Map.add ri ps
        { this with LoopToPos = lp }

    member this.RmMapLoop ri =
        let lp =
            this.LoopToPos
            |>  Map.remove ri
        { this with LoopToPos = lp }


let parseIt (nfa:NFAMachine) (stream:RollingStream<TokenData>) =
    let stMap = nfa.States |> List.fold(fun (m:Map<_,_>) i -> m.Add(i.Id, i)) Map.empty<StateId, StateNode>
    let stRepeat = nfa.Repeats |> List.fold(fun (m:Map<_,_>) i -> m.Add(i.RepeatId, i)) Map.empty<RepeatId, RepeatState>

    let NoMatch = { IsMatch = false ; FullMatch = []; Groups = [] }

    let startOfLine =
        if stream.Position = 0 then true
        else
            stream.Position <-  (stream.Position-1)
            let ch = stream.Get()
            ch.Token = Token.NewLine

    let rec processStr currentChar cs acc rollback (runningState: RunningState) startOfLine =
        let processCurrentChar = processStr currentChar
        let processNextChar cs acc rollback (runningState: RunningState) = 
            let chk = (stream.Get())
            processStr chk cs acc rollback runningState 

        if cs = PointerToStateFinal then
            stream.Position <- (stream.Position-1)
            let gs = 
                runningState.GroupParse.GroupMatches
                |>  Map.toList
                |>  List.sortBy(fun (i, _) -> i)
                |>  List.map snd
                |>  List.map(fun g -> g |> List.rev)
                |>  List.rev
            { IsMatch = true; FullMatch = acc |> List.rev; Groups = gs }
        else
            let st = stMap.[cs.Id]
            match st with
            |   SinglePath p ->
                let nxt = p.NextState
                if (p.State.Match currentChar) then
                    let ch = currentChar.Source.[0]
                    processNextChar nxt (ch :: acc) rollback (runningState.AddChar ch) (currentChar.Token = Token.NewLine)
                else 
                    NoMatch
            |   StartLinePath st ->
                if startOfLine then processCurrentChar (st.NextState) acc (rollback+1) runningState startOfLine
                else NoMatch
            |   MultiPath p ->
                let pos = stream.Position
                let rec parseMultiPath (sptrLst:SinglePathPointer list) =
                    match sptrLst with 
                    |   []      -> NoMatch 
                    |   h::tail ->
                        processCurrentChar h.StatePointer acc (rollback+1) runningState startOfLine
                        |>  fun res -> 
                            if not(res.IsMatch) then
                                stream.Position <- pos
                                parseMultiPath tail
                            else res

                parseMultiPath p.States
            |   EmptyPath p -> processCurrentChar p.NextState acc rollback runningState startOfLine
            |   RepeatStart p ->
                let strPos = stream.Position

                if runningState.LoopToPos.ContainsKey p.RepeatId then
                    if strPos > runningState.LoopToPos.[p.RepeatId] then
                        processCurrentChar p.NextState acc rollback (runningState.MapLoopToPos p.RepeatId strPos ) startOfLine
                    else
                        NoMatch
                else
                    processCurrentChar p.NextState acc rollback (runningState.MapLoopToPos p.RepeatId strPos) startOfLine
            |   RepeatInit r ->
                let rlnew = runningState.SetLoop r.RepeatId stRepeat.[r.RepeatId]
                processCurrentChar r.NextState.StatePointer acc rollback rlnew startOfLine
            |   RepeatIterOrExit r ->
                let rs = runningState.RunningLoops.[r.RepeatId]
                if rs.MustExit() then
                    processCurrentChar r.NextState acc rollback runningState startOfLine
                else
                    let pos = stream.Position
                    let rlNew = runningState.SetLoop r.RepeatId (rs.Iterate())
                    let tryIterate = processCurrentChar r.IterateState acc rollback rlNew startOfLine
                    if tryIterate.IsMatch then
                        tryIterate
                    else
                        if stream.Position = pos && rs.CanExit() && r.IterateState<>r.NextState then
                            processCurrentChar r.NextState acc rollback (rlNew.RmLoop rs.RepeatId) startOfLine // end loop by removing it from running loops
                        else
                            NoMatch
            |   GroupStart gp -> processCurrentChar gp.NextState acc rollback (runningState.StartGroup gp.GroupId) startOfLine
            |   GroupEnd   gp -> processCurrentChar gp.NextState acc rollback (runningState.StopGroup gp.GroupId) startOfLine
            |   NoMatch _ -> NoMatch
            |   GoSub gs -> processCurrentChar gs.NextState acc rollback (runningState.SetGosub gs.GosubId gs.ReturnState) startOfLine
            |   ReturnSub rs -> 
                let nx = runningState.GosubMapping.[rs.GosubId]
                processCurrentChar nx acc rollback runningState startOfLine

    processStr (stream.Get()) nfa.Start [] 0 (RunningState.Create()) startOfLine



type LevelType = 
    |   Concat = 0
    |   Multi   = 1
    |   RepeatIter = 4
    |   RepeatExit = 2
    |   LoopStart = 3
    |   Empty     = 5
    |   Group = 6
    |   Gosub = 7


let PrintIt (nfa:NFAMachine) =
    let stMap = nfa.States |> List.map(fun e -> e.Id,e) |> Map.ofList
    let rsMap = nfa.Repeats |> List.map(fun e -> e.RepeatId,e) |> Map.ofList
    let passedNodes = Set.empty<StateId>
    let passedGosub = Set.empty<GosubId>

    let rec printLine (hist : LevelType list) (current: StatePointer) (passedNodes:Set<StateId>) (passedGosub:Set<GosubId>) =
        let printPrefix hist =
            hist
            |>  List.rev
            |>  List.iter(fun i ->
                match i with
                |   LevelType.Concat    -> printf "          "
                |   LevelType.Group     -> printf "           "
                |   LevelType.Empty     -> printf "      "
                |   LevelType.Multi     -> printf "|      "
                |   LevelType.RepeatExit-> printf " |X     "
                |   LevelType.RepeatIter-> printf " |I     "
                |   LevelType.LoopStart -> printf "                "
                |   LevelType.Gosub     -> printf "|          "
            )

        printPrefix hist
        let rec printLineRest (hist : LevelType list) (current: StatePointer) (passedNodes:Set<StateId>) (passedGosub:Set<GosubId>) =
            if current.Id = 0u then
                printf "-*\n"
            else
                if not(passedNodes.Contains current.Id) then
                    match stMap.[current.Id] with
                    |   EmptyPath  ep   ->  
                        printf "~(%3d)" ep.Id
                        printLineRest (LevelType.Empty :: hist) ep.NextState (passedNodes.Add ep.Id) passedGosub
                    |   StartLinePath  ep   ->  
                        printf "^(%3d)" ep.Id
                        printLineRest (LevelType.Empty :: hist) ep.NextState (passedNodes.Add ep.Id) passedGosub
                    |   SinglePath sp   -> 
                        match sp.State with
                        |   ExactMatch c    -> printf "-(%3d:\"%s\")" sp.Id (Regex.Escape(c.Char.ToString()))
                        |   OneInSetMatch o -> printf "-(%3d:[@])" sp.Id
                        printLineRest (LevelType.Concat :: hist) sp.NextState (passedNodes.Add sp.Id) passedGosub
                    |   MultiPath mp    ->
                        match mp.States with
                        |   h::t -> 
                            printf "|(%3d)" mp.Id
                            printLineRest (LevelType.Multi :: hist) h.StatePointer (passedNodes.Add mp.Id) passedGosub
                            t |> List.iter(fun e -> printLine (LevelType.Multi :: hist) e.StatePointer (passedNodes.Add mp.Id) passedGosub) 
                        |   [] -> ()
                    |   RepeatInit rs ->
                        let rt = rsMap.[rs.RepeatId]
                        printf "->>(%3d:<%2d,%2d>)" rs.Id rt.Min rt.Max
                        printLineRest (LevelType.LoopStart :: hist) (rs.NextState.StatePointer) (passedNodes.Add rs.Id) passedGosub
                    |   RepeatStart rs ->
                        printf "L(%3d)" rs.Id
                        printLineRest (LevelType.Empty :: hist) rs.NextState (passedNodes.Add rs.Id) passedGosub
                    |   RepeatIterOrExit ri ->
                        printf "-|I(%3d)" ri.Id
                        printLineRest (LevelType.RepeatIter :: hist) ri.IterateState (passedNodes.Add ri.Id) passedGosub

                        if ri.IterateState.Id <> ri.NextState.Id then
                            printPrefix hist
                            printf "-|X(%3d)" ri.Id
                            printLineRest (LevelType.RepeatExit :: hist) ri.NextState (passedNodes.Add ri.Id) passedGosub
                        else
                            printPrefix hist
                            printf "|X(%3d) :::^^^\n" ri.NextState.Id
                    |   GroupStart gp ->
                        printf "-%2d, (%3d){" gp.GroupId.Id gp.Id 
                        printLineRest (LevelType.Group :: hist) gp.NextState (passedNodes.Add gp.Id) passedGosub
                    |   GroupEnd gp ->
                        printf "}(%3d)     " gp.Id 
                        printLineRest (LevelType.Group :: hist) gp.NextState (passedNodes.Add gp.Id) passedGosub
                    |   NoMatch d -> printf "-NoMatch(%3d)\n" d
                    |   ReturnSub d -> printfn "-Ret(%3d,%d)" d.Id d.GosubId.Id
                    |   GoSub     d -> 
                        if passedNodes.Contains d.NextState.Id then
                            printf "|GS(%3d, %d) (repeat)\n" d.Id d.GosubId.Id
                            printPrefix (hist)
                            printf "|GS-Ret    "
                            printLineRest (LevelType.Gosub :: hist) d.ReturnState (passedNodes.Add d.Id) passedGosub
                        else
                            printf "|GS(%3d, %d)" d.Id d.GosubId.Id
                            printLineRest (LevelType.Gosub :: hist) d.NextState (passedNodes.Add d.Id) (passedGosub.Add d.GosubId)
                            printPrefix (hist)
                            printf "|GS-Ret    "
                            printLineRest (LevelType.Gosub :: hist) d.ReturnState (passedNodes.Add d.Id) (passedGosub.Add d.GosubId)
                            //printLine (LevelType.Gosub :: hist) d.ReturnState.StatePointer (passedNodes.Add d.Id) (passedGosub.Add d.GosubId)
                else
                    printf "-Loop(%3d)\n" current.Id

        printLineRest hist current passedNodes passedGosub
    printLine [] nfa.Start passedNodes passedGosub


let clts (cl:char list) = System.String.Concat(cl)

module ParseResult =
    let IsMatch pr = pr.IsMatch
    let FullMatch pr = pr.FullMatch


