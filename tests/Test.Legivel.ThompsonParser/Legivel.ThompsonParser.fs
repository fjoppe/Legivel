module Legivel.ThompsonParser

open Legivel.Utilities.RegexDSL
open Legivel.Tokenizer
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Text
open NLog

let logger = LogManager.GetLogger("*")


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
            | MultiPathPointer  _ -> failwith "MultiPathPointer has no SinglePathPointer"

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
        Yield      : bool
        GroupIds   : GroupId list
        NextState  : StatePointer 
    }
    with
        static member Create i mt nx fmg = { Id = i; State = mt; NextState = nx; GroupIds = fmg; Yield = false }
        member this.LinkTo i = { this with NextState = i}
        member this.StatePointer = SinglePathPointer(SinglePathPointer.Create this.Id)
        member this.SinglePathPointer = SinglePathPointer.Create this.Id
        member this.Duplicate i = {this with Id = i}
        member this.IsEOF with get() =
            match this.State with
            |   OneInSetMatch ois -> 
                ois.ListCheck.Length = 1 && ois.ListCheck.Head = Token.EOF
            |   _ -> false


type UnYieldable = {
    NodePointer  : SinglePathPointer
    GroupUnYield : GroupId list option 
}
with    
    static member Create np yl = { NodePointer = np; GroupUnYield = yl }


type MultiPath = {
        Id         : StateId
        States     : UnYieldable list
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


type StartOfLinePath = {
    Id          : StateId
    Iftrue      : StatePointer
    IfFalse     : StatePointer 
}
with
    static member Create i t f = { Id = i; Iftrue = t; IfFalse = f}
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
    |   StartLinePath of StartOfLinePath
    |   MultiPath  of MultiPath
    |   EmptyPath  of EmptyPath
    |   RepeatInit of RepeatStateRef
    |   RepeatStart of RepeatStateRef
    |   RepeatIterOrExit of RepeatIterateOrExit
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
                |   GoSub       d -> d.Id
                |   ReturnSub   d -> d.Id
                |   NoMatch     d -> d

        member this.IsEmptyPathToFinal 
            with get() =
                match this with
                |   EmptyPath d -> d.NextState.Id = PointerToStateFinal.Id
                |   _ -> false

        member this.IsStartOfLine
            with get() =
                match this with
                |   StartLinePath _ -> true
                |   _ -> false

        member this.IsNoMatchValue 
            with get() =
                match this with
                |   NoMatch _ -> true
                |   _ -> false

        member this.NextState 
            with get() =
                match this with
                |   SinglePath d -> d.NextState.Id
                |   EmptyPath  d -> d.NextState.Id
                |   RepeatInit d -> d.NextState.Id
                |   RepeatStart d -> d.NextState.Id
                |   RepeatIterOrExit  d -> d.NextState.Id
                |   GoSub       d -> d.ReturnState.Id
                |   StartLinePath _ -> failwith "StartLinePath has no single nextstate"
                |   ReturnSub  _ -> failwith "ReturnSub has no single nextstate"
                |   MultiPath  _ -> failwith "Multipath has no single nextstate"
                |   NoMatch _ -> failwith "NoMatch has no single nextstate"

        member this.NextStatePtr 
            with get() =
                match this with
                |   SinglePath      d -> d.NextState
                |   EmptyPath       d -> d.NextState
                |   RepeatInit      d -> d.NextState.StatePointer
                |   RepeatStart     d -> d.NextState
                |   RepeatIterOrExit d -> d.NextState
                |   GoSub           d -> d.ReturnState
                |   StartLinePath _ -> failwith "StartLinePath has no single nextstate"
                |   ReturnSub  _ -> failwith "ReturnSub has no single nextstate"
                |   MultiPath  _ -> failwith "Multipath has no single nextstate"
                |   NoMatch    _ -> failwith "NoMatch has no single nextstate"


        member this.SetNextState i =
                match this with
                |   SinglePath      d -> SinglePath { d with NextState = i }
                |   EmptyPath       d -> EmptyPath { d with NextState = i }
                |   RepeatStart     d -> RepeatStart { d with NextState = i }
                |   RepeatIterOrExit d -> RepeatIterOrExit  { d with NextState = i }
                |   RepeatInit      d -> RepeatInit { d with NextState = i }
                |   GoSub           d -> GoSub { d with ReturnState = i }
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
                |   GoSub       d -> d.SinglePathPointer
                |   ReturnSub   d -> d.SinglePathPointer
                |   MultiPath  d -> failwith "Multipath has no SinglePathPointer"
                |   NoMatch d -> SinglePathPointer.Create d


        member this.AsString =
            match this with
            |   SinglePath d -> 
                match d.State with
                |   ExactMatch    em -> sprintf "Id: %d, SinglePath Exact: '%c', Next: %d, Groups: %A" d.Id em.Char d.NextState.Id d.GroupIds
                |   OneInSetMatch _  -> sprintf "Id: %d, SinglePath OiS, Next: %d, Groups: %A" d.Id d.NextState.Id d.GroupIds
            |   StartLinePath d -> sprintf "Id: %d, StartLinePath IfTrue: %d, IfFalse: %d" d.Id d.Iftrue.Id d.IfFalse.Id
            |   MultiPath  d -> sprintf "Id: %d, MultiPath, States: %O" d.Id (d.States |> List.map(fun i -> i.NodePointer.Id))
            |   EmptyPath  d -> sprintf "Id: %d, EmptyPath Next: %d" d.Id d.NextState.Id
            |   RepeatInit d -> sprintf "Id: %d, RepeatInit Next: %d" d.Id d.NextState.Id
            |   RepeatStart d -> sprintf "Id: %d, RepeatStart Next: %d" d.Id d.NextState.Id
            |   RepeatIterOrExit  d -> sprintf "Id: %d, RepeatIterOrExit, I: %d, E: %d" d.Id d.IterateState.Id d.NextState.Id
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


type LevelType = 
    |   Concat = 0
    |   Multi   = 1
    |   RepeatIter = 4
    |   RepeatExit = 2
    |   LoopStart = 3
    |   Empty     = 5
    |   Group = 6
    |   Gosub = 7
    |   StartOfLine = 8



let SPrintIt (level:int) (nfa:NFAMachine) =

    let sb = StringBuilder()

    let stMap = nfa.States |> List.map(fun e -> e.Id,e) |> Map.ofList
    let rsMap = nfa.Repeats |> List.map(fun e -> e.RepeatId,e) |> Map.ofList
    let passedNodes = HashSet<StateId>()
    let passedGosub = Set.empty<GosubId>

    let rec printLine (hist : LevelType list) (current: StatePointer) (passedGosub:Set<GosubId>) =
        let printPrefix (hist : LevelType list) =
            hist
            |>  List.rev
            |>  List.iter(fun i ->
                match i with
                |   LevelType.Concat      -> "            " |> sb.Append |> ignore
                |   LevelType.Group       -> "            " |> sb.Append |> ignore
                |   LevelType.Empty       -> "       " |> sb.Append |> ignore
                |   LevelType.StartOfLine -> "|          " |> sb.Append |> ignore
                |   LevelType.Multi       -> "|      " |> sb.Append |> ignore
                |   LevelType.RepeatExit  -> " |X      " |> sb.Append |> ignore
                |   LevelType.RepeatIter  -> " |I      " |> sb.Append |> ignore
                |   LevelType.LoopStart   -> "                 " |> sb.Append |> ignore
                |   LevelType.Gosub       -> "|           " |> sb.Append |> ignore
            )

        printPrefix hist
        let rec printLineRest (hist : LevelType list) (current: StatePointer) (passedGosub:Set<GosubId>) =
            if current.Id = 0u then
                sb.Append "-*\n" |> ignore
            else
                if not(passedNodes.Contains current.Id) then
                    passedNodes.Add current.Id |> ignore

                    match stMap.[current.Id] with
                    |   EmptyPath  ep   ->  
                        sprintf "~(%4d)" ep.Id |> sb.Append |> ignore
                        printLineRest (LevelType.Empty :: hist) ep.NextState passedGosub
                    |   StartLinePath  ep   ->  
                        sprintf "|^(%4d, T)" ep.Id |> sb.Append |> ignore
                        printLineRest (LevelType.StartOfLine :: hist) ep.Iftrue passedGosub
                        printPrefix (hist)
                        sprintf "|^(%4d, F)" ep.Id |> sb.Append |> ignore
                        printLineRest (LevelType.StartOfLine :: hist) ep.IfFalse passedGosub
                    |   SinglePath sp   -> 
                        match sp.State with
                        |   ExactMatch c    -> sprintf "-(%4d:%4s)" sp.Id (sprintf "\"%s\"" (Regex.Escape(c.Char.ToString()))) |> sb.Append |> ignore
                        |   OneInSetMatch o -> sprintf "-(%4d: [@])" sp.Id |> sb.Append |> ignore
                        printLineRest (LevelType.Concat :: hist) sp.NextState passedGosub
                    |   MultiPath mp    ->
                        match mp.States with
                        |   h::t -> 
                            sprintf "|(%4d)" mp.Id |> sb.Append |> ignore
                            printLineRest (LevelType.Multi :: hist) h.NodePointer.StatePointer passedGosub
                            t |> List.iter(fun e -> printLine (LevelType.Multi :: hist) e.NodePointer.StatePointer passedGosub) 
                        |   [] -> ()
                    |   RepeatInit rs ->
                        let rt = rsMap.[rs.RepeatId]
                        sprintf "->>(%4d:<%2d,%2d>)" rs.Id rt.Min rt.Max |> sb.Append |> ignore
                        printLineRest (LevelType.LoopStart :: hist) (rs.NextState.StatePointer) passedGosub
                    |   RepeatStart rs ->
                        sprintf "L(%4d)" rs.Id |> sb.Append |> ignore
                        printLineRest (LevelType.Empty :: hist) rs.NextState passedGosub
                    |   RepeatIterOrExit ri ->
                        sprintf "-|I(%4d)" ri.Id |> sb.Append |> ignore
                        printLineRest (LevelType.RepeatIter :: hist) ri.IterateState passedGosub

                        if ri.IterateState.Id <> ri.NextState.Id then
                            printPrefix hist
                            sprintf "-|X(%4d)" ri.Id |> sb.Append |> ignore
                            printLineRest (LevelType.RepeatExit :: hist) ri.NextState passedGosub
                        else
                            printPrefix hist
                            sprintf "|X(%4d) :::^^^\n" ri.NextState.Id |> sb.Append |> ignore
                    |   NoMatch d -> sprintf "-NoMatch(%4d)\n" d |> sb.Append |> ignore
                    |   ReturnSub d -> sprintf "-Ret(%4d,%d)" d.Id d.GosubId.Id |> sb.AppendLine |> ignore
                    |   GoSub     d -> 
                        if passedNodes.Contains d.NextState.Id then
                            sprintf "|GS(%4d, %d) -Goto(%4d) - see above\n" d.Id d.GosubId.Id d.NextState.Id |> sb.Append |> ignore
                            printPrefix (hist)
                            sprintf "|GS-Ret     " |> sb.Append |> ignore
                            printLineRest (LevelType.Gosub :: hist) d.ReturnState passedGosub
                        else
                            sprintf "|GS(%4d, %d)" d.Id d.GosubId.Id |> sb.Append |> ignore
                            printLineRest (LevelType.Gosub :: hist) d.NextState (passedGosub.Add d.GosubId)
                            printPrefix (hist)
                            sprintf "|GS-Ret     " |> sb.Append |> ignore
                            printLineRest (LevelType.Gosub :: hist) d.ReturnState (passedGosub.Add d.GosubId)
                else
                    sprintf "-Goto(%4d) - see above\n" current.Id |> sb.Append |> ignore

        if level > 0 && hist.Length > level then sb.Append " .... (more)\n" |> ignore
        else
            printLineRest hist current passedGosub
    printLine [] nfa.Start passedGosub
    sb.ToString()



module MT = //    Match Tree
    [<Literal>]
    let fullmatchGroup = 0u //  groupId '0' will be full match

    let mutable private currentId = 0u
    let mutable private currentRepeatId = 0u
    let mutable private currentGroupId = (fullmatchGroup + 1u) 
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

    let private allNodes = Dictionary<StateId, StateNode>(10000)
    let mutable private allRepeats = new System.Collections.Generic.List<RepeatState>()

    let Init() =
        currentId       <- 0u
        currentRepeatId <- 0u
        currentGroupId  <- (fullmatchGroup + 1u)
        currentGosubId  <- 0u
        allNodes.Clear()
        allRepeats      <- new System.Collections.Generic.List<RepeatState>()

    let addAndReturn (nd:StateNode) =
        allNodes.Add(nd.Id, nd)
        nd.StatePointer

    let updateAndReturn (nd:StateNode) =
        allNodes.Remove(nd.Id) |> ignore
        allNodes.Add(nd.Id, nd)
        nd.StatePointer

    let createRepeatState min max = 
        let st = RepeatState.Create (CreateNewRepeatId()) min max
        allRepeats.Add(st)
        st

    let createGroup() = CreateGroupId()

    let getNode i = allNodes.[i]

    let createSinglePath mt nx = SinglePath.Create (CreateNewId()) mt nx [GroupId MT.fullmatchGroup] |> SinglePath |> addAndReturn
    let createSinglePathForDuplicate mt nx gl = SinglePath.Create (CreateNewId()) mt nx gl |> SinglePath |> addAndReturn
    let createStartLinePath t f = StartOfLinePath.Create (CreateNewId()) t f |> StartLinePath |> addAndReturn
    let createMultiPath mt = MultiPath.Create (CreateNewId()) mt |> MultiPath |> addAndReturn
    let createEmptyPath nx = EmptyPath.Create (CreateNewId()) nx |> EmptyPath |> addAndReturn
    let createRepeatInit ri nx = RepeatStateRef.Create (CreateNewId()) ri nx |> RepeatInit |> addAndReturn
    let createRepeatStart ri nx = RepeatStateRef.Create (CreateNewId()) ri nx |> RepeatStart |> addAndReturn
    let createRepeatIterOrExit it ri nx = RepeatIterateOrExit.Create (CreateNewId()) it ri nx |> RepeatIterOrExit |> addAndReturn
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
    
    let ToNFAMachine st = 
        let nfa = NFAMachine.Create (st, (allNodes.Values |> Seq.toList), (allRepeats |> Seq.toList))
        let stMap = 
            let d = Dictionary<StateId, StateNode>()
            nfa.States |> List.iter(fun e -> d.Add(e.Id, e))
            d

        let passedNodes = HashSet<StateId>()

        let rec traverse (current:StatePointer) addToHash =
            match addToHash with
            |   Some v -> passedNodes.Add(v) |> ignore
            |   None -> ()

            if  current.Id = 0u || passedNodes.Contains (current.Id) then passedNodes
            else
                let node = stMap.[current.Id]
                match node with
                |   SinglePath sp -> traverse (sp.NextState) (Some current.Id)
                |   StartLinePath ep -> 
                    traverse (ep.Iftrue) (Some current.Id) |> ignore
                    traverse (ep.IfFalse) (Some current.Id)
                |   MultiPath  mp -> 
                    passedNodes.Add(current.Id) |> ignore
                    mp.States 
                    |> List.fold(fun _ stid -> traverse stid.NodePointer.StatePointer None) passedNodes
                |   EmptyPath  ep -> traverse (ep.NextState) (Some current.Id)
                |   RepeatInit rs -> traverse (rs.NextState.StatePointer) (Some current.Id)
                |   RepeatIterOrExit   re  -> 
                    traverse (re.IterateState) (Some current.Id) |> ignore
                    traverse (re.NextState) None
                |   RepeatStart ep -> traverse (ep.NextState) (Some current.Id) 
                |   NoMatch _ -> 
                    passedNodes.Add current.Id |> ignore
                    passedNodes
                |   GoSub    gs  -> 
                    traverse (gs.NextState) (Some current.Id) |> ignore
                    traverse (gs.ReturnState) None
                |   ReturnSub _ -> 
                    passedNodes.Add current.Id |> ignore
                    passedNodes

        let usedLst = traverse nfa.Start None
        let nodes = nfa.States |> List.filter(fun n -> usedLst.Contains n.Id)
        {nfa with States = nodes}

    let duplicate (currPtr : StatePointer) =
        match lookup currPtr with
        |   EmptyPath   p -> createEmptyPath p.NextState
        |   SinglePath  p -> createSinglePathForDuplicate p.State p.NextState p.GroupIds
        |   RepeatStart p -> createRepeatStart p.RepeatId p.NextState
        |   RepeatInit  p -> createRepeatInit p.RepeatId p.NextState
        |   RepeatIterOrExit p -> createRepeatIterOrExit p.IterateState p.RepeatId p.NextState
        |   GoSub       p -> createGoSub      p.NextState p.ReturnState p.GosubId
        |   ReturnSub   p -> createRetSub     p.GosubId
        |   MultiPath   p -> createMultiPath  p.States
        |   StartLinePath p -> createStartLinePath p.Iftrue p.IfFalse
        |   NoMatch     _ -> currPtr


    let duplicateAndLinkToNext (next:StatePointer) (currPtr : StatePointer) =
        match lookup currPtr with
        |   EmptyPath   _ -> createEmptyPath next
        |   SinglePath  p -> createSinglePathForDuplicate p.State next p.GroupIds
        |   RepeatStart p -> createRepeatStart p.RepeatId next
        |   RepeatInit  p -> createRepeatInit p.RepeatId next
        |   RepeatIterOrExit p -> createRepeatIterOrExit p.IterateState p.RepeatId next
        |   GoSub       p -> createGoSub      p.NextState next p.GosubId
        |   ReturnSub   p -> createRetSub     p.GosubId
        |   NoMatch     _ -> currPtr
        |   StartLinePath p -> failwith "duplicateAndLinkToNext cannot be applied to StartOfLine"
        |   MultiPath   _ -> failwith "duplicateAndLinkToNext cannot be applied to MultiPath"
        

    let SortStateNodes lst =
        let rec sortOrder = 
            function
            |   StartLinePath  _ -> 100
            |   MultiPath      _ -> 200
            |   RepeatInit     _ -> 400
            |   RepeatIterOrExit _ -> 500
            |   SinglePath     _ -> 600
            |   GoSub          _ -> 900
            |   RepeatStart    _ -> 950
            |   ReturnSub      _ -> 1000
            |   EmptyPath      _ -> 1100
            |   NoMatch        _ -> 1200
        let rec sortOrderCompare c1 c2 =
            (sortOrder c1).CompareTo(sortOrder c2)
            
        lst
        |>  List.sortWith sortOrderCompare


    let cleanupEmptyPaths pt =
        let rec searchStart cp =
            match lookup cp with
            |   EmptyPath p when p.NextState.Id <> PointerToStateFinal.Id -> searchStart p.NextState
            |   _  -> cp

        searchStart pt


    let simplifyMultiPathStates (lst:StatePointer list) =
        lst 
        |> List.distinct
        |> List.filter(fun e -> e.Id <> PointerToStateFinal.Id)

    let distinctEmptyPathToFinal (lst:StatePointer list) =
        let toReduce =
            lst
            |>  List.filter(fun e -> 
                let nd = lookup e
                nd.IsEmptyPathToFinal
            )
            |>  List.distinct

        if toReduce.Length > 0 then
            let toRemove = toReduce |> Set.ofList
            toReduce.Head :: (lst |> List.filter(fun e -> not(toRemove.Contains e)))
        else
            lst

    let getSinglePathPointers pt =
        match pt with 
        |   SinglePathPointer _ -> [pt.SinglePathPointerValue]
        |   MultiPathPointer  _ -> 
            match lookup pt with
            |   MultiPath mp -> mp.States |> List.map(fun e -> e.NodePointer)
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
            let mn = 
                sorted 
                |>  List.map(fun e -> UnYieldable.Create e None)
                |>  createMultiPath
            mn


    let createAndSimplifyMultiPathSp (lst:SinglePathPointer list) : StatePointer  =
        createAndSimplifyMultiPath (lst |> List.map(fun i -> i.StatePointer))

    let getRepeatState (ri:RepeatId) = allRepeats |> Seq.find(fun e -> e.RepeatId = ri)


let qcOneInSet ls = ls |> List.fold(fun s i -> s ||| uint32(i)) 0u

let createSinglePathFromRgx rgx =
    match rgx with
    |   Plain d when d.``fixed`` = "^" && d.Token = [Token.NoToken] -> MT.createStartLinePath PointerToStateFinal (MT.noMatch())
    |   Plain d when d.Token = [Token.EOF] -> 
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



let specificTokens =
    let hsh = HashSet<Token>()
    [   Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark``; Token.``t-colon``;
        Token.``t-dot``; Token.``t-square-bracket-start``; Token.``t-square-bracket-end``; Token.``t-curly-bracket-start``; Token.``t-curly-bracket-end``;
        Token.``t-hash``; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``; Token.``t-gt``;
        Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``; Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``;
        Token.``ns-dec-digit``; Token.``c-escape``; Token.``t-comma``]
    |>   List.iter(fun i -> hsh.Add i |> ignore)
    hsh

#if LOGTRACE
let logtree sp =
    sp
    |>  MT.ToNFAMachine
    |>  SPrintIt 5
    |>  fun t -> logger.Trace(sprintf "\n%s" t)
#endif

module Duplication = 
    type DuplicateWithCommand = {
        LinkTo  : StatePointer option
        Replace : (StatePointer -> StatePointer) option
    }
    with
        static member WithNoCommand = { LinkTo = None; Replace = None }
        static member WithLinkTo sp = { LinkTo = Some sp; Replace = None }
        static member WithReplace op = { LinkTo = None; Replace = Some op}

    type private DuplicateParam = {
        Current:        StatePointer
        SourceToTarget : Dictionary<StatePointer, StatePointer>
    }
    with    
        static member Create c st = { Current = c; SourceToTarget = st }

    type private DuplicateReturn = {
        Link           : StatePointer
        SourceToTarget : Dictionary<StatePointer, StatePointer>
    }
    with
        static member Create st l = { Link = l; SourceToTarget = st}

    module private DuplicateParam =
        let SetCurrent c (p:DuplicateParam)  = { p with Current = c }
        let SetStT     st (p:DuplicateParam) = { p with SourceToTarget = st}
        let AddMapped  k m (p:DuplicateParam) = 
            let dict = p.SourceToTarget
            dict.Remove(k) |> ignore
            dict.Add(k, m)
            { p with SourceToTarget = dict}

    module private DuplicateReturn =
        let LinkTargetToSource s (r:DuplicateReturn) = 
            if r.SourceToTarget.ContainsKey s then r 
            else
                let dict = r.SourceToTarget 
                dict.Add(s, r.Link)
                { r with SourceToTarget = dict}

    let duplicateStructureAndLinkToNext (premapped:(StatePointer*StatePointer) list) (entryStartId:StatePointer) (command: DuplicateWithCommand) =
        let preMappedNodes =
            let dict = Dictionary<StatePointer,StatePointer>()
            premapped |> List.iter(fun (k,v) -> dict.Add(k,v))
            match command.LinkTo with
            |   Some cp -> dict.Add(cp, cp)
            |   None -> ()
            dict

        let dup (p:DuplicateParam) (r:DuplicateReturn) = 
            let tr = 
                if p.SourceToTarget.ContainsKey r.Link then p.SourceToTarget.[r.Link]
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

            let replaceNoNext dp : DuplicateReturn = 
                match command.Replace with
                |   Some check -> 
                    let ck = check dp.Current
                    if ck = dp.Current then
                        dp.Current
                        |>  DuplicateReturn.Create p.SourceToTarget
                    else 
                        preMappedNodes.Add(dp.Current, ck) |> ignore
                        ck
                        |>  DuplicateReturn.Create p.SourceToTarget
                        |>  DuplicateReturn.LinkTargetToSource dp.Current
                |   None -> 
                    dp.Current
                    |>  DuplicateReturn.Create p.SourceToTarget
                    

            if  p.Current.Id = 0u || p.SourceToTarget.ContainsKey p.Current then 
                if p.SourceToTarget.ContainsKey p.Current then p.SourceToTarget.[p.Current] else p.Current
                |>  DuplicateReturn.Create p.SourceToTarget
            else
                let node = MT.lookup p.Current
                match node with
                |   MultiPath d -> 
                    d.States 
                    |> List.fold(fun (npt,st) stid -> 
                        p
                        |>  DuplicateParam.SetCurrent stid.NodePointer.StatePointer
                        |>  DuplicateParam.SetStT st
                        |>  traverse
                        |>  fun rt -> (rt.Link :: npt, rt.SourceToTarget)
                        ) ([], p.SourceToTarget)
                    |>  fun (llst, st) ->
                        MT.createAndSimplifyMultiPath llst
                        |>  DuplicateReturn.Create st

                |   EmptyPath d  when d.NextState.Id = PointerToStateFinal.Id -> 
                    match command.LinkTo with
                    |   Some concatPtr ->
                        MT.duplicateAndLinkToNext concatPtr p.Current
                        |>  DuplicateReturn.Create p.SourceToTarget
                        |>  DuplicateReturn.LinkTargetToSource p.Current
                    |   None -> passthrough d
                |   EmptyPath       d -> passthrough d
                |   StartLinePath   d -> 
                    let nwTr = 
                        p
                        |>  DuplicateParam.SetCurrent d.Iftrue
                        |>  traverse
                    let nwFl =
                        p
                        |>  DuplicateParam.SetCurrent d.IfFalse
                        |>  DuplicateParam.SetStT nwTr.SourceToTarget
                        |>  traverse
                    MT.createStartLinePath nwTr.Link nwFl.Link
                    |>  DuplicateReturn.Create p.SourceToTarget
                    |>  DuplicateReturn.LinkTargetToSource p.Current
                |   RepeatIterOrExit d -> passthrough d
                |   RepeatInit d -> passthrough d
                |   RepeatStart d -> 
                    let rt = DuplicateReturn.Create p.SourceToTarget PointerToStateFinal
                    let dp = dup p rt
                    let nx = 
                        p
                        |>  DuplicateParam.SetCurrent d.NextState
                        |>  DuplicateParam.AddMapped p.Current dp.Link
                        |>  traverse
                    
                    MT.setNextState nx.Link dp.Link
                    |>  DuplicateReturn.Create nx.SourceToTarget
                    |>  DuplicateReturn.LinkTargetToSource p.Current 

                |   SinglePath d ->
                    match command.LinkTo with
                    |   Some concatPtr ->
                        if d.NextState.Id = 0u then
                            MT.setNextState concatPtr p.Current
                            |>  DuplicateReturn.Create p.SourceToTarget
                            |>  DuplicateReturn.LinkTargetToSource p.Current
                        else
                            passthrough d
                    |   None -> passthrough d
                |   NoMatch _   -> DuplicateReturn.Create p.SourceToTarget p.Current
                |   ReturnSub _ -> replaceNoNext p
                |   GoSub d ->
                    let nwNext = 
                        p
                        |>  DuplicateParam.SetCurrent d.NextState
                        |>  DuplicateParam.AddMapped d.StatePointer PointerToStateFinal
                        |>  traverse
                    let gsn = 
                        MT.createGoSub nwNext.Link PointerToStateFinal d.GosubId
                    let nwRet  = 
                        p
                        |>  DuplicateParam.SetCurrent d.ReturnState
                        |>  DuplicateParam.AddMapped d.StatePointer gsn.StatePointer
                        |>  DuplicateParam.SetStT nwNext.SourceToTarget
                        |>  traverse
                    MT.setNextState nwRet.Link gsn
                    |>  DuplicateReturn.Create nwRet.SourceToTarget
                    |>  DuplicateReturn.LinkTargetToSource gsn

        DuplicateParam.Create entryStartId preMappedNodes
        |>  traverse
        |>  fun r -> r.Link


module rec Refactoring =

    type SystemList<'a> = System.Collections.Generic.List<'a>

    type RefactoringParam = {
        RefactoredFromTo : Dictionary<StateId, StateId>
        Combinations     : Dictionary<StateId list, StatePointer list>
        IsRefactored     : HashSet<StateId>
        StatePointers    : StatePointer list
        GosubMapping     : Dictionary<GosubId, StatePointer>
    }
    with
        static member Create s = { RefactoredFromTo = Dictionary<_,_>(); IsRefactored = HashSet<_>();Combinations = Dictionary<_,_>(); GosubMapping=Dictionary<_,_>(); StatePointers = s}
        member this.WithStatePointers s = {this with StatePointers = s}
        member this.Append l = { this with StatePointers = this.StatePointers @ l}
        member this.Add (f:StatePointer) (t:StatePointer) = 
            let dic = this.RefactoredFromTo
            let hsh = this.IsRefactored
            dic.Remove(f.Id) |> ignore
            dic.Add(f.Id,t.Id)
            hsh.Add(t.Id) |> ignore
            { this with RefactoredFromTo = dic; IsRefactored = hsh}

        member this.MarkRefactored t =
            let hsh = this.IsRefactored
            hsh.Add(t) |> ignore
            { this with IsRefactored = hsh}

        member this.RegisterCombination (f:StatePointer list) (t:StatePointer list) =
            let dict = this.Combinations
            let k = f |> List.map(fun e -> e.Id) |> List.sort
            dict.Remove k |> ignore
            dict.Add(k, t)
            { this with Combinations = dict}

        member this.GetRegisteredCombination (f:StatePointer list) =
            let k = f |> List.map(fun e -> e.Id) |> List.sort
            if this.Combinations.ContainsKey k then
                Some this.Combinations.[k]
            else
                None
        member this.SetGosubMapping k v =
            let dict = this.GosubMapping
            dict.Remove k |> ignore
            dict.Add(k, v)
            { this with GosubMapping = dict}

        member this.RmGosubMapping k =
            let dict = this.GosubMapping
            dict.Remove(k) |> ignore
            { this with GosubMapping = dict}


    module RefactoringParam =
        let WithStatePointers (p:RefactoringParam) s = p.WithStatePointers s
        let append l (p:RefactoringParam) = p.Append l
        let add (p:RefactoringParam) f t = p.Add f t
        let markRefactored t (p:RefactoringParam) = p.MarkRefactored t


    type NormalizedSPExactChar = { Token : Token; IdSp : StatePointer; Next : StatePointer ; ExactChar : char }
    type NormalizedSPOneInSet  = { Token : Token; IdSp : StatePointer; Next : StatePointer }

    type NormalizedProcessing = { IdSp : StatePointer; Next : StatePointer }

    type NormalizedForSPRefactoring =
        |   ExactMatchItem of NormalizedSPExactChar
        |   OneInSetItem   of NormalizedSPOneInSet

    
    let createBundle rp =
        rp
        |>  refactorMultiPathStates
        |>  fun rpn -> 
            MT.createAndSimplifyMultiPath rpn.StatePointers, rpn



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
  


    let normalizeForSPRefactoring2 (stNodes:StatePointer list) =
        stNodes
        |>  List.map MT.lookup
        |>  List.fold(fun (normList,irrelst) e ->
            match e with
            |   SinglePath sp -> 
                let norm =
                    match sp.State with
                    |   ExactMatch ch       ->  [ExactMatchItem { Token = ch.ListCheck.Head; IdSp = sp.StatePointer; Next = sp.NextState ; ExactChar = ch.Char}]
                    |   OneInSetMatch ois   ->  
                        ois.ListCheck
                        |>  List.map(fun t  ->   OneInSetItem { Token = t; IdSp = sp.StatePointer; Next = sp.NextState })
                (norm :: normList, irrelst)
            |   _  -> (normList, e.StatePointer :: irrelst)
        ) ([],[])
        |>  fun (normList,irrelst) -> (normList |> List.collect id, irrelst)
    

    let refactorSimplifyStartOfLine chr nxt =
        if chr='\n' then getStartOfLineTrue nxt
        else getStartOfLineFalse nxt


    let refactorCommonPlains2 (rp:RefactoringParam) (sil:(char*StatePointer*(NormalizedProcessing list))list) =
        let empty = lazy(MT.createEmptyPath PointerToStateFinal)
        sil
        |>  List.fold(fun (rps:RefactoringParam,pl) (c, sp, lst) ->
            let (bundle, rpr) =
                lst
                |>  List.map(fun e -> 
                    if e.Next.Id = PointerToStateFinal.Id then empty.Force() else e.Next
                    |>  refactorSimplifyStartOfLine c
                )
                |>  rps.WithStatePointers
                |>  createBundle

            let finbun = refactorSimplifyStartOfLine c bundle
            rpr, MT.duplicateAndLinkToNext finbun sp ::pl
        ) (rp,[])
        |>  fun (rpn, lst) -> rpn.WithStatePointers lst


    let refactorConflictingCharacterSets2 (rp:RefactoringParam) (sil:(Token*(NormalizedProcessing list))list) =
        let prvmaps = Dictionary<StateId list, StatePointer>()
        let toSpToken = Dictionary<StatePointer, SystemList<Token>>()

        let empty = lazy(MT.createEmptyPath PointerToStateFinal)

        let rpn =
            sil
            |>  List.fold(fun (rps:RefactoringParam) (t, lst) ->
                let (bundle, rpn) =
                    let nxtLst = 
                        lst 
                        |> List.map(fun e -> 
                            if e.Next.Id = PointerToStateFinal.Id then empty.Force() else e.Next
                            |>  if t=Token.NewLine then getStartOfLineTrue
                                else getStartOfLineFalse
                        )
                    let nk = nxtLst |> List.map(fun e -> e.Id) |> List.sort
                    if  prvmaps.ContainsKey nk then
                        prvmaps.[nk], rps
                    else
                        let (target,rpn) =
                            nxtLst
                            |>  rps.WithStatePointers
                            |>  createBundle

                        prvmaps.Add(nk,target)
                        target, rpn

                if toSpToken.ContainsKey bundle then
                    let tl = toSpToken.[bundle]
                    tl.Add t
                    rpn
                else
                    let tl = SystemList<Token>()
                    tl.Add t
                    toSpToken.Add(bundle, tl)
                    rpn
            ) rp
 
        toSpToken.Keys
        |>  List.ofSeq
        |>  List.map(fun b ->
            let tl = toSpToken.[b] |> List.ofSeq
            MT.createSinglePath (OneInSetMatch({ ListCheck = tl; QuickCheck = qcOneInSet tl})) b
        )
        |>  rpn.WithStatePointers



    let mergeCompatibleOneInSet2 (rp:RefactoringParam) (sil:(Token*(NormalizedProcessing list))list) =
        let nextToToken = Dictionary<StatePointer, SystemList<Token> >()

        sil
        |>  List.iter(fun (t, nl) -> 
            nl
            |>  List.iter(fun nm ->
                if nextToToken.ContainsKey nm.Next then
                    let l = nextToToken.[nm.Next]
                    l.Add(t)
                else
                   let l = SystemList<Token>()
                   l.Add(t)
                   nextToToken.Add(nm.Next, l) 
            )
        )

        nextToToken.Keys
        |>  List.ofSeq
        |>  List.map(fun nx ->
            let tl  = nextToToken.[nx] |> List.ofSeq
            let newQC = qcOneInSet tl
            MT.createSinglePath (OneInSetMatch({ QuickCheck = newQC; ListCheck = tl})) nx
        )
        |>  rp.WithStatePointers


    let refactorUnprocessedSinglePaths (rp:RefactoringParam) : RefactoringParam =
        rp.StatePointers
        |>  List.fold(fun (srp:RefactoringParam,rl) sp ->

            let notEmpty (irp:RefactoringParam) (nx:StatePointer) gt = 
                if nx.Id = PointerToStateFinal.Id then irp, gt :: rl
                else 
                    let rpn = 
                        irp.WithStatePointers [nx]
                        |>  refactorSingleStatePointer

                    let nwnx = rpn.StatePointers.Head
                    if nwnx.Id <> nx.Id then
                        let nn = MT.duplicateAndLinkToNext nwnx gt
                        let rpnr = rpn.Add gt nn
                        rpnr.WithStatePointers [nn], nn :: rl
                    else
                        irp, gt :: rl


            if sp.Id = PointerToStateFinal.Id then
                (srp, sp :: rl)
            else
                if srp.RefactoredFromTo.ContainsKey sp.Id then
                    let chc = SinglePathPointer.Create srp.RefactoredFromTo.[sp.Id]
                    srp, chc.StatePointer :: rl
                else if srp.IsRefactored.Contains sp.Id then
                    srp, sp :: rl
                else
                    MT.lookup sp
                    |>  function
                        |   StartLinePath   d -> 
                            let nx = d.Iftrue
                            if nx.Id = PointerToStateFinal.Id then (srp, sp :: rl) 
                            else 
                                let rpn = 
                                    srp.WithStatePointers  [nx]
                                    |>  refactorSingleStatePointer

                                let nwnx = rpn.StatePointers.Head
                                let nn =  MT.createStartLinePath nwnx d.IfFalse

                                let rpnr = rpn.Add sp nn
                                RefactoringParam.WithStatePointers rpnr [nn], nn :: rl
                        |   MultiPath       d -> 
                            let nsp, nrp =
                                MT.getSinglePathPointers sp
                                |>  List.map(fun e -> e.StatePointer)
                                |>  srp.WithStatePointers
                                |>  createBundle
                            nrp, nsp :: rl
                        |   RepeatInit      d -> (srp, sp :: rl)
                        |   RepeatIterOrExit d -> (srp, sp :: rl)
                        |   SinglePath      d -> 
                            match d.State with
                            |   ExactMatch em ->
                                let nx = d.NextState
                                if nx.Id = PointerToStateFinal.Id then srp, sp :: rl
                                else 
                                    let rpn = 
                                        srp.WithStatePointers [nx]
                                        |>  refactorSingleStatePointer

                                    let nwnx = rpn.StatePointers.Head
                                    if nwnx.Id <> nx.Id then
                                        let nwnxsim = refactorSimplifyStartOfLine em.Char nwnx
                                        let nn = MT.duplicateAndLinkToNext nwnxsim sp
                                        let rpnr = rpn.Add sp nn
                                        rpnr.WithStatePointers [nn], nn :: rl
                                    else
                                        srp, sp :: rl
                            |   _ -> notEmpty srp d.NextState sp
                        
                        //|   GoSub           d -> 
                        //    let rpp =
                        //        if d.NextState.Id <> d.Id then
                        //            srp.SetGosubMapping d.GosubId d.NextState
                        //        else
                        //            srp
                        //    let (rpp1, nxt) = notEmpty rpp d.NextState sp
                        //    let (rpp2, ret) = notEmpty rpp1 d.ReturnState sp

                        //    let nxtw = MT.createAndSimplifyMultiPath nxt
                        //    let retw = MT.createAndSimplifyMultiPath ret

                        //    let gs = MT.createGoSub nxtw retw d.GosubId

                        //    (rpp2.RmGosubMapping d.GosubId, gs :: rl)
                        |   GoSub           d -> (srp, sp :: rl)
                        |   RepeatStart     d -> (srp, sp :: rl)
                        |   ReturnSub       d -> (srp, sp :: rl)
                        |   EmptyPath       d -> notEmpty srp d.NextState sp
                        |   NoMatch         d -> (srp, sp :: rl)
        ) (rp,[])
        |>  fun (rpn, spl) -> RefactoringParam.WithStatePointers rpn spl


    type SinglePathRefactoringWork = {
        NoProcessing    :   StatePointer list
        CommonPlainRefactoring: (char*StatePointer*NormalizedProcessing list)list 
        NoCharProcessing    : StatePointer list
        OisOverlapRefactoring : (Token*(NormalizedProcessing list))list 
        NoTokenProcessing : (Token * NormalizedProcessing list) list
    }


    let getSinglePathWork (rp:RefactoringParam) =
        let sameCharSPs  = Dictionary<char, Token option * StatePointer * SystemList<NormalizedProcessing> >()
        let sameTokenSPs = Dictionary<Token, SystemList<NormalizedProcessing> >()

        let updateExactChar c t sp p =
            if not(sameCharSPs.ContainsKey c) then
                let lst = SystemList<NormalizedProcessing>()
                lst.Add(p)
                sameCharSPs.Add(c, (t, sp, lst)) |> ignore
            else
                let (st, sp, lst) = sameCharSPs.[c]
                lst.Add(p)
                match st with
                |   Some t -> ()
                |   None -> 
                    sameCharSPs.Remove(c) |> ignore
                    sameCharSPs.Add(c, (t, sp, lst)) |> ignore

        let updateSameTokenSp t p =
            if not(sameTokenSPs.ContainsKey t) then
                let l = SystemList<NormalizedProcessing>()
                l.Add(p)
                sameTokenSPs.Add(t, l) |> ignore
            else
                let prv = sameTokenSPs.[t]
                prv.Add(p)


        let (toProcess,noProcessing) =
            rp.StatePointers
            |>  normalizeForSPRefactoring2

        toProcess
        |>  List.iter(fun i ->
            match i with
            |   ExactMatchItem ni ->
                let strc = {IdSp=ni.IdSp; Next = ni.Next}
                if specificTokens.Contains ni.Token then
                    updateExactChar   ni.ExactChar (Some ni.Token) (ni.IdSp) strc
                    updateSameTokenSp ni.Token strc 
                else
                    updateExactChar   ni.ExactChar None (ni.IdSp) strc

            |   OneInSetItem   oi -> updateSameTokenSp oi.Token {IdSp=oi.IdSp; Next = oi.Next}
        )


        let (commonPlainRefactoring, noCharProcessing) =
            sameCharSPs.Keys
            |>  List.ofSeq
            |>  List.fold(fun (toRefac, noRefac) c ->
                let (token, sp, nmlst) = sameCharSPs.[c]

                let lst =
                    match token with
                    |   Some t -> 
                        let lst =  sameTokenSPs.[t]
                        sameTokenSPs.Remove(t)  |> ignore //  prevent double processing
                        lst
                    |   None -> nmlst

                if lst.Count > 1 then
                    ((c, sp, lst |> List.ofSeq) :: toRefac, noRefac)
                else
                    (toRefac, lst.[0].IdSp :: noRefac)

            ) ([],[])
            

        let (oisOverlapRefactoring, noTokenProcessing) =
            sameTokenSPs.Keys
            |>  List.ofSeq
            |>  List.fold(fun (toRefac, noRefac) t ->
                let lst = sameTokenSPs.[t]

                if lst.Count > 1 then
                    ((t,lst  |> List.ofSeq) :: toRefac, noRefac)
                else
                    let stp = (t, [lst.[0]])
                    (toRefac, stp :: noRefac)
            ) ([],[])


        { NoProcessing = noProcessing; CommonPlainRefactoring = commonPlainRefactoring; NoCharProcessing = noCharProcessing; OisOverlapRefactoring = oisOverlapRefactoring; NoTokenProcessing = noTokenProcessing}


    let refactorSinglePaths (rp:RefactoringParam) =
        let work = getSinglePathWork rp

        let plainsRefactored   = refactorCommonPlains2 rp work.CommonPlainRefactoring
        let plainsRest         = work.NoCharProcessing
        let oneInSetRefactored = refactorConflictingCharacterSets2 plainsRefactored work.OisOverlapRefactoring
        let oneInSetRest       = mergeCompatibleOneInSet2 oneInSetRefactored work.NoTokenProcessing

        work.NoProcessing
        |>  List.append plainsRest 
        |>  List.append oneInSetRest.StatePointers
        |>  RefactoringParam.WithStatePointers oneInSetRest
        |>  refactorUnprocessedSinglePaths
        |>  RefactoringParam.append plainsRefactored.StatePointers
        |>  RefactoringParam.append oneInSetRefactored.StatePointers
        |>  Some


    let refactorMultipleReturns (rp:RefactoringParam) =
        
        let (gsl, npl) =
            rp.StatePointers
            |>  List.fold(fun (gsl, npl) e ->
                MT.lookup e 
                |>  function
                    |   ReturnSub d ->
                        if rp.GosubMapping.ContainsKey d.GosubId then
                            rp.GosubMapping.[d.GosubId] :: gsl, npl
                        else
                           gsl, e :: npl
                    | _ -> gsl, e :: npl
            ) ([],[])

        if gsl.Length < 2 then
            rp
        else
            gsl @ npl
            |>  rp.WithStatePointers


    let getStartOfLineTrue sp : StatePointer =
        MT.lookup sp
        |>  function
            |   StartLinePath sl -> sl.Iftrue
            |   _ -> sp


    let getStartOfLineTrueSp (sp:SinglePathPointer) =
        sp.StatePointer |> getStartOfLineTrue


    let getStartOfLineFalse sp : StatePointer  =
        MT.lookup sp
        |>  function
            |   StartLinePath sl -> sl.IfFalse
            |   _ -> sp


    let getStartOfLineFalseSp (sp:SinglePathPointer) =
        sp.StatePointer |> getStartOfLineFalse


    let mergeStartOfLine (rp:RefactoringParam) =
        let (stnl, stno) =
            rp.StatePointers 
            |>  List.map(fun i -> MT.lookup i)
            |>  List.fold(fun (pr,np) ->
                function
                |   StartLinePath p -> p :: pr, np
                |   nd -> pr, nd.StatePointer :: np
            ) ([],[])

        if stnl.Length = 0 || rp.StatePointers.Length=1  then None
        else
            let empty = lazy(MT.createEmptyPath PointerToStateFinal)
            let allIfTrueIds =
                stnl
                |>  List.map(fun e ->
                    if e.Iftrue.Id = PointerToStateFinal.Id then empty.Force() else e.Iftrue
                    |>  MT.getSinglePathPointers)
                |>  List.collect id
                |>  List.map(getStartOfLineTrueSp >> MT.getSinglePathPointers)
                |>  List.collect id
                |>  List.map(fun e -> e.StatePointer)

            let allIfFalseIds =
                stnl
                |>  List.filter(fun e -> e.IfFalse.Id <> PointerToStateFinal.Id)
                |>  List.filter(fun e -> 
                    let nd = MT.lookup e.IfFalse
                    not(nd.IsNoMatchValue)
                ) 
                |>  List.map(fun e -> MT.getSinglePathPointers e.IfFalse)
                |>  List.collect id
                |>  List.map(getStartOfLineFalseSp >> MT.getSinglePathPointers)
                |>  List.collect id
                |>  List.map(fun e -> e.StatePointer)

            let (bundleTrue, rpnt) = 
                allIfTrueIds @ stno
                |>  rp.WithStatePointers
                |>  createBundle

            let (bundleFalse, rpnf) = 
                allIfFalseIds @ stno
                |>  rpnt.WithStatePointers
                |>  createBundle

            let replace = MT.createStartLinePath bundleTrue bundleFalse

            [replace]
            |>  RefactoringParam.WithStatePointers rpnf
            |>  Some


    let mergeAllGosubs (rp:RefactoringParam) =
        let (gsr, gsrt, np) =
            rp.StatePointers
            |>  List.map(fun i -> MT.lookup i)
            |>  List.fold(fun (pr, prt, np) e ->
                match e with
                |   GoSub p -> p :: pr, prt+1, np
                |   _       -> pr, prt, e.StatePointer :: np
            ) ([], 0, [])


        match gsrt with
        |   0 -> None
        |   1 -> 
            let gsh = gsr |> List.head

            if rp.RefactoredFromTo.ContainsKey gsh.StatePointer.Id then
                let sp = SinglePathPointer.Create rp.RefactoredFromTo.[gsh.StatePointer.Id]
                [sp.StatePointer]
                |>  rp.WithStatePointers
                |>  Some
            else
                let nx = gsh.NextState :: np
                    
                let rpp = 
                    if gsh.ReturnState <> gsh.StatePointer then
                        rp.SetGosubMapping gsh.GosubId gsh.ReturnState
                    else
                        rp

                let (bundle, rpn) =
                    nx
                    |>  rpp.WithStatePointers
                    |>  createBundle

                let (gs, rpgs) =
                    if gsh.StatePointer = gsh.ReturnState then
                        let gs = MT.createGoSub bundle PointerToStateFinal gsh.GosubId
                        MT.setNextState gs gs, rpn
                    else
                        let (rt, rpns) =
                            [gsh.ReturnState]
                            |>  rpn.WithStatePointers
                            |>  refactorUnprocessedSinglePaths
                            |>  createBundle
                        MT.createGoSub bundle rt gsh.GosubId, rpns
                    
                let rpr = rpgs.Add gsh.StatePointer gs
                RefactoringParam.WithStatePointers (rpr.RmGosubMapping gsh.GosubId) [gs]
                |>  Some

        | _ ->
            let (bundle, rpn) =
                let (rpl, spl) =
                    gsr
                    |>  List.fold(fun (st:RefactoringParam, nxl) e -> 
                        if (e.ReturnState <> e.StatePointer) then
                            st.SetGosubMapping e.GosubId e.ReturnState, e.NextState :: nxl
                        else
                            st, e.NextState :: nxl
                    ) (rp, [])

                spl
                |>  List.append np
                |>  rpl.WithStatePointers 
                |>  createBundle

            let (gs, rprs) = 
                gsr
                |>  List.fold(fun (nxt, (rpst:RefactoringParam)) (gsh:GoSubPath) ->
                    if gsh.StatePointer = gsh.ReturnState then
                        let gs = MT.createGoSub nxt PointerToStateFinal gsh.GosubId
                        MT.setNextState gs gs, rpst
                    else
                        let rpstp = rpst.SetGosubMapping gsh.GosubId gsh.ReturnState
                            
                        let (rt, rpns) =
                            [gsh.ReturnState]
                            |>  rpstp.WithStatePointers
                            |>  refactorUnprocessedSinglePaths
                            |>  createBundle
                            
                        MT.createGoSub nxt rt gsh.GosubId, (rpns.RmGosubMapping gsh.GosubId)
                ) (bundle, rpn)

            rprs.WithStatePointers [gs]
            |>  Some



    let getCleanPointers (lst: StatePointer list) =
        lst
        |>  List.map MT.getSinglePathPointers
        |>  List.collect id
        |>  List.map(fun e -> e.StatePointer)
        |>  MT.simplifyMultiPathStates
        |>  List.map MT.cleanupEmptyPaths
        |>  MT.distinctEmptyPathToFinal


    let refactorMultiPathStates (rp:RefactoringParam) : RefactoringParam =
        let rpp = 
            rp.StatePointers
            |>  getCleanPointers
            |>  rp.WithStatePointers

        let (|MergeStartOfLine|_|) rp = mergeStartOfLine rp
        let (|MergeGosubs|_|) rp = mergeAllGosubs rp
        let (|RefactorSinglePaths|_|) rp = rp |> refactorSinglePaths

#if LOGTRACE
        let ids (l:StatePointer list) = l |> List.map(fun e -> e.Id) |> List.sort
#endif

        match rpp.GetRegisteredCombination rpp.StatePointers with
        |   Some rs -> 

#if LOGTRACE
            logger.Trace (sprintf "refactorMultiPathStates cache: %A -> %A" (ids rpp.StatePointers) (ids rs))
#endif
            rpp.WithStatePointers rs
        |   None    ->
            match rpp with
            |   MergeStartOfLine rps -> rps
            |   MergeGosubs rps -> rps
            |   RefactorSinglePaths rps -> rps
            |   _ -> rpp
            |>  fun rpn ->
                rpn.RegisterCombination rpp.StatePointers rpn.StatePointers
#if LOGTRACE
            |> fun rpn ->
            logger.Trace (sprintf "refactorMultiPathStates processed: %A -> %A" (ids rpp.StatePointers) (ids rpn.StatePointers))
            rpn
#endif
    
    let refactorSingleStatePointer (rp:RefactoringParam) : RefactoringParam =
        rp.StatePointers.Head
        |>  MT.getSinglePathPointers
        |>  List.map(fun e -> e.StatePointer)
        |>  rp.WithStatePointers
        |>  refactorMultiPathStates
        |>  fun rpn ->
            [MT.createAndSimplifyMultiPath rpn.StatePointers]
            |>  rpn.WithStatePointers
                
    
    let refactorAll (sp:StatePointer) : StatePointer =
        Refactoring.RefactoringParam.Create [sp]
        |>  refactorSingleStatePointer
        |>  fun rpn -> rpn.StatePointers.Head



    let appendStateIdToAllFinalPathNodes (entryStartId:StatePointer) (concatPtr:StatePointer) =

        let inline setNextState obj nx = MT.setNextState nx obj
    
        let passedNodes = HashSet<StateId>()

        let rec traverse (current:StatePointer) (concatPtr:StatePointer) =
            if  current.Id = 0u || current.Id = concatPtr.Id || passedNodes.Contains (current.Id) then current 
            else
                passedNodes.Add current.Id |> ignore

                let node = MT.lookup current
                match node with
                |   MultiPath  d -> 
                    d.States 
                    |>  List.map(fun stid -> traverse stid.NodePointer.StatePointer concatPtr)
                    |>  fun llst ->
                        llst
                        |>  List.map(MT.cleanupEmptyPaths)
                        |>  MT.createAndSimplifyMultiPath
                |   EmptyPath  d  when d.NextState.Id = PointerToStateFinal.Id -> 
                    MT.setNextState concatPtr node 
                |   EmptyPath     d -> 
                    traverse d.NextState concatPtr
                    |>  setNextState current
                    |>  MT.cleanupEmptyPaths
                |   RepeatIterOrExit    d -> 
                    traverse d.NextState concatPtr
                    |>  MT.cleanupEmptyPaths
                    |>  setNextState current
                |   RepeatInit  d -> 
                    traverse d.NextState.StatePointer concatPtr 
                    |>  setNextState current
                |   RepeatStart d -> 
                    traverse d.NextState concatPtr
                    |>  setNextState current
                |   SinglePath  d ->
                    if d.NextState.Id = PointerToStateFinal.Id then
                        MT.setNextState concatPtr current
                    else
                        traverse d.NextState concatPtr 
                        |>  setNextState current
                |   StartLinePath d -> 
                    let nt = 
                        if d.Iftrue.Id = PointerToStateFinal.Id then concatPtr
                        else traverse d.Iftrue concatPtr

                    { d with Iftrue = nt}
                    |>  StartLinePath
                    |>  MT.updateAndReturn

                |   GoSub d ->
                    if d.NextState.Id = 0u then
                        MT.setNextState concatPtr current
                    else
                        let gs = traverse d.NextState   concatPtr
                        let rt = traverse d.ReturnState concatPtr 
                        GoSub { d with NextState = gs; ReturnState = rt}
                        |>  MT.updateAndReturn
                |   ReturnSub _ -> current
                |   NoMatch   _ -> current
        traverse entryStartId concatPtr


    let addGroupToSinglePathNodes (entryStartId:StatePointer) (gi:GroupId) =
        let passedNodes = HashSet<StateId>()

        let rec traverse (current:StatePointer) =
            if  current.Id = 0u || passedNodes.Contains (current.Id) then ()
            else
                passedNodes.Add current.Id |> ignore

                let node = MT.lookup current
                match node with
                |   MultiPath           d -> d.States |>  List.iter(fun stid -> traverse stid.NodePointer.StatePointer)
                |   EmptyPath           d -> traverse d.NextState
                |   RepeatIterOrExit    d -> traverse d.NextState 
                |   RepeatInit          d -> traverse d.NextState.StatePointer
                |   RepeatStart         d -> traverse d.NextState 
                |   SinglePath  d ->
                    let ngil = gi :: d.GroupIds
                    SinglePath {d with GroupIds = ngil}
                    |>  MT.updateAndReturn |> ignore
                    traverse d.NextState
                |   StartLinePath d -> 
                    traverse d.Iftrue 
                    traverse d.IfFalse
                |   GoSub d ->
                    traverse d.NextState
                    if d.ReturnState <> current then
                        traverse d.ReturnState
                |   ReturnSub _ -> ()
                |   NoMatch   _ -> ()
        traverse entryStartId

    let convertRepeaterToExplicitGraph (start:StatePointer) =
        let convertRepeaterToExplicitTree (ri:RepeatStateRef) (rs:RepeatStateRef) (rioe:RepeatIterateOrExit) (rt:RepeatState) =
            let finalPath nx =
                let iterPath = Duplication.duplicateStructureAndLinkToNext [(rioe.StatePointer, nx);(rs.StatePointer, nx)] rioe.IterateState (Duplication.DuplicateWithCommand.WithLinkTo nx)
                if rt.Min > 0 then
                    iterPath
                else
                    (MT.getSinglePathPointers iterPath) @ (MT.getSinglePathPointers rioe.NextState)
                    |>  MT.createAndSimplifyMultiPathSp

            if rt.Max > 0 then
                let gosubId = MT.CreateGosubId()
                let rtNode = MT.createRetSub gosubId

                let mandatoryPath = Duplication.duplicateStructureAndLinkToNext [(rioe.StatePointer, rtNode);(rs.StatePointer, rtNode)] rioe.IterateState (Duplication.DuplicateWithCommand.WithLinkTo rtNode)
                let optionalPath =
                    lazy(
                        (MT.getSinglePathPointers mandatoryPath) @ (MT.getSinglePathPointers rioe.NextState)
                        |>  MT.createAndSimplifyMultiPathSp
                    )

                [rt.Max .. -1 .. 2]
                |>  List.fold(fun nxt cur ->
                    if cur > rt.Min then
                        MT.createGoSub (optionalPath.Force()) nxt gosubId
                    else
                        MT.createGoSub mandatoryPath nxt gosubId
                ) rioe.NextState
                |>  finalPath
            else if rt.Max = 0 then MT.createEmptyPath rioe.NextState
            else
                let gosubId = MT.CreateGosubId()
                let rtNode = MT.createRetSub gosubId

                let mandatoryPath = Duplication.duplicateStructureAndLinkToNext [(rioe.StatePointer, rtNode);(rs.StatePointer, rtNode)] rioe.IterateState (Duplication.DuplicateWithCommand.WithLinkTo rtNode)
                let optionalPath =
                    (MT.getSinglePathPointers mandatoryPath) @ (MT.getSinglePathPointers rioe.NextState)
                    |>  MT.createAndSimplifyMultiPathSp

                let infiniteLoop =
                    MT.createGoSub optionalPath PointerToStateFinal gosubId

                let infiniteLoop =
                    MT.setNextState (infiniteLoop.StatePointer) infiniteLoop

                [rt.Min .. -1 .. 2]
                |>  List.fold(fun nxt _ ->
                    MT.createGoSub mandatoryPath nxt gosubId
                ) infiniteLoop
                |>  finalPath


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
        |   _ -> 
            start



let PrintIt (nfa:NFAMachine) =
    printfn "%s" (SPrintIt 0 nfa)

    nfa.States
    |>  List.sortBy(fun s -> s.Id)
    |>  List.iter(fun s ->
        match s with
        |   SinglePath sp ->
            match sp.State with
            |   OneInSetMatch    ois ->
                printfn "{ id: %d, OiS, Tokens: [%A] }" sp.Id ois.ListCheck
            |   _ -> ()
        |   _ -> ()
    )


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
            let repPath = o |> convert |>  Refactoring.convertRepeaterToExplicitGraph
            let repState = MT.createRepeatState min max
            let repExit = MT.createRepeatIterOrExit repPath repState.RepeatId linkState
            let repeatLoopStart = MT.createRepeatStart repState.RepeatId repExit
            
            let nx = Refactoring.appendStateIdToAllFinalPathNodes repPath repeatLoopStart

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

        |   OneInSet _ -> 
            convert rgx
        |   Concat l -> 
            let linkState = MT.createEmptyPath PointerToStateFinal
            let converts =
                l
                |>  List.map(convert)

            converts
            |>  List.fold(fun (concatPtr:StatePointer) (entryStart:StatePointer) ->
                    let append = Refactoring.appendStateIdToAllFinalPathNodes entryStart concatPtr
                    let convert = Refactoring.convertRepeaterToExplicitGraph append

                    convert
                    ) linkState
        |   Or     l ->
            l
            |>  List.map(convert >> MT.cleanupEmptyPaths >> Refactoring.convertRepeaterToExplicitGraph)
            |>  List.map(fun sp -> sp)
            |>  List.fold(fun sil sp ->
                    let mpl = 
                        MT.lookup sp
                        |>  function
                            |   MultiPath  mp -> mp.States |> List.map(fun e -> e.NodePointer)
                            |   _             -> [sp.SinglePathPointerValue]
                    sil @ mpl
            ) []
            |>  MT.createAndSimplifyMultiPathSp
        |   Optional    r -> createRepeat r 0 1
        |   ZeroOrMore  r -> createRepeat r 0 -1
        |   ZeroOrMoreNonGreedy  r -> createRepeat r 0 -1
        |   OneOrMore   r -> createRepeat r 1 -1
        |   OneOrMoreNonGreedy r -> createRepeat r 1 -1
        |   Group       r ->
            let gp = convert r |> Refactoring.convertRepeaterToExplicitGraph
            let gs = MT.createGroup()
            Refactoring.addGroupToSinglePathNodes gp gs
            gp
        |   IterRange (irx,mxo,mno) ->
            match mno with
            |   Some minVal -> createRepeat irx minVal mxo
            |   None        -> createRepeat irx mxo mxo  
        |   _ -> failwith "Not Implemented Yet"
    processConversion rgx
    |>  Refactoring.convertRepeaterToExplicitGraph
    |>  Refactoring.refactorAll
    |>  MT.ToNFAMachine


type ParseResult = {
    IsMatch     : bool
    FullMatch   : char list
    Groups      : (char list) list
}

type GroupParse = {
    GroupMatches  : Dictionary<GroupId, char list>
}
with
    static member Create() = 
        let fullMatchId = GroupId MT.fullmatchGroup
        let dict = Dictionary<GroupId, char list>()
        dict.Add(fullMatchId, [])
        { GroupMatches = dict}

    member this.AddChar c gps =
        let dict = this.GroupMatches
        gps
        |>  List.iter(fun gi ->
            if dict.ContainsKey gi then
                let org = dict.[gi]
                dict.Remove gi |> ignore
                dict.Add(gi, c :: org)
            else
                let org = [c]
                dict.Add(gi, org)
        )
        {this with GroupMatches = dict}

    member this.ResetGroups rl =
        let dict = this.GroupMatches
        let resetList = 
            let d = Dictionary<GroupId, int>()
            rl 
            |> List.iter(fun (gi, pos) ->
                d.Add(gi, pos)
            )
            d

        dict.Keys
        |>  List.ofSeq
        |>  List.iter(fun gi ->
            let org = dict.[gi]
            dict.Remove gi |> ignore
            if resetList.ContainsKey gi then
                let diff = org.Length - resetList.[gi]
                dict.Add(gi, org |> List.skip diff)
        )
        {this with GroupMatches = dict}


    member this.GroupPositions 
        with get() =
            let dict = this.GroupMatches
            dict.Keys
            |>  List.ofSeq
            |>  List.map(fun k -> k, dict.[k].Length)

type RepositionInfo = {
    StreamPosition : int
    Character      : TokenData
    StartOfLine    : bool
    GroupPositions : (GroupId*int) list
}
with    
    static member Create sp c sl gp = { StreamPosition =sp ; Character = c ; StartOfLine = sl; GroupPositions = gp }


type RunningState = {
    GroupParse      : GroupParse
    GosubMapping    : Map<GosubId, StatePointer * int>
    LoopToPos       : Map<RepeatId, int>
    CurrentChar     : TokenData
    StartOfLine     : bool
    Stack           : (StatePointer * RepositionInfo option) list
}
with
    static member Create cc st sl = {GroupParse = GroupParse.Create(); GosubMapping = Map.empty<_,_>; LoopToPos = Map.empty<_,_>; CurrentChar = cc; StartOfLine = sl; Stack = [(st, None)]}

    member this.AddChar ch gps = {this with GroupParse = this.GroupParse.AddChar ch gps}

    member this.ResetGroups rl = {this with GroupParse = this.GroupParse.ResetGroups rl}


    member this.SetGosub gi sp =
        let gs =
            this.GosubMapping
            |>  Map.remove gi
            |>  Map.add gi sp
        { this with GosubMapping = gs }


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

    member this.SetCurrentChar cc = { this with CurrentChar = cc }

    member this.Push st = { this with Stack = st :: this.Stack }
    member this.Pop()   = 
        match this.Stack with
        |   [] ->     None, this
        |   h :: t -> Some(h), { this with Stack = t } 

    member this.SetStartOfLine b = { this with StartOfLine = b }


module RunningState =
    let AddChar ch gps (rs:RunningState) = rs.AddChar ch gps
    let ResetGroups rl (rs:RunningState) = rs.ResetGroups rl
    let Push st (rs:RunningState) = rs.Push st
    let SetGosub gi sp (rs:RunningState) = rs.SetGosub gi sp
    let SetCurrentChar cc (rs:RunningState) = rs.SetCurrentChar cc
    let SetStartOfLine b (rs:RunningState) = rs.SetStartOfLine b

let parseIt (nfa:NFAMachine) (stream:RollingStream<TokenData>) =
    let stMap = Dictionary<StateId, StateNode>()
   
    nfa.States |> List.iter(fun e -> stMap.Add(e.Id, e))

    let NoMatch = { IsMatch = false ; FullMatch = []; Groups = [] }

    let startOfLine =
        if stream.Position = 0 then true
        else
            stream.Position <-  (stream.Position-1)
            let ch = stream.Get()
            ch.Token = Token.NewLine

    let rec processStr (runningState: RunningState)  =
        let (st, runningState) = runningState.Pop()
        
        match st with
        |   None    ->  NoMatch
        |   Some (cs, rpinfopt) ->
            let runningState = 
                match rpinfopt with
                |   Some rpinf -> 
                    stream.Position <- rpinf.StreamPosition
                    runningState.ResetGroups rpinf.GroupPositions
                    |>  RunningState.SetCurrentChar rpinf.Character
                |   None   -> runningState

            let AdvanceToNextChar (runningState: RunningState) = 
                let chk = (stream.Get())
                (runningState.SetCurrentChar chk) 

            if cs = PointerToStateFinal then
                stream.Position <- (stream.Position-1)

                let postProcessGroup (chlst: char list) =
                    chlst
                    |>  List.skipWhile(fun c -> c = '\x00' )
                    |>  List.rev

                let gs = 
                    runningState.GroupParse.GroupMatches.Keys
                    |>  Seq.toList
                    |>  List.filter(fun e -> e.Id <> MT.fullmatchGroup)
                    |>  List.map(fun k -> k, runningState.GroupParse.GroupMatches.[k])
                    |>  List.sortBy(fun (i, _) -> i)
                    |>  List.map snd
                    |>  List.map postProcessGroup
                    |>  List.rev
                { IsMatch = true; FullMatch = runningState.GroupParse.GroupMatches.[GroupId MT.fullmatchGroup] |> postProcessGroup; Groups = gs }
            else
#if LOGTRACE
                logger.Trace(sprintf "Id: %d, Pos: %d, Char: '%s'" cs.Id stream.Position (Regex.Escape(runningState.CurrentChar.Source)))
#endif
                let st = stMap.[cs.Id]
                match st with
                |   SinglePath p ->
                    if (p.State.Match runningState.CurrentChar) then
                        if p.IsEOF then
                            runningState
                            |>  RunningState.AddChar (runningState.CurrentChar.Source.[0]) p.GroupIds
                            |>  RunningState.Push(p.NextState, None)
                            |>  RunningState.SetStartOfLine (runningState.CurrentChar.Token = Token.NewLine)
                        else
                            runningState
                            |>  RunningState.AddChar (runningState.CurrentChar.Source.[0]) p.GroupIds
                            |>  RunningState.Push(p.NextState, None)
                            |>  RunningState.SetStartOfLine (runningState.CurrentChar.Token = Token.NewLine)
                            |>  AdvanceToNextChar
                    else 
                        runningState
                |   StartLinePath st ->
                    if runningState.StartOfLine then 
                        runningState.Push (st.Iftrue, None) 
                    else 
                        runningState.Push (st.IfFalse, None) 
                |   MultiPath p ->
                    let pos = stream.Position
                    p.States
                    |>  List.rev
                    |>  List.fold(fun (st:RunningState) i -> st.Push(i.NodePointer.StatePointer, Some(RepositionInfo.Create pos runningState.CurrentChar runningState.StartOfLine runningState.GroupParse.GroupPositions))) runningState
                |   EmptyPath p -> runningState.Push (p.NextState, None) 
                |   RepeatStart _ 
                |   RepeatInit _ 
                |   RepeatIterOrExit _ ->
                    failwith "These should have been refactored away.."
                |   NoMatch _ -> runningState
                |   GoSub gs -> 
                    runningState
                    |>  RunningState.SetGosub gs.GosubId (gs.ReturnState, stream.Position)
                    |>  RunningState.Push (gs.NextState , None)
                |   ReturnSub rs -> 
                    let ill = SinglePathPointer.Create (System.UInt32.MaxValue)
                    let (sp, gp) = runningState.GosubMapping.[rs.GosubId]
                    if stream.Position > gp then
                        runningState
                        |>  RunningState.SetGosub rs.GosubId (ill.StatePointer, stream.Position)
                        |>  RunningState.Push (sp, None)
                    else
                        runningState
                |>  processStr
    processStr (RunningState.Create (stream.Get()) nfa.Start startOfLine) 




let clts (cl:char list) = System.String.Concat(cl)

module ParseResult =
    let IsMatch pr = pr.IsMatch
    let FullMatch pr = pr.FullMatch


