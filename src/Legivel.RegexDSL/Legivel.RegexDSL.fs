module Legivel.Utilities.RegexDSL 

open System.Text.RegularExpressions

#nowarn "52" // "value has been copied to ensure the original is not mutated"

open System

open Legivel.Tokenizer

exception RegexException of string

type [<NoComparison>] Plain =
     {
        ``fixed`` : string
        mutable optimized : RGXType list
        Token     : Token list
     }
     override this.ToString() = sprintf "%s" this.``fixed``

     member this.OptimizeOnce() = 
        if (this.optimized = []) then
            if this.Token.Head <> Token.EOF then
                let unescapedString = 
                    this.``fixed``
                        .Replace("\\{","{")
                        .Replace("\\}","}")
                        .Replace("\\[","[")
                        .Replace("\\]","]")
                        .Replace("\\(","(")
                        .Replace("\\)",")")
                        .Replace("\\|","|")
                        .Replace("\\+","+")
                        .Replace("\\*","*")
                        .Replace("\\.",".")
                        .Replace("\\?","?")
                        .Replace("\\z", "")
                        .Replace("\\\\","\\")
            
                this.optimized <-
                    unescapedString.ToCharArray()
                    |>  List.ofArray
                    |>  List.map(fun c -> Plain <| Plain.Create (c.ToString()) this.Token)
                    |>  List.rev
            else
                this.optimized <- [Plain(Plain.Create "" [Token.EOF])]

     static member (+) (r1:Plain, r2:Plain) = 
        let appd = r1.``fixed`` + r2.``fixed``
        {``fixed`` = appd; ``optimized`` = []; Token = r1.Token @ r2.Token}

     static member Create r t = {``fixed`` = r; Token = t; optimized = []}


and [<NoComparison>] OneInSet =
    {
        not      : bool
        mainset  : string
        subtractset : string
        OneInSet : Lazy<string>
        Token'     : Lazy<Token list>
        TokenQuickCheck : Lazy<uint32>
    }
    static member subtractable = [
            Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark`` 
            Token.``t-colon`` ; Token.``t-comma``; Token.``t-dot`` ; Token.``t-square-bracket-start`` ; Token.``t-square-bracket-end`` ; Token.``t-curly-bracket-start``
            Token.``t-curly-bracket-end`` ; Token.``t-hash`` ; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``
            Token.``t-gt``; Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``;Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``
            Token.``c-escape``; 
            ]

    static member OptimizeSet (ms:string) (ss:string) =
        let sar = ss.ToCharArray()
        ms.ToCharArray()
        |>  Array.filter(fun c -> not (Array.exists(fun ce -> ce = c) sar))
        |>  fun ca -> new string(ca)

    override this.ToString() =
        let subtract = this.subtractset <> ""
        match (subtract, this.not) with 
        //  https://msdn.microsoft.com/en-us/library/20bw873z(v=vs.110).aspx#Anchor_13
        |   (true, true)    -> sprintf "[%s-[^%s]]" (this.subtractset) (this.mainset)
        |   (true, false)   -> sprintf "[%s-[%s]]"  (this.mainset) (this.subtractset)
        |   (false, true)   -> sprintf "[^%s]" (this.mainset)
        |   (false, false)  -> sprintf "[%s]" (this.mainset)

    static member (-) (r1:OneInSet, r2:OneInSet) =
        let subtr = lazy(r2.Token'.Force() |> List.filter(fun e -> OneInSet.subtractable |> List.exists(fun s -> e=s)))
        let tokens = lazy(r1.Token'.Force() |> List.filter(fun tf -> subtr.Force() |> List.exists(fun te -> te = tf) |> not))
        let ms = r1.mainset
        let ss =  r1.subtractset + r2.mainset
        {mainset = ms; subtractset = ss; OneInSet = lazy(OneInSet.OptimizeSet ms ss); not = r1.not; Token' = tokens; TokenQuickCheck = lazy(tokens.Force() |> List.fold(fun s i -> s ||| uint32(i)) 0u) }

    static member (-) (r1:OneInSet, r2:Plain) =
        let subtr = lazy(r2.Token |> List.filter(fun e -> OneInSet.subtractable |> List.exists(fun s -> e=s)))
        let tokens = lazy(r1.Token'.Force() |> List.filter(fun tf -> subtr.Force() |> List.exists(fun te -> te = tf) |> not))
        let ms = r1.mainset
        let ss = r1.subtractset + r2.``fixed``
        {mainset = ms; subtractset = ss; OneInSet = lazy(OneInSet.OptimizeSet ms ss); not = r1.not; Token' = tokens; TokenQuickCheck = lazy(tokens.Force() |> List.fold(fun s i -> s ||| uint32(i)) 0u)}

    static member (+) (r1:OneInSet, r2:OneInSet) =
        let tokens = lazy(r1.Token'.Force() @ r2.Token'.Force()) 
        let ms = r1.mainset + r2.mainset
        let ss = r1.subtractset + r2.subtractset
        {mainset = ms; subtractset = ss; OneInSet = lazy(OneInSet.OptimizeSet ms ss); not = r1.not; Token' = tokens; TokenQuickCheck = lazy(tokens.Force() |> List.fold(fun s i -> s ||| uint32(i)) 0u) }

    static member (+) (_:OneInSet, _:Plain) =
        failwith "Unsupported RGX addition"

    static member Create r tl = 
        {mainset= r; subtractset = ""; OneInSet = lazy(OneInSet.OptimizeSet r ""); not = false; Token' = lazy(tl); TokenQuickCheck = lazy(tl |> List.fold(fun s i -> s ||| uint32(i)) 0u)}


    member this.Token with get () = this.Token'.Force()

    member this.Not() = 
        {this with not = true}


and [<NoComparison>] RGXType =
    |   Plain of Plain
    |   OneInSet   of OneInSet
    |   Or         of RGXType list
    |   Concat     of RGXType list
    |   IterRange  of RGXType * int * (int option)
    |   ZeroOrMore of RGXType
    |   ZeroOrMoreNonGreedy of RGXType
    |   OneOrMore  of RGXType
    |   OneOrMoreNonGreedy  of RGXType
    |   Optional   of RGXType
    |   Group      of RGXType

    member this.ToStringOnce() = 
        let str =
            match this with
            |   Plain    r -> r.ToString()
            |   OneInSet r -> r.ToString()
            |   Or       l ->
                    let l = l |> List.rev
                    let body = l.Tail |> List.fold(fun s e -> sprintf "(?:%s)|%O" s e) (sprintf "%O" l.Head)
                    sprintf "(?:%s)" body
            |   Concat   l ->
                    let l = l |> List.rev
                    l.Tail |> List.fold(fun s e -> sprintf "(?:%s)%O" s e) (sprintf "%O" l.Head)
            |   IterRange(t,mx,mno) ->
                    match mno with
                    |   Some(mn) ->  sprintf "(?:%O){%d,%d}" t mn mx
                    |   None     ->  sprintf "(?:%O){%d}" t mx
            |   ZeroOrMore t -> sprintf "(?:%O)*" t
            |   ZeroOrMoreNonGreedy t -> sprintf "(?:%O)*?" t
            |   OneOrMore  t -> sprintf "(?:%O)+" t
            |   OneOrMoreNonGreedy  t -> sprintf "(?:%O)+?" t
            |   Optional   t -> sprintf "(?:%O)?" t
            |   Group      t -> sprintf "(%O)" t
        str

    override this.ToString() = this.ToStringOnce()

    static member private DoConcat (r1:RGXType, r2:RGXType) = 
        match (r1,r2) with
        |   (Concat c1, _) -> Concat(r2 :: c1)
        |   _   -> Concat([r2; r1])

    static member (|||) (r1:RGXType, r2:RGXType) =
        match (r1,r2) with
        |   (OneInSet o1, OneInSet o2)   -> OneInSet(o1 + o2)
        |   _ ->
            match r1 with
            | Or     l ->   Or(r2 :: l)
            | _       ->    Or([r2; r1])

    static member (-) (r1:RGXType, r2:RGXType) =
        match (r1,r2) with
        |   (OneInSet o1, OneInSet o2)  -> OneInSet(o1 - o2)
        |   (OneInSet o1,    Plain p1)  -> OneInSet(o1 - p1)
        |   _   -> failwith "Unsupported RGX subtraction"

    static member (+) (r1:RGXType, r2:RGXType) =
        match (r1,r2) with
        |   (Plain p1   , Plain p2)      -> RGXType.DoConcat(r1, r2) // Plain(p1 + p2)
        |   (OneInSet o1, OneInSet o2)   -> OneInSet(o1 + o2)
        |   _   ->  RGXType.DoConcat(r1, r2)

    member this.Not = 
        match this with
        |   OneInSet o1 ->  OneInSet(o1.Not())
        |   _   -> failwith "Unsupported Not-case"


/// Regex pattern must repeat exactly given value, eg: Range(RGP("abc"), 2) := (abc){2}
let Repeat(t, mx) = IterRange(t, mx, None)

/// Regex pattern may repeat within given range, eg: Range(RGP("abc"), 2, 3) := (abc){2,3}
let Range(t, mn, mx) = IterRange(t, mx, Some(mn))

/// Regex pattern may repeat zero or more, eg: ZOM(RGP("abc")) := (abc)*
let ZOM(t) = ZeroOrMore(t)

/// Regex pattern may repeat zero or more - nongreedy, eg: ZOM(RGP("abc")) := (abc)*
let ZOMNG(t) = ZeroOrMoreNonGreedy(t)

/// Regex pattern may repeat once or more, eg: OOM(RGP("abc")) := (abc)+
let OOM(t) = OneOrMore(t)

/// Regex pattern may repeat once or more - non greedy, eg: OOM(RGP("abc")) := (abc)+
let OOMNG(t) = OneOrMore(t)

/// Make Regex optional, eg: OPT(RGP("abc")) := (abc)?
let OPT(t) = Optional(t)

/// Plain regex pattern, eg: RGP("abc") := abc
let RGP (c,tl) = Plain(Plain.Create c tl)

/// One in Set regex pattern, eg: RGO("a-zA-Z") := [a-zA-Z]
let RGO (c,tl) = OneInSet(OneInSet.Create c tl)

/// Exclude Set regex pattern, eg: NOT(RGO("a-zA-Z")) := [^a-zA-Z]
let NOT (c:RGXType) = c.Not


/// Regex ToString - match from string start
let RGS p = sprintf "\\A(%O)" p

/// Regex ToString - full string match
let RGSF p = sprintf "\\A(%O)\\z" p

/// Regex ToString - match anywhere in the string (FR = free)
let RGSFR p = sprintf "(%O)" p

/// Creates Regex group, eg GRP(RGP("abc")) := (abc)
let GRP p = Group(p)

/// Returns rest-string, where match 'm' is removed from source 's'
let Advance(m : string, s : string) =  s.Substring(m.Length)



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
                |   MultiPath  _ -> failwith "Multipath has no single nextstate"
                |   NoMatch _ -> failwith "NoMatch has no single nextstate"

        member this.NextStatePtr 
            with get() =
                match this with
                |   SinglePath d -> d.NextState
                |   StartLinePath d -> d.NextState
                |   EmptyPath  d -> d.NextState
                |   RepeatInit d -> d.NextState.StatePointer
                |   RepeatStart d -> d.NextState
                |   RepeatIterOrExit  d -> d.NextState
                |   GroupStart  d -> d.NextState
                |   GroupEnd    d -> d.NextState
                |   MultiPath  _ -> failwith "Multipath has no single nextstate"
                |   NoMatch _ -> failwith "NoMatch has no single nextstate"


        member this.SetNextState i =
                match this with
                |   SinglePath d -> SinglePath { d with NextState = i }
                |   StartLinePath d -> StartLinePath { d with NextState = i }
                |   EmptyPath  d -> EmptyPath { d with NextState = i }
                |   RepeatStart d -> RepeatStart { d with NextState = i }
                |   RepeatIterOrExit  d -> RepeatIterOrExit  { d with NextState = i }
                |   RepeatInit d -> RepeatInit { d with NextState = i }
                |   GroupStart  d -> GroupStart { d with NextState = i }
                |   GroupEnd d -> GroupEnd { d with NextState = i }
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

    let private CreateNewId() =
        currentId <- (currentId + 1u)
        currentId
    
    let private CreateNewRepeatId() =
        currentRepeatId <- (currentRepeatId + 1u)
        RepeatId currentRepeatId

    let private CreateGroupId() =
        currentGroupId <- (currentGroupId + 1u)
        GroupId currentGroupId 

    let mutable private allNodes = Map.empty<StateId, StateNode>
    let mutable private allRepeats = new System.Collections.Generic.List<RepeatState>()

    let Init() =
        currentId       <- 0u
        currentRepeatId <- 0u
        currentGroupId  <- 0u
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
            |   (something, GroupStart gs) -> sortOrder something (gs.NextState |> lookup)
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
            let (MultiPath mp) = lookup pt
            mp.States

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
        if  current.Id = 0u || current.Id = concatPtr.Id || passedNodes.Contains (current.Id) then 
            if pm.ContainsKey current then pm.[current] else current
        else
            let node = MT.lookup current
            match node with
            |   MultiPath d -> 
                d.States 
                |> List.fold(fun npt stid -> (traverse (stid.StatePointer) (passedNodes.Add d.Id) pm) ::  npt) []
                |>  MT.createAndSimplifyMultiPath
            |   EmptyPath d  when d.NextState.Id = PointerToStateFinal.Id -> 
                MT.duplicateAndLinkToNext concatPtr current
            |   EmptyPath       d -> traverse (d.NextState) (passedNodes.Add (d.Id)) pm |> dup pm current
            |   StartLinePath   d -> traverse (d.NextState) (passedNodes.Add (d.Id)) pm |> dup pm current
            |   RepeatIterOrExit d -> traverse (d.NextState) (passedNodes.Add (d.Id)) pm |> dup pm current
            |   RepeatInit d -> 
                traverse (d.NextState.StatePointer) (passedNodes.Add (d.Id)) pm |> dup pm current
            |   RepeatStart d -> 
                let dp = dup pm current PointerToStateFinal
                let nx = traverse (d.NextState) (passedNodes.Add (d.Id)) (pm.Add(current, dp))
                MT.setNextState nx dp
            |   GroupStart d -> traverse (d.NextState.StatePointer) (passedNodes.Add (d.Id)) pm |> dup pm current
            |   GroupEnd   d -> traverse (d.NextState.StatePointer) (passedNodes.Add (d.Id)) pm |> dup pm current
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

                let bundle = 
                    allNextIds |> List.map(fun e -> e.StatePointer)
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
                |>  List.map(fun stid -> traverse stid.StatePointer (passedNodes.Add d.Id) concatPtr)
                |>  refactorMultiPathStates
                |>  MT.createAndSimplifyMultiPathSp
            |   EmptyPath  d  when d.NextState.Id = PointerToStateFinal.Id -> 
                MT.setNextState concatPtr node 
            |   EmptyPath     d -> 
                traverse d.NextState (passedNodes.Add (d.Id)) concatPtr
                |>  setNextState current
                |>  MT.cleanupEmptyPaths
            |   RepeatIterOrExit    d -> 
                traverse d.NextState (passedNodes.Add (d.Id)) concatPtr
                |>  MT.cleanupEmptyPaths
                |>  setNextState current
            |   RepeatInit  d -> 
                traverse d.NextState.StatePointer (passedNodes.Add (d.Id)) concatPtr
                |>  setNextState current
            |   RepeatStart d -> 
                traverse d.NextState (passedNodes.Add (d.Id)) concatPtr
                |>  setNextState current
            |   GroupStart  d -> 
                traverse d.NextState (passedNodes.Add (d.Id)) concatPtr
                |>  setNextState current
            |   GroupEnd    d -> 
                traverse d.NextState (passedNodes.Add (d.Id)) concatPtr
                |>  setNextState current
            |   SinglePath  d ->
                if d.NextState.Id = PointerToStateFinal.Id then
                    MT.setNextState concatPtr current
                else
                    traverse d.NextState (passedNodes.Add (d.Id)) concatPtr
                    |>  setNextState current
            |   StartLinePath d -> 
                if d.NextState.Id = 0u then
                    MT.setNextState concatPtr current
                else
                    traverse d.NextState (passedNodes.Add (d.Id)) concatPtr
                    |>  setNextState current
            |   NoMatch _ -> current
    traverse entryStartId passedNodes concatPtr



let convertRepeaterToExplicitTree (start:StatePointer) =

    let convertRepeaterToExplicitTree (ri:RepeatStateRef) (rs:RepeatStateRef) (rioe:RepeatIterateOrExit) (rt:RepeatState) =
        if rt.Max > 0 then
            [rt.Max .. -1 .. 1]
            |>  List.fold(fun nxt cur ->
                let iterPath = duplicateStructureAndLinkToNext [rioe.StatePointer; rs.StatePointer] [(rioe.StatePointer, nxt);(rs.StatePointer, nxt)] rioe.IterateState nxt

                if cur > rt.Min then
                    (MT.getSinglePathPointers iterPath) @ (MT.getSinglePathPointers rioe.NextState)
                    |>  refactorMultiPathStatesSp
                    |>  MT.createAndSimplifyMultiPathSp
                else
                    iterPath
            ) rioe.NextState
        else
            let finalPath =
                let loopStart = MT.createRepeatStart rs.RepeatId PointerToStateFinal
                let iterDup = duplicateStructureAndLinkToNext [rs.StatePointer] [(rs.StatePointer, loopStart)] rioe.IterateState loopStart
                let mpo =
                    MT.getSinglePathPointers iterDup @ MT.getSinglePathPointers rioe.NextState
                    |>  refactorMultiPathStatesSp
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
            |   StartLinePath ep -> traverse (ep.NextState) (passedNodes.Add (ep.Id)) (ep.Id::used)
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
            |   GroupStart  ep  -> traverse (ep.NextState) (passedNodes.Add (ep.Id)) (ep.Id::used)
            |   GroupEnd ep  -> traverse (ep.NextState) (passedNodes.Add (ep.Id)) (ep.Id::used)
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
            let repPath = convert o |> convertRepeaterToExplicitTree
            let repState = MT.createRepeatState min max
            let repExit = MT.createRepeatIterOrExit repPath repState.RepeatId linkState
            let repeatLoopStart = MT.createRepeatStart repState.RepeatId repExit
            
            let nx = appendStateIdToAllFinalPathNodes repPath repeatLoopStart
            let (RepeatIterOrExit ioe) = MT.lookup repExit
            RepeatIterOrExit {ioe with IterateState = nx}
            |>  MT.updateNode
            |>  ignore

            let repStart = MT.createRepeatInit repState.RepeatId repeatLoopStart
            repStart 

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
                    |>  convertRepeaterToExplicitTree
                    ) linkState
        |   Or     l -> 
            l
            |>  List.map(convert >> MT.cleanupEmptyPaths >> convertRepeaterToExplicitTree)
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
            let gp = convert r |> convertRepeaterToExplicitTree
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
    |>  convertRepeaterToExplicitTree
    |>  MT.ToNFAMachine
    |>  removeUnused
    


type ParseResult = {
    IsMatch     : bool
    FullMatch   : char list
    Groups      : (char list) list
}


let parseIt (nfa:NFAMachine) (stream:RollingStream<TokenData>) =
    let stMap = nfa.States |> List.fold(fun (m:Map<_,_>) i -> m.Add(i.Id, i)) Map.empty<StateId, StateNode>
    let stRepeat = nfa.Repeats |> List.fold(fun (m:Map<_,_>) i -> m.Add(i.RepeatId, i)) Map.empty<RepeatId, RepeatState>
    let runningLoops = Map.empty<RepeatId, RepeatState>
    let groupParse = GroupParse.Create()

    let NoMatch = { IsMatch = false ; FullMatch = []; Groups = [] }

    let startOfLine =
        if stream.Position = 0 then true
        else
            stream.Position <-  (stream.Position-1)
            let ch = stream.Get()
            ch.Token = Token.NewLine

    let rec processStr currentChar cs acc rollback (runningLoops : Map<RepeatId, RepeatState>) (loopToPosMap:Map<RepeatId, int>) (groupParse: GroupParse) startOfLine =
        let processCurrentChar = processStr currentChar
        let processNextChar cs acc rollback (runningLoops : Map<RepeatId, RepeatState>) = 
            let chk = (stream.Get())
            processStr chk cs acc rollback runningLoops 

        if cs = PointerToStateFinal then
            stream.Position <- (stream.Position-1)
            let gs = 
                groupParse.GroupMatches
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
                    processNextChar nxt (ch :: acc) rollback runningLoops loopToPosMap (groupParse.AddChar ch) (currentChar.Token = Token.NewLine)
                else 
                    NoMatch
            |   StartLinePath st ->
                if startOfLine then processCurrentChar (st.NextState) acc (rollback+1) runningLoops loopToPosMap groupParse startOfLine
                else NoMatch
            |   MultiPath p ->
                let pos = stream.Position
                let rec parseMultiPath (sptrLst:SinglePathPointer list) =
                    match sptrLst with 
                    |   []      -> NoMatch 
                    |   h::tail ->
                        processCurrentChar h.StatePointer acc (rollback+1) runningLoops loopToPosMap groupParse startOfLine
                        //match stMap.[h.Id] with
                        //|   SinglePath st -> 
                        //    if st.State.Match currentChar then
                        //        let ch = currentChar.Source.[0]
                        //        processNextChar (st.NextState) (ch :: acc) rollback runningLoops loopToPosMap (groupParse.AddChar ch) (currentChar.Token = Token.NewLine)
                        //    else
                        //        NoMatch
                        //|   EmptyPath  st -> processCurrentChar (st.NextState) acc (rollback+1) runningLoops loopToPosMap groupParse startOfLine
                        //|   GroupStart st -> processCurrentChar (st.NextState) acc (rollback+1) runningLoops loopToPosMap groupParse startOfLine
                        //|   StartLinePath st ->
                        //    if startOfLine then processCurrentChar (st.NextState) acc (rollback+1) runningLoops loopToPosMap groupParse startOfLine
                        //    else NoMatch
                        //|   RepeatStart p  -> 
                        //    let strPos = stream.Position
                        //    let addPos ri p loopToPosMap =
                        //        loopToPosMap
                        //        |>  Map.remove ri
                        //        |>  Map.add ri p

                        //    if loopToPosMap.ContainsKey p.RepeatId then
                        //        if strPos > loopToPosMap.[p.RepeatId] then
                        //            let ln = addPos p.RepeatId strPos loopToPosMap
                        //            processCurrentChar p.NextState acc rollback runningLoops ln groupParse startOfLine
                        //        else
                        //            NoMatch
                        //    else
                        //        let ln = addPos p.RepeatId strPos loopToPosMap
                        //        processCurrentChar p.NextState acc rollback runningLoops ln groupParse startOfLine
                        //|   RepeatIterOrExit re  -> processCurrentChar re.StatePointer acc rollback runningLoops loopToPosMap groupParse startOfLine
                        //|   _ -> failwith "Not implemented yet"
                        |>  fun res -> 
                            if not(res.IsMatch) then
                                stream.Position <- pos
                                parseMultiPath tail
                            else res

                parseMultiPath p.States
            |   EmptyPath p -> processCurrentChar p.NextState acc rollback runningLoops loopToPosMap groupParse startOfLine
            |   RepeatStart p ->
                let strPos = stream.Position
                let addPos ri p loopToPosMap =
                    loopToPosMap
                    |>  Map.remove ri
                    |>  Map.add ri p

                if loopToPosMap.ContainsKey p.RepeatId then
                    if strPos > loopToPosMap.[p.RepeatId] then
                        let ln = addPos p.RepeatId strPos loopToPosMap
                        processCurrentChar p.NextState acc rollback runningLoops ln groupParse startOfLine
                    else
                        NoMatch
                else
                    let ln = addPos p.RepeatId strPos loopToPosMap
                    processCurrentChar p.NextState acc rollback runningLoops ln groupParse startOfLine
            |   RepeatInit r ->
                let rlnew =
                    if runningLoops.ContainsKey r.RepeatId then
                        runningLoops.Remove(r.RepeatId).Add(r.RepeatId, stRepeat.[r.RepeatId])
                    else
                        runningLoops.Add(r.RepeatId, stRepeat.[r.RepeatId])
                processCurrentChar r.NextState.StatePointer acc rollback rlnew loopToPosMap groupParse startOfLine
            |   RepeatIterOrExit r ->
                let rs = runningLoops.[r.RepeatId]
                if rs.MustExit() then
                    processCurrentChar r.NextState acc rollback runningLoops loopToPosMap groupParse startOfLine
                else
                    let pos = stream.Position
                    let rlNew = runningLoops.Remove(r.RepeatId).Add(r.RepeatId, rs.Iterate())
                    let tryIterate = processCurrentChar r.IterateState acc rollback rlNew loopToPosMap groupParse startOfLine
                    if tryIterate.IsMatch then
                        tryIterate
                    else
                        if stream.Position = pos && rs.CanExit() && r.IterateState<>r.NextState then
                            processCurrentChar r.NextState acc rollback (rlNew.Remove rs.RepeatId) loopToPosMap groupParse startOfLine // end loop by removing it from running loops
                        else
                            NoMatch
            |   GroupStart gp -> processCurrentChar gp.NextState acc rollback runningLoops loopToPosMap (groupParse.Start gp.GroupId) startOfLine
            |   GroupEnd   gp -> processCurrentChar gp.NextState acc rollback runningLoops loopToPosMap (groupParse.Stop gp.GroupId) startOfLine
            |   NoMatch _ -> NoMatch

    processStr (stream.Get()) nfa.Start [] 0 runningLoops Map.empty groupParse startOfLine


type LevelType = 
    |   Concat = 0
    |   Multi   = 1
    |   RepeatIter = 4
    |   RepeatExit = 2
    |   LoopStart = 3
    |   Empty     = 5
    |   Group = 6


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
                |   LevelType.Group     -> printf "          "
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
                    |   StartLinePath  ep   ->  
                        printf "^(%2d)" ep.Id
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
                    |   GroupStart gp ->
                        printf "-%2d, (%2d){" gp.GroupId.Id gp.Id 
                        printLineRest (LevelType.Group :: hist) gp.NextState (passedNodes.Add gp.Id)
                    |   GroupEnd gp ->
                        printf "}(%2d)     " gp.Id 
                        printLineRest (LevelType.Group :: hist) gp.NextState (passedNodes.Add gp.Id)
                    |   NoMatch d -> printf "-NoMatch(%2d)\n" d
                else
                    printf "-Loop(%2d)\n" current.Id

        printLineRest hist current passedNodes
    printLine [] nfa.Start passedNodes


let clts (cl:char list) = System.String.Concat(cl)

module ParseResult =
    let IsMatch pr = pr.IsMatch
    let FullMatch pr = pr.FullMatch


