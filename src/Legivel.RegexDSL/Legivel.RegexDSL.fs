module Legivel.Utilities.RegexDSL 

open System.Text.RegularExpressions
open System.Collections.Generic
open System.Text

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
    static member subtractable = 
        let hsh = HashSet<Token>()
        [
            Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark`` 
            Token.``t-colon`` ; Token.``t-comma``; Token.``t-dot`` ; Token.``t-square-bracket-start`` ; Token.``t-square-bracket-end`` ; Token.``t-curly-bracket-start``
            Token.``t-curly-bracket-end`` ; Token.``t-hash`` ; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``
            Token.``t-gt``; Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``;Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``
            Token.``c-escape``; 
        ]
        |>  List.iter(fun t -> hsh.Add(t) |> ignore)
        hsh

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
        let subtr = lazy(r2.Token'.Force() |> List.filter(fun e -> OneInSet.subtractable.Contains e))
        let tokens = lazy(r1.Token'.Force() |> List.filter(fun tf -> subtr.Force() |> List.exists(fun te -> te = tf) |> not))
        let ms = r1.mainset
        let ss =  r1.subtractset + r2.mainset
        {mainset = ms; subtractset = ss; OneInSet = lazy(OneInSet.OptimizeSet ms ss); not = r1.not; Token' = tokens; TokenQuickCheck = lazy(tokens.Force() |> List.fold(fun s i -> s ||| uint32(i)) 0u) }

    static member (-) (r1:OneInSet, r2:Plain) =
        let subtr = lazy(r2.Token |> List.filter(fun e -> OneInSet.subtractable.Contains e))
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

        member this.IsEmptyPathToFinal 
            with get() =
                match this with
                |   EmptyPath d -> d.NextState.Id = PointerToStateFinal.Id
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


type LevelType = 
    |   Concat = 0
    |   Multi   = 1
    |   RepeatIter = 4
    |   RepeatExit = 2
    |   LoopStart = 3
    |   Empty     = 5
    |   Group = 6
    |   Gosub = 7



let SPrintIt (nfa:NFAMachine) =

    let sb = StringBuilder()

    let stMap = nfa.States |> List.map(fun e -> e.Id,e) |> Map.ofList
    let rsMap = nfa.Repeats |> List.map(fun e -> e.RepeatId,e) |> Map.ofList
    let passedNodes = HashSet<StateId>()
    let passedGosub = Set.empty<GosubId>

    let rec printLine (hist : LevelType list) (current: StatePointer) (passedGosub:Set<GosubId>) =
        let printPrefix hist =
            hist
            |>  List.rev
            |>  List.iter(fun i ->
                match i with
                |   LevelType.Concat    -> "            " |> sb.Append |> ignore
                |   LevelType.Group     -> "            " |> sb.Append |> ignore
                |   LevelType.Empty     -> "       " |> sb.Append |> ignore
                |   LevelType.Multi     -> "|      " |> sb.Append |> ignore
                |   LevelType.RepeatExit-> " |X      " |> sb.Append |> ignore
                |   LevelType.RepeatIter-> " |I      " |> sb.Append |> ignore
                |   LevelType.LoopStart -> "                 " |> sb.Append |> ignore
                |   LevelType.Gosub     -> "|           " |> sb.Append |> ignore
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
                        sprintf "^(%4d)" ep.Id |> sb.Append |> ignore
                        printLineRest (LevelType.Empty :: hist) ep.NextState passedGosub
                    |   SinglePath sp   -> 
                        match sp.State with
                        |   ExactMatch c    -> sprintf "-(%4d:%4s)" sp.Id (sprintf "\"%s\"" (Regex.Escape(c.Char.ToString()))) |> sb.Append |> ignore
                        |   OneInSetMatch o -> sprintf "-(%4d: [@])" sp.Id |> sb.Append |> ignore
                        printLineRest (LevelType.Concat :: hist) sp.NextState passedGosub
                    |   MultiPath mp    ->
                        match mp.States with
                        |   h::t -> 
                            sprintf "|(%4d)" mp.Id |> sb.Append |> ignore
                            printLineRest (LevelType.Multi :: hist) h.StatePointer passedGosub
                            t |> List.iter(fun e -> printLine (LevelType.Multi :: hist) e.StatePointer passedGosub) 
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
                    |   GroupStart gp ->
                        sprintf "-%2d, (%4d){" gp.GroupId.Id gp.Id |> sb.Append |> ignore
                        printLineRest (LevelType.Group :: hist) gp.NextState passedGosub
                    |   GroupEnd gp ->
                        sprintf "}(%4d)     " gp.Id |> sb.Append |> ignore
                        printLineRest (LevelType.Group :: hist) gp.NextState passedGosub
                    |   NoMatch d -> sprintf "-NoMatch(%4d)\n" d |> sb.Append |> ignore
                    |   ReturnSub d -> sprintf "-Ret(%4d,%d)" d.Id d.GosubId.Id |> sb.AppendLine |> ignore
                    |   GoSub     d -> 
                        if passedNodes.Contains d.NextState.Id then
                            sprintf "|GS(%4d, %d) -Goto(%4d) - see above\n" d.Id d.GosubId.Id d.NextState.Id |> sb.Append |> ignore
                            printPrefix (hist)
                            sprintf "|GS-Ret    " |> sb.Append |> ignore
                            printLineRest (LevelType.Gosub :: hist) d.ReturnState passedGosub
                        else
                            sprintf "|GS(%4d, %d)" d.Id d.GosubId.Id |> sb.Append |> ignore
                            printLineRest (LevelType.Gosub :: hist) d.NextState (passedGosub.Add d.GosubId)
                            printPrefix (hist)
                            sprintf "|GS-Ret    " |> sb.Append |> ignore
                            printLineRest (LevelType.Gosub :: hist) d.ReturnState (passedGosub.Add d.GosubId)
                else
                    sprintf "-Goto(%4d) - see above\n" current.Id |> sb.Append |> ignore

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

    let private allNodes = Dictionary<StateId, StateNode>()
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
    
    let ToNFAMachine st = NFAMachine.Create (st, (allNodes.Values |> Seq.toList), (allRepeats |> Seq.toList))

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
        let rec sortOrder = 
            function
            |   GroupStart     gs -> sortOrder (gs.NextState |> lookup)
            |   StartLinePath  _ -> 100
            |   MultiPath      _ -> 200
            |   RepeatInit     _ -> 400
            |   RepeatIterOrExit _ -> 500
            |   SinglePath     _ -> 600
            |   GroupEnd       _ -> 700
            |   GoSub          _ -> 900
            |   RepeatStart    _ -> 950
            |   EmptyPath      _ -> 1000
            |   ReturnSub      _ -> 1100
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



let specificTokens =
    let hsh = HashSet<Token>()
    [   Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark``; Token.``t-colon``;
        Token.``t-dot``; Token.``t-square-bracket-start``; Token.``t-square-bracket-end``; Token.``t-curly-bracket-start``; Token.``t-curly-bracket-end``;
        Token.``t-hash``; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``; Token.``t-gt``;
        Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``; Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``;
        Token.``ns-dec-digit``; Token.``c-escape``; Token.``t-comma``]
    |>   List.iter(fun i -> hsh.Add i |> ignore)
    hsh


module Duplication = 
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

    let duplicateStructureAndLinkToNext (premapped:(StatePointer*StatePointer) list) (entryStartId:StatePointer) (concatPtr:StatePointer) =
        
        let preMappedNodes =
            let dict = Dictionary<StatePointer,StatePointer>()
            premapped |> List.iter(fun (k,v) -> dict.Add(k,v))
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

            if  p.Current.Id = 0u || p.Current.Id = concatPtr.Id || p.SourceToTarget.ContainsKey p.Current then 
                if p.SourceToTarget.ContainsKey p.Current then p.SourceToTarget.[p.Current] else p.Current
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
                        |>  DuplicateParam.AddMapped p.Current dp.Link
                        |>  traverse
                    
                    MT.setNextState nx.Link dp.Link
                    |>  DuplicateReturn.Create nx.SourceToTarget
                    |>  DuplicateReturn.LinkTargetToSource p.Current 

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

    type NormalizedSPExactChar = { Token : Token; IdSp : StatePointer; Next : StatePointer ; ExactChar : char }
    type NormalizedSPOneInSet  = { Token : Token; IdSp : StatePointer; Next : StatePointer }

    type NormalizedProcessing = { IdSp : StatePointer; Next : StatePointer }

    type NormalizedForSPRefactoring =
        |   ExactMatchItem of NormalizedSPExactChar
        |   OneInSetItem   of NormalizedSPOneInSet

    
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
    





    let refactorCommonPlains2 (sil:(char*StatePointer*(NormalizedProcessing list))list) =
        let empty = lazy(MT.createEmptyPath PointerToStateFinal)
        sil
        |>  List.map(fun (c, sp, lst) ->
            let bundle =
                lst
                |>  List.map(fun e -> if e.Next.Id = PointerToStateFinal.Id then empty.Force() else e.Next)
                |>  List.map(MT.getSinglePathPointers)
                |>  List.collect id
                |>  refactorMultiPathStates2
                |>  MT.createAndSimplifyMultiPathSp
            
            sp
            |>  MT.duplicateAndLinkToNext bundle
        )


    let refactorConflictingCharacterSets2 (sil:(Token*(NormalizedProcessing list))list) =
        let prvmaps = Dictionary<StateId list, StatePointer>()
        let empty = lazy(MT.createEmptyPath PointerToStateFinal)
        sil
        |>  List.map(fun (t, lst) ->
            let bundle =
                let nxtLst = lst |> List.map(fun e -> if e.Next.Id = PointerToStateFinal.Id then empty.Force() else e.Next)
                let nk = nxtLst |> List.map(fun e -> e.Id) |> List.sort
                if  prvmaps.ContainsKey nk then
                    prvmaps.[nk]
                else
                    let target =
                        nxtLst
                        |>  List.map(MT.getSinglePathPointers)
                        |>  List.collect id
                        |>  refactorMultiPathStates2
                        |>  MT.createAndSimplifyMultiPathSp
                    prvmaps.Add(nk,target)
                    target

            MT.createSinglePath (OneInSetMatch({ ListCheck = [t]; QuickCheck = uint32(t)})) bundle
        )
 

    let mergeCompatibleOneInSet2 (sil:(Token*(NormalizedProcessing list))list) =
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


    let refactorSinglePaths (sil:SinglePathPointer list) =
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
            sil
            |>  List.map(fun p -> p.StatePointer)
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

        let plainsRefactored   = refactorCommonPlains2 commonPlainRefactoring
        let plainsRest         = noCharProcessing
        let oneInSetRefactored = refactorConflictingCharacterSets2 oisOverlapRefactoring
        let oneInSetRest       = mergeCompatibleOneInSet2 noTokenProcessing

        noProcessing
        |>  List.append plainsRefactored
        |>  List.append plainsRest 
        |>  List.append oneInSetRefactored 
        |>  List.append oneInSetRest


    let mergeStartOfLine (sil:SinglePathPointer list) =
        let stnl =
            sil 
            |>  List.map(fun i -> MT.lookup i)
            |>  List.fold(fun st ->
                function
                |   StartLinePath p -> p :: st
                |   _ -> st
            ) []

        if stnl.Length = 0 then
            sil
        else
            let allNextIds =
                let empty = MT.createEmptyPath PointerToStateFinal
                stnl
                |>  List.map(fun e -> 
                    if e.NextState.Id = PointerToStateFinal.Id then empty else e.NextState 
                    |>  MT.getSinglePathPointers
                )
                |>  List.collect id
                |>  List.map(fun e -> e.StatePointer)

            let bundle = 
                allNextIds
                |>  refactorMultiPathStates
                |>  MT.createAndSimplifyMultiPathSp 

            let newSl = stnl.Head.StatePointer |> MT.duplicateAndLinkToNext bundle
            let stMap = stnl |> List.map(fun e -> e.Id) |> Set.ofList

            newSl.SinglePathPointerValue :: (sil |> List.filter(fun e -> stMap.Contains e.Id |> not))


    let mergeInfiniteGosubs (sil : SinglePathPointer list) =
        let stnl =
            sil 
            |>  List.map(fun i -> MT.lookup i)
            |>  List.fold(fun st ->
                function
                |   GoSub p when p.ReturnState.Id = p.Id -> 
                    if not (p.NextState.IsSinglePath) then
                        p :: st
                    else
                        st
                |   _ -> st
            ) []
        if stnl.Length < 2 then
            sil
        else
            let gosubId = MT.CreateGosubId()
            let rtNode = MT.createRetSub gosubId
            let empty = MT.createEmptyPath PointerToStateFinal

            let replaceRet gid (curr:StatePointer) =
                MT.lookup curr.StatePointer
                |>  function
                    |   ReturnSub rp when rp.GosubId = gid -> rtNode
                    |   _ -> curr.StatePointer

            let bundle =
                let origBundle =
                    stnl 
                    |>  List.map(fun gs -> gs.NextState.SinglePathPointerValue)
                    |>  MT.createAndSimplifyMultiPathSp

                let dup = Duplication.duplicateStructureAndLinkToNext [] origBundle empty
                stnl
                |>  List.map(fun gs -> gs.GosubId)
                |>  List.distinct
                |>  List.iter(fun gi -> searchAndReplace (replaceRet gi) dup |> ignore)
                dup
                |>  MT.getSinglePathPointers
                |>  List.map(fun e -> e.StatePointer)
                |>  refactorMultiPathStates
                |>  MT.createAndSimplifyMultiPathSp


            let gs = MT.createGoSub bundle PointerToStateFinal gosubId
            let gs = MT.setNextState gs gs

            let toRemove = stnl |> List.map(fun e -> e.SinglePathPointer) |> List.distinct |> Set.ofList

            gs.SinglePathPointerValue :: (sil |> List.filter(fun e -> not(toRemove.Contains e)))


    let refactorMultiPathStates2 (lst : SinglePathPointer list) =
        lst
        |>  refactorSinglePaths
        |>  List.map(fun p -> p.SinglePathPointerValue)


    let refactorMultiPathStates (lst : StatePointer list) =
        lst
        |>  MT.simplifyMultiPathStates
        |>  List.map MT.cleanupEmptyPaths
        |>  MT.distinctEmptyPathToFinal
        |>  List.map MT.getSinglePathPointers
        |>  List.collect id
        //|>  mergeStartOfLine
        |>  mergeInfiniteGosubs
        |>  refactorMultiPathStates2

    

    let refactorMultiPathStatesSp (lst : SinglePathPointer list) =
        lst
        |>  List.map(fun e -> e.StatePointer)
        |>  refactorMultiPathStates


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
                    |>  List.map(fun stid -> traverse stid.StatePointer concatPtr)
                    |>  List.map MT.lookup
                    |>  MT.SortStateNodes
                    |>  List.map(fun e -> e.StatePointer)
                    //|>  Refactoring.refactorMultiPathStates
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
                |   GroupStart  d -> 
                    traverse d.NextState concatPtr
                    |>  setNextState current
                |   GroupEnd    d -> 
                    traverse d.NextState concatPtr
                    |>  setNextState current
                |   SinglePath  d ->
                    if d.NextState.Id = PointerToStateFinal.Id then
                        MT.setNextState concatPtr current
                    else
                        traverse d.NextState concatPtr 
                        |>  setNextState current
                |   StartLinePath d -> 
                    if d.NextState.Id = 0u then
                        MT.setNextState concatPtr current
                    else
                        traverse d.NextState concatPtr 
                        |>  setNextState current
                |   GoSub d ->
                    if d.NextState.Id = 0u then
                        MT.setNextState concatPtr current
                    else
                        traverse d.NextState   concatPtr |> ignore
                        traverse d.ReturnState concatPtr 
                        |>  setNextState current
                |   ReturnSub d -> 
                    current
                |   NoMatch _ -> 
                    current
        traverse entryStartId concatPtr


    let searchAndReplace (check: StatePointer -> StatePointer) (entryStartId:StatePointer) =
        let passedNodes = HashSet<StateId>()
        
        let inline setNextState obj nx = MT.setNextState nx obj

        let rec traverse (current:StatePointer) =
            let replaceOrNext nxt  =
                let ck = check current
                if ck = current then traverse nxt 
                else 
                    passedNodes.Add ck.Id |> ignore
                    ck

            let replace()  = check current

            if  current.Id = 0u || passedNodes.Contains (current.Id) then current 
            else
                passedNodes.Add current.Id |> ignore
        
                let node = MT.lookup current
                match node with
                |   MultiPath  d -> 
                    d.States 
                    |>  List.map(fun stid -> traverse stid.StatePointer)
                    |>  List.map(fun e -> e.StatePointer)
                    //|>  Refactoring.refactorMultiPathStates
                    |>  MT.createAndSimplifyMultiPath
                |   EmptyPath     d -> replaceOrNext d.NextState |> setNextState current
                |   RepeatIterOrExit   d ->  replaceOrNext d.NextState |> setNextState current
                |   RepeatInit  d -> replaceOrNext d.NextState |> setNextState current
                |   RepeatStart d -> replaceOrNext d.NextState |> setNextState current
                |   GroupStart  d -> replaceOrNext d.NextState |> setNextState current 
                |   GroupEnd    d -> replaceOrNext d.NextState |> setNextState current
                |   SinglePath  d -> replaceOrNext d.NextState |> setNextState current
                |   StartLinePath d -> replaceOrNext d.NextState |> setNextState current
                |   GoSub d ->  
                    let gs = replaceOrNext d.NextState
                    let rt = 
                        if d.ReturnState <> current then
                            replaceOrNext d.ReturnState
                        else
                            gs
                    GoSub { d with NextState = gs; ReturnState = rt}
                    |>  MT.updateAndReturn
                |   ReturnSub d -> replace()
                |   NoMatch _   -> current
        traverse entryStartId 



    let convertRepeaterToExplicitGraph (start:StatePointer) =
        let convertRepeaterToExplicitTree (ri:RepeatStateRef) (rs:RepeatStateRef) (rioe:RepeatIterateOrExit) (rt:RepeatState) =
            let finalPath nx =
                let iterPath =  Duplication.duplicateStructureAndLinkToNext [(rioe.StatePointer, nx);(rs.StatePointer, nx)] rioe.IterateState nx
                if rt.Min > 0 then
                    iterPath
                else
                    (MT.getSinglePathPointers iterPath) @ (MT.getSinglePathPointers rioe.NextState)
                    |>  Refactoring.refactorMultiPathStatesSp
                    |>  MT.createAndSimplifyMultiPathSp

            if rt.Max > 0 then
                let gosubId = MT.CreateGosubId()
                let rtNode = MT.createRetSub gosubId

                let mandatoryPath = Duplication.duplicateStructureAndLinkToNext [(rioe.StatePointer, rtNode);(rs.StatePointer, rtNode)] rioe.IterateState rtNode
                let optionalPath =
                    (MT.getSinglePathPointers mandatoryPath) @ (MT.getSinglePathPointers rioe.NextState)
                    |>  Refactoring.refactorMultiPathStatesSp
                    |>  MT.createAndSimplifyMultiPathSp

                [rt.Max .. -1 .. 2]
                |>  List.fold(fun nxt cur ->
                    if cur > rt.Min then
                        MT.createGoSub optionalPath nxt gosubId
                    else
                        MT.createGoSub mandatoryPath nxt gosubId
                ) rioe.NextState
                |>  finalPath
            else if rt.Max = 0 then MT.createEmptyPath rioe.NextState
            else
                let gosubId = MT.CreateGosubId()
                let rtNode = MT.createRetSub gosubId

                let mandatoryPath = Duplication.duplicateStructureAndLinkToNext [(rioe.StatePointer, rtNode);(rs.StatePointer, rtNode)] rioe.IterateState rtNode
                let optionalPath =
                    (MT.getSinglePathPointers mandatoryPath) @ (MT.getSinglePathPointers rioe.NextState)
                    |>  Refactoring.refactorMultiPathStatesSp
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
        |   _ -> start



    let removeUnused (nfa:NFAMachine) =
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
                |   StartLinePath ep -> traverse (ep.NextState) (Some current.Id)
                |   MultiPath  mp -> 
                    passedNodes.Add(current.Id) |> ignore
                    mp.States 
                    |> List.fold(fun _ stid -> traverse stid.StatePointer None) passedNodes
                |   EmptyPath  ep -> traverse (ep.NextState) (Some current.Id)
                |   RepeatInit rs -> traverse (rs.NextState.StatePointer) (Some current.Id)
                |   RepeatIterOrExit   re  -> 
                    traverse (re.IterateState) (Some current.Id) |> ignore
                    traverse (re.NextState) None
                |   RepeatStart ep -> traverse (ep.NextState) (Some current.Id) 
                |   GroupStart  ep  -> traverse (ep.NextState) (Some current.Id) 
                |   GroupEnd ep  -> traverse (ep.NextState) (Some current.Id) 
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



let PrintIt (nfa:NFAMachine) =
    printfn "%s" (SPrintIt nfa)



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

        |   OneInSet _ -> convert rgx
        |   Concat l -> 
            let linkState = MT.createEmptyPath PointerToStateFinal
            let converts =
                l
                |>  List.map(convert)

            converts
            |>  List.fold(fun (concatPtr:StatePointer) (entryStart:StatePointer) ->
                    Refactoring.appendStateIdToAllFinalPathNodes entryStart concatPtr
                    |>  Refactoring.convertRepeaterToExplicitGraph
                    ) linkState
        |   Or     l -> 
            l
            |>  List.map(convert >> MT.cleanupEmptyPaths >> Refactoring.convertRepeaterToExplicitGraph)
            |>  List.map(fun sp -> sp)
            |>  List.fold(fun sil sp ->
                    let mpl = 
                        MT.lookup sp
                        |>  function
                            |   MultiPath  mp -> mp.States
                            |   _             -> [sp.SinglePathPointerValue]
                    sil @ mpl
            ) []
            |>  Refactoring.refactorMultiPathStatesSp
            |>  MT.createAndSimplifyMultiPathSp
        |   Optional    r -> createRepeat r 0 1
        |   ZeroOrMore  r -> createRepeat r 0 -1
        |   ZeroOrMoreNonGreedy  r -> createRepeat r 0 -1
        |   OneOrMore   r -> createRepeat r 1 -1
        |   OneOrMoreNonGreedy r -> createRepeat r 1 -1
        |   Group       r ->
            let ep = MT.createEmptyPath PointerToStateFinal
            let gp = convert r |> Refactoring.convertRepeaterToExplicitGraph
            let gs = MT.createGroup()
            let ge = MT.createGroupEnd gs ep
            Refactoring.appendStateIdToAllFinalPathNodes gp ge
            |>  MT.createGroupStart gs
        |   IterRange (irx,mxo,mno) ->
            match mno with
            |   Some minVal -> createRepeat irx minVal mxo
            |   None        -> createRepeat irx mxo mxo  
        |   _ -> failwith "Not Implemented Yet"
    processConversion rgx
    |>  Refactoring.convertRepeaterToExplicitGraph
    |>  MT.ToNFAMachine
    |>  Refactoring.removeUnused


type ParseResult = {
    IsMatch     : bool
    FullMatch   : char list
    Groups      : (char list) list
}

type GroupParse = {
    RunningGroups : GroupId list
    GroupMatches  : Dictionary<GroupId, char list>
}
with
    static member Create() = 
        let fullMatchId = GroupId MT.fullmatchGroup
        let dict = Dictionary<GroupId, char list>()
        dict.Add(fullMatchId, [])
        { RunningGroups = [fullMatchId]; GroupMatches = dict}

    member this.AddChar c =
        let dict = this.GroupMatches
        this.RunningGroups
        |>  List.iter(fun gi ->
            let org = dict.[gi]
            dict.Remove gi |> ignore
            dict.Add(gi, c :: org)
        ) 
        {this with GroupMatches = dict}

    member this.Start gi =
        let dict = this.GroupMatches
        dict.Add (gi,[])
        { this with RunningGroups = gi :: this.RunningGroups; GroupMatches = dict}

    member this.Stop gi =
        { this with RunningGroups = this.RunningGroups |> List.filter(fun i -> i<>gi)}

    member this.Rollback n =
        let dict = this.GroupMatches
        this.RunningGroups
        |>  List.iter(fun gi ->
            let org = dict.[gi]
            let rb = org |> List.skip n
            dict.Remove gi |> ignore
            dict.Add(gi, rb)
        ) 
        {this with GroupMatches = dict}

type RepositionInfo = {
    StreamPosition : int
    Character      : TokenData
    StartOfLine    : bool
}
with    
    static member Create sp c sl = { StreamPosition =sp ; Character = c ; StartOfLine = sl }


type RunningState = {
    RunningLoops    : Map<RepeatId, RepeatState>
    GroupParse      : GroupParse
    GosubMapping    : Map<GosubId, StatePointer>
    LoopToPos       : Map<RepeatId, int>
    CurrentChar     : TokenData
    StartOfLine     : bool
    Stack           : (StatePointer * RepositionInfo option) list
}
with
    static member Create cc st sl = {RunningLoops = Map.empty<_,_>; GroupParse = GroupParse.Create(); GosubMapping = Map.empty<_,_>; LoopToPos = Map.empty<_,_>; CurrentChar = cc; StartOfLine = sl; Stack = [(st, None)]}

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

    member this.SetGosub gi sp =
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

    member this.SetCurrentChar cc = { this with CurrentChar = cc }

    member this.Push st = { this with Stack = st :: this.Stack }
    member this.Pop()   = 
        match this.Stack with
        |   [] ->     None, this
        |   h :: t -> Some(h), { this with Stack = t } 

    member this.Rollback r = { this with GroupParse = this.GroupParse.Rollback r}

    member this.SetStartOfLine b = { this with StartOfLine = b }

module RunningState =
    let AddChar ch (rs:RunningState) = rs.AddChar ch
    let Push st (rs:RunningState) = rs.Push st
    let StartGroup gi (rs:RunningState) = rs.StartGroup gi
    let StopGroup gi (rs:RunningState) = rs.StopGroup gi
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
        |   Some (cs, pos) ->
            let runningState = 
                match pos with
                |   Some rpi -> 
                    let df = stream.Position - rpi.StreamPosition
                    stream.Position <- rpi.StreamPosition
                    runningState.Rollback df
                    |>  RunningState.SetCurrentChar rpi.Character
                |   None   -> runningState

            //let inline backtoupperlevel() = processStr runningState 
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
                //logger.Trace(sprintf "Id: %d, Pos: %d, Char: '%s'" cs.Id stream.Position (Regex.Escape(runningState.CurrentChar.Source)))
                let st = stMap.[cs.Id]
                match st with
                |   SinglePath p ->
                    if (p.State.Match runningState.CurrentChar) then
                        runningState
                        |>  RunningState.AddChar (runningState.CurrentChar.Source.[0])
                        |>  RunningState.Push(p.NextState, None)
                        |>  RunningState.SetStartOfLine (runningState.CurrentChar.Token = Token.NewLine)
                        |>  AdvanceToNextChar
                    else 
                        runningState
                |   StartLinePath st ->
                    if runningState.StartOfLine then 
                        runningState.Push (st.NextState, None) 
                    else 
                        runningState
                |   MultiPath p ->
                    let pos = stream.Position
                    p.States
                    |>  List.rev
                    |>  List.fold(fun (st:RunningState) i -> st.Push(i.StatePointer, Some(RepositionInfo.Create pos runningState.CurrentChar runningState.StartOfLine))) runningState
                |   EmptyPath p -> runningState.Push (p.NextState, None) 
                |   RepeatStart _ 
                |   RepeatInit _ 
                |   RepeatIterOrExit _ ->
                    failwith "These should have been refactored away.."
                |   GroupStart gp -> 
                    runningState
                    |>  RunningState.StartGroup gp.GroupId
                    |>  RunningState.Push (gp.NextState , None)
                |   GroupEnd   gp -> 
                    runningState
                    |>  RunningState.StopGroup gp.GroupId
                    |>  RunningState.Push (gp.NextState , None)
                |   NoMatch _ -> runningState
                |   GoSub gs -> 
                    runningState
                    |>  RunningState.SetGosub gs.GosubId gs.ReturnState
                    |>  RunningState.Push (gs.NextState , None)
                |   ReturnSub rs -> 
                    runningState
                    |>  RunningState.Push (runningState.GosubMapping.[rs.GosubId], None)
                |>  processStr
    processStr (RunningState.Create (stream.Get()) nfa.Start startOfLine) 




let clts (cl:char list) = System.String.Concat(cl)

module ParseResult =
    let IsMatch pr = pr.IsMatch
    let FullMatch pr = pr.FullMatch


