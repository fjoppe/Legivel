module Legivel.Utilities.RegexDSL 

#nowarn "52" // "value has been copied to ensure the original is not mutated"

open System.Diagnostics
open System.Text.RegularExpressions
open System.Text
open System
open System.Globalization

open Legivel.Tokenizer
open System.Collections.Generic
open System.ComponentModel

exception RegexException of string

type Plain =
    private {
        ``fixed`` : string
        mutable optimized : RGXType list
        Token     : Token list
    }
    override this.ToString() = sprintf "%s" this.``fixed``

    member this.OptimizeOnce() = 
        if (this.optimized = []) then
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
                    .Replace("\\\\","\\")

            this.optimized <-
                unescapedString.ToCharArray()
                |>  List.ofArray
                |>  List.map(fun c -> Plain <| Plain.Create (c.ToString()) this.Token)
                |>  List.rev

    static member (+) (r1:Plain, r2:Plain) = 
        let appd = r1.``fixed`` + r2.``fixed``
        {``fixed`` = appd; ``optimized`` = []; Token = r1.Token @ r2.Token}

    static member Create r t = {``fixed`` = r; Token = t; optimized = []}


and OneInSet =
    private {
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


and RGXType =
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
        |   (Plain p1   , Plain p2)      -> Plain(p1 + p2)
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



type ParseResult = {
    Groups  : (TokenData list) list;
    Match   : (TokenData list)
}
with
    static member Empty = {Groups = []; Match = []}
    static member Create m g = {Groups = g; Match = m}


type ParseOutput = bool * ParseResult



//  ================================================================================================
//  Start Experimental - Thompson algorithm for Regex parsing
//  ================================================================================================

type CharacterMatch =
    |   NoMatch         //  decided no match
    |   Match           //  decided match


type SingleStepFunc = TokenData option -> TokenData -> RegexState -> ProcessResult
and [<NoComparison; NoEquality>]
SingleStepState = {
    Info            :   string
    Transiton       :   SingleStepFunc
    NextState       :   RegexState
}
and [<NoComparison; NoEquality>]
SinglePathState = {
    MainState       :   RegexState
    NextState       :   RegexState
}
and [<NoComparison; NoEquality>]
SingleTrackingPathState = {
    Id              :   int
    Track           :   TokenData list
    MainState       :   RegexState
    NextState       :   RegexState
}
and [<NoComparison; NoEquality>]
DoublePathState = {
    CharCount       :   int
    MainState       :   RegexState
    AlternateState  :   (CharacterMatch*RegexState) option
    NextState       :   RegexState
}
and [<NoComparison; NoEquality>]
RepeatDoublePathState = {
    CharCount       :   int
    IterCount       :   int
    Min             :   int
    Max             :   int
    MainState       :   RegexState
    InitialState    :   RegexState
    AlternateState  :   (CharacterMatch*RegexState) option
    NextState       :   RegexState
}
and [<NoComparison; NoEquality>]
IntermediateState = {
        Reduce      : int
        CharCount   : int
    }
    with
        static member Create c r = { Reduce = r; CharCount = c}

and [<NoComparison; NoEquality>]
ParallelPathState = {
    CharCount       :   int
    Targets         :   (IntermediateState * RegexState) list
    NextState       :   RegexState
}
and [<NoComparison; NoEquality>]
RegexState =
    |   SingleRGS   of  SingleStepState
    |   ConcatRGS   of  SinglePathState
    |   OptionalRGS of  DoublePathState
    |   RepeatRGS   of  RepeatDoublePathState
    |   MultiRGS    of  ParallelPathState
    |   GroupRGS    of  SingleTrackingPathState
    |   Final
    with
        member this.NextState 
            with get() = 
                match this with
                |   SingleRGS   s -> s.NextState
                |   ConcatRGS   s -> s.NextState
                |   MultiRGS    s -> s.NextState
                |   OptionalRGS s -> s.NextState
                |   RepeatRGS   s -> s.NextState
                |   GroupRGS    s -> s.NextState
                |   Final -> Final

        member this.SetNextState ns =
                match this with
                |   SingleRGS   s -> SingleRGS {s  with NextState = ns}
                |   ConcatRGS   s -> ConcatRGS {s  with NextState = ns}
                |   MultiRGS    s -> MultiRGS  {s  with NextState = ns}
                |   OptionalRGS s -> OptionalRGS {s  with NextState = ns}
                |   RepeatRGS   s -> RepeatRGS {s  with NextState = ns}
                |   GroupRGS    s -> GroupRGS {s  with NextState = ns}
                |   Final -> Final

        member this.IsFinalValue
            with get() = 
                match this with
                |   Final -> true
                |   _     -> false

and GroupResult = {
        Id      :   int
        Match   :   TokenData list
    }
    with
        static member Create i m = { Id = i; Match = m}
        static member CreateFrom (go:SingleTrackingPathState) = { Id = go.Id; Match = go.Track}
        static member CreateEmpty (id) = { Id = id ; Match = []}

and [<NoComparison; NoEquality>]
ProcessResult = {
    IsMatch     : CharacterMatch
    NextState   : RegexState
    Reduce      : int
    GroupsResults : GroupResult list
}

    with
        static member Create (m, n, r) = {IsMatch = m; NextState = n; Reduce = r; GroupsResults = []}
        static member AddGroup g (o:ProcessResult) = {o with GroupsResults = g :: o.GroupsResults}
        static member AddGroupOf pr (o:ProcessResult) = {o with GroupsResults = o.GroupsResults @  pr.GroupsResults }


[<NoComparison; NoEquality>]
type TPParserState = {
        NextState   :   RegexState
        GroupResults:   GroupResult list
        Groups      :   int
    }
    with
        static member Create n g gc = { NextState = n; GroupResults = g; Groups = gc}


let rec processState (pr:TokenData option) (td:TokenData) (st:RegexState)  =

    let processAlternativeState (cm:CharacterMatch, st:RegexState) = 
        if cm = CharacterMatch.NoMatch ||  st.IsFinalValue then
            ProcessResult.Create (CharacterMatch.NoMatch, Final, 0)
        else
            processState pr td st

    match st with
    |   SingleRGS s -> 
        s.Transiton pr td st
    |   ConcatRGS s -> 
        let repeatThisState ns = ConcatRGS {s with MainState = ns}
        let r = processState pr td s.MainState
        match r.IsMatch with
        |   CharacterMatch.Match when r.NextState.IsFinalValue -> 
            ProcessResult.Create (CharacterMatch.Match, s.NextState, r.Reduce)
            |> ProcessResult.AddGroupOf r
        |   CharacterMatch.Match    -> 
            ProcessResult.Create (CharacterMatch.Match, repeatThisState r.NextState, r.Reduce)
            |> ProcessResult.AddGroupOf r
        |   CharacterMatch.NoMatch  -> 
            ProcessResult.Create (CharacterMatch.NoMatch, Final, 0)
    |   MultiRGS m ->
        let rec multiProcessor first (tg:(IntermediateState * RegexState) list) (ntl:(IntermediateState*ProcessResult) list) =
            match tg with
            |   []  -> 
                let nextStates = ntl |> List.filter(fun (_,s) -> s.Reduce = 0)
                if nextStates.Length = 0 then
                    if ntl.Length=0 then
                        ProcessResult.Create (CharacterMatch.NoMatch, Final,0)
                    else
                        let largestReduce = 
                            ntl
                            |> List.map(fun (_,s) -> s)
                            |> List.maxBy(fun s -> s.Reduce)
                        ProcessResult.Create (CharacterMatch.Match, largestReduce.NextState, largestReduce.Reduce)
                else
                    ProcessResult.Create (CharacterMatch.Match, MultiRGS { m with Targets = ntl |> List.rev |> List.map(fun (i,r) -> i, r.NextState) }, 0)
            |   (hr,hs) :: rest ->
                if hr.Reduce = 0 then 
                    let r = processState pr td hs
                    match r.IsMatch with
                    |   CharacterMatch.Match when r.NextState.IsFinalValue && r.Reduce = 0 ->
                        ProcessResult.Create (CharacterMatch.Match, st.NextState, r.Reduce)
                        |> ProcessResult.AddGroupOf r
                    |   CharacterMatch.Match when r.NextState.IsFinalValue && r.Reduce > 0 && first ->
                        ProcessResult.Create (CharacterMatch.Match, m.NextState, r.Reduce)
                        |> ProcessResult.AddGroupOf r
                    //|   CharacterMatch.Match when r.Reduce > 0 && first ->
                    //    ProcessResult.Create (CharacterMatch.Match, r.NextState, r.Reduce)
                    //    |> ProcessResult.AddGroupOf r
                    |   CharacterMatch.Match   -> multiProcessor false rest ((IntermediateState.Create m.CharCount r.Reduce,r)::ntl)
                    |   CharacterMatch.NoMatch -> multiProcessor first rest ntl
                else
                    if first then
                        ProcessResult.Create (CharacterMatch.Match, m.NextState, m.CharCount - hr.CharCount - hr.Reduce)
                        //|> ProcessResult.AddGroupOf hr.
                    else
                        let prr = ProcessResult.Create (CharacterMatch.Match, Final, hr.Reduce)
                        multiProcessor false rest ((hr,prr)::ntl)
        multiProcessor true m.Targets []    
    |   OptionalRGS o ->
        let ost = { o with AlternateState = if o.AlternateState.IsNone then Some (CharacterMatch.Match, o.NextState) else o.AlternateState}

        let rm = processState pr td ost.MainState
        let ra = processAlternativeState (ost.AlternateState.Value)

        let continueThisState ns rd =
            OptionalRGS { ost with MainState = ns; AlternateState = Some(ra.IsMatch, ra.NextState); CharCount = ost.CharCount + 1 - rd}

        match (rm.IsMatch, ra.IsMatch) with
        |   (CharacterMatch.Match, _)  when rm.NextState.IsFinalValue   ->
            ProcessResult.Create (CharacterMatch.Match, ost.NextState, rm.Reduce)
            |> ProcessResult.AddGroupOf rm
        |   (CharacterMatch.Match, _)   ->
            ProcessResult.Create (CharacterMatch.Match, continueThisState rm.NextState rm.Reduce, rm.Reduce)
            |> ProcessResult.AddGroupOf rm
        |   (CharacterMatch.NoMatch, CharacterMatch.Match)  ->
            ProcessResult.Create (CharacterMatch.Match, ra.NextState, ra.Reduce)
        |   (CharacterMatch.NoMatch, CharacterMatch.NoMatch)    ->
            ProcessResult.Create (CharacterMatch.Match, ost.NextState, ost.CharCount+1)
    |   RepeatRGS zom ->
        let hasMax = zom.Max > zom.Min
        let zost = 
            { zom with
                MainState       = if zom.MainState.IsFinalValue then zom.InitialState else zom.MainState
                AlternateState  = if zom.AlternateState.IsNone then Some (CharacterMatch.Match, zom.NextState) else zom.AlternateState
            }

        let rm = processState pr td zost.MainState
        let ra = processAlternativeState (zost.AlternateState.Value)

        let iterFutureBelowMax zost = (not(hasMax) || zost.IterCount + 1 < zost.Max)
        let iterBelowMax zost = (not(hasMax) || zost.IterCount < zost.Max)
        let iterMinOrAfter zost = zost.IterCount >= zost.Min

        let continueThisState ns rd =
            RepeatRGS { zost with MainState = ns; AlternateState = Some(ra.IsMatch, ra.NextState); CharCount = zost.CharCount + 1 - rd }

        let repeatThisState z = 
            RepeatRGS { z with MainState = z.InitialState; AlternateState = None; CharCount = 0; IterCount = z.IterCount + 1}

        match (rm.IsMatch, ra.IsMatch) with
        |   (CharacterMatch.Match, _)  when rm.NextState.IsFinalValue  -> 
            if iterFutureBelowMax zost && rm.Reduce = 0 then // do repeat
                ProcessResult.Create (CharacterMatch.Match, repeatThisState zost, rm.Reduce)
            else
               ProcessResult.Create (CharacterMatch.Match, zost.NextState, rm.Reduce) 
        |   (CharacterMatch.Match, _) -> 
            ProcessResult.Create (CharacterMatch.Match, continueThisState rm.NextState rm.Reduce, rm.Reduce)
        |   (CharacterMatch.NoMatch, CharacterMatch.Match)      -> 
            if (iterBelowMax zost && iterMinOrAfter zost) then
                ProcessResult.Create (CharacterMatch.Match, ra.NextState, ra.Reduce)
            else
                ProcessResult.Create (CharacterMatch.NoMatch, Final, zost.CharCount+1)
        |   (CharacterMatch.NoMatch, CharacterMatch.NoMatch)    -> 
            if (iterBelowMax zost && iterMinOrAfter zost) then
                ProcessResult.Create (CharacterMatch.Match, zost.NextState, zost.CharCount+1)
            else
                ProcessResult.Create (CharacterMatch.NoMatch, Final, zost.CharCount+1)
    |   GroupRGS s ->
        let repeatThisState ns = GroupRGS { s with MainState = ns; Track = td ::s.Track }

        let r = processState pr td s.MainState
        match r.IsMatch with
        |   CharacterMatch.Match when r.NextState.IsFinalValue -> 
            let t = { s with Track = td :: s.Track |> List.skip(r.Reduce) }
            ProcessResult.Create (CharacterMatch.Match, s.NextState, r.Reduce)
            |>   ProcessResult.AddGroup (GroupResult.CreateFrom t)
        |   CharacterMatch.Match    -> 
            ProcessResult.Create (CharacterMatch.Match, repeatThisState r.NextState, 0)
        |   CharacterMatch.NoMatch  -> 
            ProcessResult.Create (CharacterMatch.NoMatch, Final, 0)
            |>  ProcessResult.AddGroup (GroupResult.CreateEmpty s.Id)

    |   Final   -> ProcessResult.Create (CharacterMatch.Match, Final, 0)


let plainParser (pl:Plain) =
    let fn =
        fun (pr:TokenData option) (td:TokenData) (st:RegexState)  ->
            match  pl.``fixed`` with
            |   "^" when pl.Token.Head = Token.NoToken ->
                if (pr = None || pr.Value.Token = Token.NewLine) then
                    ProcessResult.Create (CharacterMatch.Match,   st.NextState, 1)
                else
                    ProcessResult.Create (CharacterMatch.NoMatch, Final, 0)
            |   _ when pl.``fixed`` = td.Source ->
                ProcessResult.Create (CharacterMatch.Match,   st.NextState, 0)
            |   _ ->
                ProcessResult.Create (CharacterMatch.NoMatch, Final, 0)

    SingleRGS { Transiton = fn; NextState = Final; Info = pl.``fixed``}

let oneInSetParser (ois:OneInSet) =
    let fn =
        fun (pr:TokenData option) (td:TokenData) (st:RegexState) ->
            if uint32(td.Token) >= 0b0100_0000_0000_0000_0000_0000_0000_0000u then
                ois.Token |> List.exists(fun e -> e=td.Token)
            else
                let checkItWith = ois.TokenQuickCheck.Force()
                (checkItWith &&& uint32(td.Token) > 0u)
            |>  function
                |   true   -> ProcessResult.Create (CharacterMatch.Match,   st.NextState, 0)
                |   false -> ProcessResult.Create (CharacterMatch.NoMatch, Final, 0)
    SingleRGS { Transiton = fn; NextState = Final; Info = sprintf "(%s)-(%s)" ois.mainset ois.subtractset}


let oredParser (rl:RegexState list) =
    MultiRGS {
        Targets     = rl |> List.map(fun e -> IntermediateState.Create 0 0, e)
        NextState   = Final
        CharCount   = 0
    }


let concatParser (rl:RegexState list) = 
    let rec knit (acc:RegexState) (l:RegexState list) =
        match l with
        |   []     -> acc 
        |   h :: t -> knit (h.SetNextState acc) t
    let ccc =
        rl
        |>  List.rev
        |>  knit Final
    ConcatRGS { MainState =ccc; NextState = Final}

let optionalParser (ro:RegexState) =
    OptionalRGS {MainState = ro; AlternateState = None; NextState = Final; CharCount = 0}

let zeroOrMoreParser (ro:RegexState) = 
    RepeatRGS 
        {
            Min = 0
            Max = -1
            MainState = ro
            InitialState = ro
            AlternateState = None
            NextState = Final
            CharCount = 0
            IterCount = 0
        }

let oneOrMoreParser (ro:RegexState) = 
    RepeatRGS 
        {
            Min = 1
            Max = -1
            MainState = ro
            InitialState = ro
            AlternateState = None
            NextState = Final
            CharCount = 0
            IterCount = 0
        }

let RangeParser (ro:RegexState) mn mx = 
    RepeatRGS 
        {
            Min = mn
            Max = mx
            MainState = ro
            InitialState = ro
            AlternateState = None
            NextState = Final
            CharCount = 0
            IterCount = 0
        }

let GroupParser (gc:int) (ro:RegexState) =
    GroupRGS {Id = gc; Track  = []; MainState=ro; NextState = Final }


let CreatePushParser (rgx:RGXType) =
    let rec getParser groupCounter rgx : (int * RegexState) =
        match rgx with
        |   Plain    pl -> 
            if pl.``fixed``.Length > 1 then
                pl.OptimizeOnce()
                getParser groupCounter (Concat pl.optimized)
            else
                groupCounter, plainParser pl
        |   OneInSet ois -> (groupCounter, oneInSetParser ois)
        |   Or       l ->
            let gc,orList = l |> List.fold(fun (gc,acc) i -> getParser gc i ||> fun g v -> g, (v::acc)) (groupCounter,[])
            gc, oredParser orList
        |   Concat   l ->
            let gc,ccList = l |> List.fold(fun (gc,acc) i -> getParser gc i ||> fun g v -> g, (v::acc)) (groupCounter,[])
            gc, concatParser ccList
        |   Optional   t -> 
            let gc,orp = getParser groupCounter t
            gc, optionalParser orp
        |   ZeroOrMoreNonGreedy t
        |   ZeroOrMore t -> 
            let gc, rp = getParser groupCounter t
            gc, zeroOrMoreParser rp
        |   OneOrMoreNonGreedy  t 
        |   OneOrMore  t -> 
            let gc, rp = getParser groupCounter t
            gc, oneOrMoreParser rp
        |   IterRange(t,mx,mno) ->
            let gc, rp = getParser groupCounter t
            match mno with
            |   Some(mn) ->  gc, RangeParser rp mn mx
            |   None     ->  gc, RangeParser rp mx mx
        |   Group      t -> 
            let gcn = groupCounter + 1
            let gc,rp = getParser gcn t
            gc, GroupParser groupCounter rp

    getParser 0 rgx 
    |> fun (gc,rgs) -> TPParserState.Create rgs [] gc


let EndOfStream = TokenData.Create (Token.EOF) ""

type MatchResultTP = {
        IsMatch     : bool
        FullMatch   : TokenData list
        GroupsResults : GroupResult list
    }

let MatchRegexState (streamReader:RollingStream<TokenData>) (rgst:TPParserState) =
    let startPos = streamReader.Position
    let rec rgxProcessor (streamReader:RollingStream<TokenData>) (rgst:TPParserState) (matched) =
        let pr = streamReader.PeekPrevious()
        let tk = streamReader.Get()
        let rt = processState pr tk rgst.NextState
        match rt.IsMatch with
        |   CharacterMatch.Match   -> 
            streamReader.Position <- streamReader.Position - rt.Reduce
            let reduceMatch = (tk::matched) |> List.skip rt.Reduce
            if rt.NextState.IsFinalValue then
                { IsMatch = true; FullMatch = reduceMatch; GroupsResults = rt.GroupsResults @ rgst.GroupResults}
            else
                let nr = { rgst with GroupResults = rt.GroupsResults @ rgst.GroupResults; NextState = rt.NextState }
                rgxProcessor streamReader nr reduceMatch 
        |   CharacterMatch.NoMatch when rt.Reduce = 0 -> 
            streamReader.Position <- startPos
            { IsMatch = false; FullMatch = []; GroupsResults = []}
        |   CharacterMatch.NoMatch  -> 
            streamReader.Position <- streamReader.Position - rt.Reduce
            let reduceMatch = matched |> List.skip rt.Reduce
            rgxProcessor streamReader {rgst with NextState = rt.NextState} reduceMatch

    let r = rgxProcessor streamReader rgst []
    let fullListOfGroups =
        let allGroupIds = [0..(rgst.Groups - 1)] |> Set.ofList
        let resultGroupIds = r.GroupsResults |> List.map(fun m -> m.Id) |> Set.ofList
        resultGroupIds 
        |>  Set.difference allGroupIds
        |>  Set.toList
        |>  List.map(fun i -> GroupResult.Create i [])
        |>  List.append r.GroupsResults
        |>  List.map(fun i -> { i with Match = i.Match |> List.rev })
    { r with FullMatch = r.FullMatch |> List.rev; GroupsResults = fullListOfGroups}


let memoizeThompsonParser = new Dictionary<RGXType, TPParserState>()

let AssesInputThompsonParser (rs:RollingStream<TokenData>) (rg:RGXType) = 
    let ts = 
        if memoizeThompsonParser.ContainsKey(rg) then
            memoizeThompsonParser.[rg]
        else
            let tsc = CreatePushParser rg
            //memoizeThompsonParser.Add(rg, tsc)
            tsc
    let r = MatchRegexState rs ts
    (r.IsMatch, ParseResult.Create (r.FullMatch) (r.GroupsResults |> List.map(fun i -> i.Match)))


//  ================================================================================================
//  End Experimental - Thompson algorithm for Regex parsing
//  ================================================================================================










let AssesInputPostParseCondition (condition: RollingStream<TokenData> * TokenData -> bool) (rs:RollingStream<TokenData>) (rg:RGXType) =
    let rec parse rgx tkl gl : ParseOutput =
        let conditionalParse rgx tk gl : ParseOutput =
            let p = rs.Position
            let r = parse rgx tk gl
            if not(fst(r)) then rs.Position <- p
            r

        let mkResult t tkl gl =
            function 
            | true ->  (true,  ParseResult.Create (t :: tkl) gl) 
            | false -> (false, ParseResult.Create tkl gl)

        let rec repeatWhileMatching t acc gl =
            let pr = conditionalParse t tkl gl
            let added = (snd pr).Match
            if (fst pr) && added <> [] then 
                let nwa = (added @ acc)
                repeatWhileMatching t nwa gl
            else ParseResult.Create acc gl

        let checkParseCondition() =
            let noToken = TokenData.Create Token.NoToken ""
            rs.PeekPrevious()
            |>  function
                |   Some x  -> condition (rs, x)
                |   None    -> condition (rs, noToken)

        if rs.EOF then 
            (false, ParseResult.Empty)
        else
            match rgx with
            |   OneInSet ois    -> 
                if checkParseCondition() then
                    rs.Get() 
                    |> fun i -> 
                        if uint32(i.Token) >= 0b0100_0000_0000_0000_0000_0000_0000_0000u then
                            ois.Token |> List.exists(fun e -> e=i.Token)
                        else
                            let checkItWith = ois.TokenQuickCheck.Force()
                            if (checkItWith &&& uint32(i.Token) > 0u) then
                                //let p = ois.OneInSet.Force()
                                //let r = p.Contains(i.Source)    // not ready yet to check one in set char-by-char
                                true
                            else
                                false
                        |> mkResult i tkl gl
                else
                    (false, ParseResult.Empty)
            |   Plain pl        -> 
                if checkParseCondition() then
                    match (pl.``fixed``, pl.Token) with
                    |   ("^",[Token.NoToken]) ->
                        let pk = rs.PeekPrevious()
                        (pk = None || pk.Value.Token = Token.NewLine), ParseResult.Empty
                    |   (_, [Token.EOF]) ->
                        let isEof = rs.Peek().Token = Token.EOF
                        (isEof, ParseResult.Empty)
                    |   (_, []) -> true, ParseResult.Empty
                    | _ ->
                        if pl.``fixed``.Length > 1 then
                            pl.OptimizeOnce()
                            parse (Concat pl.optimized) tkl gl
                        else
                            rs.Get() |> fun i -> pl.``fixed`` = i.Source |> mkResult i tkl gl
                else
                    (false, ParseResult.Empty)
            |   Or rl           -> 
                let rec pickFirst l =
                    match l with
                    |   h::tl -> 
                        let rs = conditionalParse h tkl gl
                        if fst(rs) then (true, ParseResult.Create ((snd rs).Match @ tkl) gl)
                        else pickFirst tl
                    |   [] -> (false, ParseResult.Empty)
                rl |> List.rev |> pickFirst
            |   Concat rl       -> 
                let rec pickAll acc gi l  =
                    match l with
                    |   h::tl -> 
                        let suc, rs = parse h tkl gi
                        if suc then pickAll (rs.Match @ acc) (rs.Groups) tl 
                        else (false, ParseResult.Create tkl gi)
                    |   [] -> (true, ParseResult.Create acc gi)
                rl |> List.rev |> pickAll [] gl
            |   IterRange (irx,mxo,mno) -> 
                let dec a = if a>=0 then (a-1) else a
                let rec repeatRange min max rx acc =
                    if max>0 then
                        let pr = conditionalParse rx tkl gl
                        let nwacc = ((snd pr).Match @ acc)
                        if (fst pr) && nwacc.Length > acc.Length && max>0 then repeatRange (dec min) (dec max) rx nwacc
                        else (min<=0), ParseResult.Create acc gl
                    else
                        true, ParseResult.Create acc gl
                match mno with
                |   Some minVal -> repeatRange minVal mxo irx []
                |   None        -> repeatRange mxo mxo irx []
            |   ZeroOrMore t         -> true, repeatWhileMatching t tkl gl 
            |   ZeroOrMoreNonGreedy t -> true, repeatWhileMatching t tkl gl
            |   OneOrMore t          ->  repeatWhileMatching t tkl gl |> fun l -> (l.Match.Length>=1), l
            |   OneOrMoreNonGreedy t -> repeatWhileMatching t tkl gl |> fun l -> (l.Match.Length>=1), l
            |   Optional t           -> true, conditionalParse t tkl gl |> snd
            |   Group t              -> 
                let s, res = parse t tkl gl
                if s then
                    s, { res with Groups = (res.Match |> List.rev)  :: res.Groups }
                else
                    s, res

    parse rg [] []
    |> fun (b,pr) -> b, { pr with Match = pr.Match |> List.rev}

let AssesInput (rs:RollingStream<TokenData>) (rg:RGXType) =
    //try
        AssesInputThompsonParser rs rg
        //AssesInputPostParseCondition (fun _ -> true) rs rg
    //with
    //|  e -> raise e


let TokenDataToString =
    function
    |   (true, tkl:ParseResult) -> (tkl.Match |> List.map(fun td -> td.Source) |> List.fold(fun (str:StringBuilder) i -> str.Append(i)) (StringBuilder())).ToString() |> Some
    |   (false, _) -> None


type MatchResult = {
        FullMatch   : string
        Groups      : string list
    }
    with
        static member Create f g = { FullMatch = f; Groups = g }
        member this.ge1 with get() = (this.Groups.[1])
        member this.ge2 with get() = (this.Groups.[1], this.Groups.[2])
        member this.ge3 with get() = (this.Groups.[1], this.Groups.[2], this.Groups.[3])
        member this.ge4 with get() = (this.Groups.[1], this.Groups.[2], this.Groups.[3], this.Groups.[4])


/// Returns list of match groups, for pattern p on string s
[<DebuggerStepThrough>]
let Match(s, p) = 
    let mt = Regex.Matches(s, RGS(p), RegexOptions.Multiline)
    if mt.Count = 0 then 
        []
    else
        [ for g in mt -> g.Value ]

/// Returns whether pattern p matches on string s
[<DebuggerStepThrough>]
let IsMatch(s:RollingStream<TokenData>, p) = 
    let pos = s.Position
    AssesInput s p |> fst
    |> fun res -> 
        s.Position <- pos
        res

[<DebuggerStepThrough>]
let IsMatchStr(s, p) = 
    let ml = Match(s, p)
    ml.Length > 0

/// Checks for matches of pattern p in string s.
/// If matched, returns (true, <match-string>, <rest-string>), otherwise (false, "",s)
[<DebuggerStepThrough>]
let HasMatches(s,p) = 
    let s, str =  AssesInput s p
    if not s then
        (false, String.Empty)
    else
        str
        |>  fun i -> i.Match
        |> List.map(fun td -> td.Source)
        |> List.fold(fun (str:StringBuilder) i -> str.Append(i)) (StringBuilder())
        |> fun sb -> s, sb.ToString()

[<DebuggerStepThrough>]
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.Multiline)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

[<DebuggerStepThrough>]
let (|Regex2|_|) (pattern:RGXType) (input:RollingStream<TokenData>) =
    let p = input.Position
    AssesInput input pattern 
    |> TokenDataToString
    |>  Option.bind(fun mts ->
        let m = Regex.Match(mts, RGS(pattern), RegexOptions.Multiline)
        if m.Success then 
            let lst = [ for g in m.Groups -> g.Value ]
            let fullMatch = lst |> List.head
            let groups = lst |> List.tail
            Some(MatchResult.Create fullMatch groups)
        else 
            failwith "Difference between assesinput and regex"
    )
    |>  function
        |   None -> input.Position <- p;None
        |   Some x -> Some x

let DecodeEncodedUnicodeCharacters value =
    Regex.Replace(value,
        @"(\\u(?<Value>[a-zA-Z0-9]{4}))|(\\U(?<Value>[a-zA-Z0-9]{8}))",
        (fun (m:Match) -> (char(Int64.Parse(m.Groups.["Value"].Value, NumberStyles.HexNumber))).ToString()))

let DecodeEncodedHexCharacters value =
    Regex.Replace(value,
        @"\\x(?<Value>[a-fA-F0-9]{2})",
        (fun (m:Match) -> (char(Int32.Parse(m.Groups.["Value"].Value, NumberStyles.HexNumber))).ToString()))

let DecodeEncodedUriHexCharacters value =
    Regex.Replace(value,
        @"%(?<Value>[a-fA-F0-9]{2})",
        (fun (m:Match) -> (char(Int32.Parse(m.Groups.["Value"].Value, NumberStyles.HexNumber))).ToString()))
    
let DecodeEncodedEscapedCharacters value =
    Regex.Replace(value,
        @"\\(?<Value>[0abtnvfre ""/N_LP])",
        (fun (m:Match) -> 
            match (m.Groups.["Value"].Value) with
            |   "0" -> "\x00"
            |   "a" -> "\a"
            |   "b" -> "\b"
            |   "t" -> "\t"
            |   "n" -> "\n"
            |   "v" -> "\v"
            |   "f" -> "\f"
            |   "r" -> "\r"
            |   "e" -> "\x1b"
            |   " " -> " "
            |   "\"" -> "\""
            |   "/" -> "\x2f"
            |   "N" -> "\u0085"
            |   "_" -> "\u00a0"
            |   "L" -> "\u2028"
            |   "P" -> "\u2029"
            | _ -> sprintf "\\%s" m.Groups.["Value"].Value
        ))



