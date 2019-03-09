#I __SOURCE_DIRECTORY__ 

#time

//#r @"bin/Debug/net45/FSharp.Core.dll"
//#r @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
#r @"bin/Debug/net45/Legivel.Parser.dll"
#r @"bin/Debug/net45/NLog.dll"
#r @"bin/Debug/net45/nunit.framework.dll"

//#r @"bin/Release/net45/FSharp.Core.dll"
//#r @"bin/Release/net45/Legivel.Parser.dll"
//#r @"bin/Release/net45/NLog.dll"

open System
open System.Text.RegularExpressions
open System.Globalization
open System.Text
open Legivel.Parser
open Legivel.TagResolution
open Legivel.Serialization
open Legivel.RepresentationGraph
open Legivel.Common
open NLog
open System.IO
open Legivel.Tokenizer
open NUnit.Framework


#load "nlog.fsx"
#load "RegexDSL-Base.fsx"


open System
open System.Globalization
open Legivel.Tokenizer
open System.Threading
open System
open NLog.Config
open System.ComponentModel
open ``RegexDSL-Base``
open System.Drawing

//  ================================================================================================
//  Experimental - Thompson algorithm for Regex parsing
//  ================================================================================================

type CharacterMatch =
    |   NoMatch         //  decided no match
    |   Match           //  decided match

type ThompsonFunc = TokenData -> CharacterMatch

type SingleStepState = {
    Info            :   string
    Transiton       :   ThompsonFunc
    NextState       :   RegexState
}
and SinglePathState = {
    MainState       :   RegexState
    NextState       :   RegexState
}
and DoublePathState = {
    CharCount       :   int
    MainState       :   RegexState
    AlternateState  :   (CharacterMatch*RegexState) option
    NextState       :   RegexState
}
and ParallelPathState = {
    Targets         :   RegexState list
    NextState       :   RegexState
}
and RegexState =
    |   SingleRGS   of  SingleStepState
    |   ConcatRGS   of  SinglePathState
    |   OptionalRGS of  DoublePathState
    |   MultiRGS    of  ParallelPathState
    |   Final
    with
        member this.NextState 
            with get() = 
                match this with
                |   SingleRGS s -> s.NextState
                |   ConcatRGS s -> s.NextState
                |   MultiRGS  s -> s.NextState
                |   OptionalRGS s -> s.NextState
                |   Final -> Final

        member this.SetNextState ns =
                match this with
                |   SingleRGS s -> SingleRGS {s  with NextState = ns}
                |   ConcatRGS s -> ConcatRGS {s  with NextState = ns}
                |   MultiRGS  s -> MultiRGS  {s  with NextState = ns}
                |   OptionalRGS s -> OptionalRGS {s  with NextState = ns}
                |   Final -> Final

        member this.IsFinalValue
            with get() = 
                match this with
                |   Final -> true
                |   _     -> false


type ProcessResult = {
    IsMatch     : CharacterMatch
    NextState   : RegexState
    Reduce      : int
}
    with
        static member Create (m, n, r) = {IsMatch = m; NextState = n; Reduce = r}

let rec processState (td:TokenData) (st:RegexState) =
    match st with
    |   SingleRGS s -> 
        s.Transiton td
        |>  function
            |   CharacterMatch.Match   -> ProcessResult.Create (CharacterMatch.Match,   st.NextState, 0)
            |   CharacterMatch.NoMatch -> ProcessResult.Create (CharacterMatch.NoMatch, Final, 0)
    |   ConcatRGS s -> 
        let repeatThisState ns =
            ConcatRGS {
                s with
                    MainState = ns
            }
        let r = processState td s.MainState
        match r.IsMatch with
        |   CharacterMatch.Match when r.NextState.IsFinalValue
                                    -> ProcessResult.Create (CharacterMatch.Match, s.NextState, r.Reduce)
        |   CharacterMatch.Match    -> ProcessResult.Create (CharacterMatch.Match, repeatThisState r.NextState, 0)
        |   CharacterMatch.NoMatch  -> ProcessResult.Create (CharacterMatch.NoMatch, Final, 0)
    |   MultiRGS m ->
        let rr =
            m.Targets
            |>  List.map(processState td)
            |>  List.filter(fun rt -> rt.IsMatch = CharacterMatch.Match)
            |>  List.map(fun rt -> rt.NextState)
        let isFinal = rr |> List.exists(fun s -> s.IsFinalValue)
        if isFinal then
            ProcessResult.Create (CharacterMatch.Match, st.NextState, 0)
        else
            if rr.Length = 0 then 
                ProcessResult.Create (CharacterMatch.NoMatch, Final,0)
            else
                ProcessResult.Create (CharacterMatch.Match, MultiRGS { m with Targets = rr }, 0)
    |   OptionalRGS o ->
        if o.NextState.IsFinalValue then    // optional at the end
            let r = processState td o.MainState
            match (r.IsMatch, r.NextState) with
            |   CharacterMatch.Match,   Final -> ProcessResult.Create (CharacterMatch.Match, Final, r.Reduce)
            |   CharacterMatch.Match,   _     -> 
                let nxt = OptionalRGS { o with MainState = r.NextState; CharCount = o.CharCount + 1}
                ProcessResult.Create (CharacterMatch.Match, nxt, 0)
            |   CharacterMatch.NoMatch, _     -> ProcessResult.Create (CharacterMatch.Match,   Final, o.CharCount+1)
        else
            let ost = if o.AlternateState.IsNone then {o with AlternateState = Some (CharacterMatch.Match, o.NextState)} else o
            let rm = processState td ost.MainState

            let am, ``as`` = ost.AlternateState.Value
            let ra = 
                if am = CharacterMatch.NoMatch ||  ``as``.IsFinalValue then
                    ProcessResult.Create (am, ``as``, 0)
                else
                    processState td ``as`` 

            let repeatThisState ns =
                OptionalRGS {
                    ost with
                        MainState = ns
                        AlternateState = Some(ra.IsMatch, ra.NextState)
                        CharCount = ost.CharCount + 1
                }

            match (rm.IsMatch, ra.IsMatch) with
            |   (CharacterMatch.Match, _)  when rm.NextState.IsFinalValue  
                                                                    -> ProcessResult.Create (CharacterMatch.Match, ost.NextState, rm.Reduce)
            |   (CharacterMatch.Match, CharacterMatch.NoMatch)      -> ProcessResult.Create (CharacterMatch.Match, repeatThisState rm.NextState, 0)
            |   (CharacterMatch.Match, CharacterMatch.Match)        -> ProcessResult.Create (CharacterMatch.Match, repeatThisState rm.NextState, 0)
            |   (CharacterMatch.NoMatch, CharacterMatch.Match)      -> ProcessResult.Create (CharacterMatch.Match, ra.NextState, 0)
            |   (CharacterMatch.NoMatch, CharacterMatch.NoMatch)    -> ProcessResult.Create (CharacterMatch.Match, ost.NextState, ost.CharCount+1)

    |   Final   -> ProcessResult.Create (CharacterMatch.Match, Final, 0)


let boolToMatch() = 
    function
    |   true ->  CharacterMatch.Match
    |   false -> CharacterMatch.NoMatch

let plainParser (pl:Plain) =
    let fn =
        fun (td:TokenData) ->
            pl.``fixed`` = td.Source 
            |>  boolToMatch()
    SingleRGS { Transiton = fn; NextState = Final; Info = pl.``fixed``}

let oneInSetParser (ois:OneInSet) =
    let fn =
        fun (td:TokenData) ->
            if uint32(td.Token) >= 0b0100_0000_0000_0000_0000_0000_0000_0000u then
                ois.Token |> List.exists(fun e -> e=td.Token)
            else
                let checkItWith = ois.TokenQuickCheck.Force()
                (checkItWith &&& uint32(td.Token) > 0u)
            |>  boolToMatch()
    SingleRGS { Transiton = fn; NextState = Final; Info = sprintf "(%s)-(%s)" ois.mainset ois.subtractset}


let oredParser (rl:RegexState list) =
    MultiRGS {
        Targets     = rl
        NextState   = Final
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


//let itemRangeParser minRep maxRep (ts:ThompsonState, td:TokenData) =
//    let quitOnkUpperBound (r:ThompsonState) = 
//        if r.IterationCount = (maxRep-1) then r.SetState CharacterMatch.Match else r.SetState CharacterMatch.Indecisive
//    let r = (ts,td) |> (ts.FunctionState |> List.head |> snd)
//    match r.State with
//    |   CharacterMatch.Match        -> quitOnkUpperBound r |> iterate |> reinit
//    |   CharacterMatch.MatchContinue
//    |   CharacterMatch.Indecisive   -> r
//    |   CharacterMatch.MatchReduce  -> quitOnkUpperBound r |> iterate 
//    |   CharacterMatch.NoMatch      ->
//        if r.IterationCount < minRep || r.IterationCount >= maxRep then
//            r
//        else
//            r.SetState (CharacterMatch.MatchReduce)
//    |   _ -> failwith "Illegal state"


let CreatePushParser (rgx:RGXType) =
    let rec getParser rgx : (RegexState) =
        match rgx with
        |   Plain    pl -> 
            if pl.``fixed``.Length > 1 then
                pl.OptimizeOnce()
                getParser (Concat pl.optimized) 
            else
                plainParser pl
        |   OneInSet ois -> oneInSetParser ois
        |   Or       l ->
            let orList = l |> List.map(fun i -> getParser i)
            oredParser orList 
        |   Concat   l ->
            l 
            |>  List.rev
            |>  List.map(fun i -> getParser i)
            |>  concatParser
        //    ThompsonState.Create concatList, concatParser()
        |   Optional   t -> 
            let orp = getParser t
            optionalParser orp
            
        //|   ZeroOrMore t -> sprintf "(?:%O)*" t
        //|   ZeroOrMoreNonGreedy t -> sprintf "(?:%O)*?" t
        //|   OneOrMore  t -> sprintf "(?:%O)+" t
        //|   OneOrMoreNonGreedy  t -> sprintf "(?:%O)+?" t
        //|   Group      t -> sprintf "(%O)" t
        //|   IterRange(t,mx,mno) ->
        //    let (funcStartState, funcToRepeat) = getParser t
        //    //let range minRep maxRep (ts:ThompsonState, td:TokenData) =
        //    //    let r = funcToRepeat(ts,td)
        //    //    match r.State with
        //    //    |   CharacterMatch.Indecisive   -> r
        //    //    |   CharacterMatch.NoMatch      ->
        //    //        if r.CharCount < minRep || r.CharCount > maxRep then
        //    //            r
        //    //        else
        //    //            r.SetState (CharacterMatch.MatchReduce)
        //    //    |   CharacterMatch.Match        -> r.SetState (r.State) []
        //    //    |   CharacterMatch.MatchContinue-> r.SetState CharacterMatch.Indecisive
        //    //    |   CharacterMatch.MatchReduce  -> concat r tail

        //    ThompsonState.Create(),
        //    match mno with
        //    |   Some(mn) ->  itemRangeParser funcToRepeat mn mx
        //    |   None     ->  itemRangeParser funcToRepeat mx mx

    getParser rgx

let EndOfStream = TokenData.Create (Token.EOF) ""


let processor (streamReader:RollingStream<TokenData>) (rst:RegexState) =
    let rec rgxProcessor (streamReader:RollingStream<TokenData>) (rst:RegexState) (matched) =
        let tk = streamReader.Get()
        let rt = processState tk rst
        match rt.IsMatch with
        |   CharacterMatch.Match   -> 
            streamReader.Position <- streamReader.Position - rt.Reduce
            let reduceMatch = (tk.Source::matched) |> List.skip rt.Reduce
            if rt.NextState.IsFinalValue then
                (CharacterMatch.Match, reduceMatch)
            else
                rgxProcessor streamReader rt.NextState (tk.Source::matched)
        |   CharacterMatch.NoMatch when rt.Reduce = 0 -> (CharacterMatch.NoMatch, [])
        |   CharacterMatch.NoMatch  -> 
            streamReader.Position <- streamReader.Position - rt.Reduce
            let reduceMatch = matched |> List.skip rt.Reduce
            rgxProcessor streamReader rt.NextState reduceMatch

    rgxProcessor streamReader rst []


[<Test>]
let ``Parse Plain Character - sunny day``() =
    let rgxst = RGP("A", [Token.``c-printable``]) |> CreatePushParser
    let yaml = "A"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    
    let (r,m) = processor streamReader rgxst
    Assert.AreEqual(CharacterMatch.Match, r)
    Assert.AreEqual(Token.EOF, streamReader.Get().Token)



[<Test>]
let ``Parse Plain String - sunny day``() =
    let rgxst = RGP("ABC", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABC"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let (r,m) = processor streamReader rgxst
    Assert.AreEqual(CharacterMatch.Match, r)
    Assert.AreEqual(Token.EOF, streamReader.Get().Token)

    let yaml = "ABD"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let (r,m) = processor streamReader rgxst
    Assert.AreEqual(CharacterMatch.NoMatch, r)
    Assert.AreEqual(Token.EOF, streamReader.Get().Token)


[<Test>]
let ``Parse Plain String concats - sunny day``() =
    let rgxst = RGP("AB", [Token.``c-printable``]) + RGP("CD", [Token.``c-printable``]) + RGP("EF", [Token.``c-printable``])|> CreatePushParser

    let yaml = "ABCDEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let (r,m) = processor streamReader rgxst
    Assert.AreEqual(CharacterMatch.Match, r)
    Assert.AreEqual(Token.EOF, streamReader.Get().Token)


[<Test>]
let ``Parse Plain String Ored - sunny day``() =
    let rgxst = RGP("ABCE", [Token.``c-printable``]) ||| RGP("ABD", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABDE"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let (r,m) = processor streamReader rgxst
    Assert.AreEqual(CharacterMatch.Match, r)
    Assert.AreEqual("E", streamReader.Get().Source)

    let yaml = "ABCE"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let (r,m) = processor streamReader rgxst
    Assert.AreEqual(CharacterMatch.Match, r)
    Assert.AreEqual(Token.EOF, streamReader.Get().Token)

[<Test>]
let ``Parse RGO Character - sunny day``() =
    let rgxst = RGO("A-", [Token.``c-printable``;Token.``t-hyphen``]) |> CreatePushParser

    let yaml = "-"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let (r,m) = processor streamReader rgxst
    Assert.AreEqual(CharacterMatch.Match, r)
    Assert.AreEqual(Token.EOF, streamReader.Get().Token)

    let yaml = "A"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let (r,m) = processor streamReader rgxst
    Assert.AreEqual(CharacterMatch.Match, r)
    Assert.AreEqual(["A"], m)
    Assert.AreEqual(Token.EOF, streamReader.Get().Token)

[<Test>]
let ``Parse Plain String with optional end - sunny day``() =
    let rgxst = RGP("ABC", [Token.``c-printable``])  + OPT(RGP("E", [Token.``c-printable``])) |> CreatePushParser

    let yaml = "ABCD"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let (r,m) = processor streamReader rgxst
    Assert.AreEqual(CharacterMatch.Match, r)
    Assert.AreEqual(["A"; "B"; "C"] |> List.rev, m)
    Assert.AreEqual("D", streamReader.Get().Source)

    let yaml = "ABCE"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let (r,m) = processor streamReader rgxst
    Assert.AreEqual(CharacterMatch.Match, r)
    Assert.AreEqual(["A"; "B"; "C"; "E"] |> List.rev, m)
    Assert.AreEqual(Token.EOF, streamReader.Get().Token)


[<Test>]
let ``Parse Plain String with optional middle - sunny day``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + OPT(RGP("CDF", [Token.``c-printable``])) + RGP("CDEF", [Token.``c-printable``])  |> CreatePushParser

    let yaml = "ABCDEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let (r,m) = processor streamReader rgxst
    Assert.AreEqual(CharacterMatch.Match, r)
    Assert.AreEqual(["A"; "B"; "C"; "D"; "E"; "F"] |> List.rev, m)
    Assert.AreEqual(Token.EOF, streamReader.Get().Token)


``Parse Plain Character - sunny day``()

``Parse Plain String - sunny day``()

``Parse Plain String Ored - sunny day``()

``Parse RGO Character - sunny day``()

``Parse Plain String concats - sunny day``()

``Parse Plain String with optional end - sunny day``()

``Parse Plain String with optional middle - sunny day``()

