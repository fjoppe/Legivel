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

//  ================================================================================================
//  Experimental - Thompson algorithm for Regex parsing
//  ================================================================================================

type CharacterMatch =
    |   NoMatch         //  decided no match
    |   Match           //  decided match

type ThompsonFunc = TokenData -> CharacterMatch

type SinglePathState = {
    Transiton       :   ThompsonFunc
    NextState       :   RegexState
}
and MultiPathState = {
    Targets         :   RegexState list
    NextState       :   RegexState
}
and RegexState =
    |   Single of  SinglePathState
    |   Multi  of  MultiPathState
    |   Final
    with
        member this.NextState 
            with get() = 
                match this with
                |   Single s -> s.NextState
                |   Multi  s -> s.NextState
                |   Final -> Final

        member this.SetNextState ns =
                match this with
                |   Single s -> Single {s  with NextState = ns}
                |   Multi  s -> Multi  {s  with NextState = ns}
                |   Final -> Final

        member this.IsFinalValue
            with get() = 
                match this with
                |   Final -> true
                |   _     -> false


let rec processState (td:TokenData) (st:RegexState) =
    match st with
    |   Single s -> 
        s.Transiton td
        |>  function
            |   CharacterMatch.Match   -> CharacterMatch.Match,   st.NextState
            |   CharacterMatch.NoMatch -> CharacterMatch.NoMatch, st
    |   Multi m ->
        let rr =
            m.Targets
            |>  List.map(processState td)
            |>  List.filter(fun (rt, rs) -> rt = CharacterMatch.Match)
            |>  List.map(fun (e,s) -> s)
        let isFinal = rr |> List.exists(fun s -> s.IsFinalValue)
        if isFinal then
            CharacterMatch.Match, st.NextState
        else
            if rr.Length = 0 then 
                CharacterMatch.NoMatch, st
            else
                CharacterMatch.Match, Multi { m with Targets = rr }
    |   Final   -> CharacterMatch.Match, Final


let boolToMatch() = 
    function
    |   true ->  CharacterMatch.Match
    |   false -> CharacterMatch.NoMatch

let plainParser (pl:Plain) =
    let fn =
        fun (td:TokenData) ->
            pl.``fixed`` = td.Source 
            |>  boolToMatch()
    Single { Transiton = fn; NextState = Final}

let oneInSetParser (ois:OneInSet) =
    let fn =
        fun (td:TokenData) ->
            if uint32(td.Token) >= 0b0100_0000_0000_0000_0000_0000_0000_0000u then
                ois.Token |> List.exists(fun e -> e=td.Token)
            else
                let checkItWith = ois.TokenQuickCheck.Force()
                (checkItWith &&& uint32(td.Token) > 0u)
            |>  boolToMatch()
    Single { Transiton = fn; NextState = Final}


let oredParser (rl:RegexState list) =
    Multi {
        Targets     = rl
        NextState   = Final
    }


let concatParser (rl:RegexState list) = 
    let rec knit (acc:RegexState) (l:RegexState list) =
        match l with
        |   []     -> acc 
        |   h :: t -> knit (h.SetNextState acc) t
    rl
    |>  knit Final

//let optionalParser() =
//    fun (ts:ThompsonState, td:TokenData) ->
//        let r = callFirst (ts,td)
//        r.State
//        |>  function
//        |   CharacterMatch.MatchContinue
//        |   CharacterMatch.Indecisive   -> r.IncreasCharCount()
//        |   CharacterMatch.Match        -> { r with CharCount = 0 }
//        |   CharacterMatch.MatchReduce  -> { r with CharCount = 1 }
//        |   CharacterMatch.NoMatch      -> r.SetState CharacterMatch.MatchReduce
//        |   _ -> failwith "Illegal state"


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
            |>  List.map(fun i -> getParser i)
            |>  concatParser
        //    ThompsonState.Create concatList, concatParser()
        //|   Optional   t -> 
        //    let orp = getParser t
        //    ThompsonState.Create [orp], optionalParser()
            
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
        let (m,ns) = processState tk rst
        match m with
        |   CharacterMatch.Match   -> 
            if ns.IsFinalValue then
                (CharacterMatch.Match, (tk.Source::matched))
            else
                rgxProcessor streamReader ns (tk.Source::matched)
        |   CharacterMatch.NoMatch -> (CharacterMatch.NoMatch, [])
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
    Assert.AreEqual(Token.EOF, streamReader.Get().Token)

[<Test>]
let ``Parse Plain String with optional end - sunny day``() =
    let (st,fn) = RGP("ABC", [Token.``c-printable``])  + OPT(RGP("E", [Token.``c-printable``])) |> CreatePushParser

    let yaml = "ABCD"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let r = processor streamReader fn st
    Assert.AreEqual(CharacterMatch.Match, r.State)
    Assert.AreEqual("D", streamReader.Get().Source)

[<Test>]
let ``Parse Plain String with optional middle - sunny day``() =
    let (st,fn) = RGP("AB", [Token.``c-printable``])  + OPT(RGP("CDF", [Token.``c-printable``])) + RGP("CDEF", [Token.``c-printable``])  |> CreatePushParser

    let yaml = "ABCDEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let r = processor streamReader fn st
    Assert.AreEqual(CharacterMatch.Match, r.State)
    Assert.AreEqual(Token.EOF, streamReader.Get().Token)


``Parse Plain Character - sunny day``()

``Parse Plain String - sunny day``()

``Parse Plain String Ored - sunny day``()

``Parse RGO Character - sunny day``()


``Parse Plain String with optional end - sunny day``()


``Parse Plain String with optional middle - sunny day``()

