module Legivel.Utilities.RegexDSL 

#nowarn "52" // "value has been copied to ensure the original is not mutated"

open System.Diagnostics
open System.Text.RegularExpressions
open System.Text
open System
open System.Globalization

open Legivel.Tokenizer

exception RegexException of string

type Plain =
    private {
        ``fixed`` : string
        Token     : Token list
    }
    override this.ToString() = sprintf "%s" this.``fixed``

    static member (+) (r1:Plain, r2:Plain) = {``fixed`` = r1.``fixed`` + r2.``fixed``; Token = r1.Token @ r2.Token}

    static member Create r t = {``fixed`` = r; Token = t}


type OneInSet =
    private {
        not      : bool
        mainset  : string
        subtractset : string
        Token     : Token list
    }
    static member subtractable = [
            Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark`` 
            Token.``t-colon`` ; Token.``t-comma``; Token.``t-dot`` ; Token.``t-square-bracket-start`` ; Token.``t-square-bracket-end`` ; Token.``t-curly-bracket-start``
            Token.``t-curly-bracket-end`` ; Token.``t-hash`` ; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``
            Token.``t-gt``; Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``;Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``
            Token.``c-escape``; 
            ]

    override this.ToString() =
        let subtract = this.subtractset <> ""
        match (subtract, this.not) with 
        //  https://msdn.microsoft.com/en-us/library/20bw873z(v=vs.110).aspx#Anchor_13
        |   (true, true)    -> sprintf "[%s-[^%s]]" (this.subtractset) (this.mainset)
        |   (true, false)   -> sprintf "[%s-[%s]]"  (this.mainset) (this.subtractset)
        |   (false, true)   -> sprintf "[^%s]" (this.mainset)
        |   (false, false)  -> sprintf "[%s]" (this.mainset) 
    static member (-) (r1:OneInSet, r2:OneInSet) =
        let subtr = r2.Token |> List.filter(fun e -> OneInSet.subtractable |> List.exists(fun s -> e=s))
        {mainset = r1.mainset; subtractset = r1.subtractset + r2.mainset; not = r1.not; Token = r1.Token |> List.filter(fun tf -> subtr |> List.exists(fun te -> te = tf) |> not)}
    static member (-) (r1:OneInSet, r2:Plain) =
        let subtr = r2.Token |> List.filter(fun e -> OneInSet.subtractable |> List.exists(fun s -> e=s))
        {mainset = r1.mainset; subtractset = r1.subtractset + r2.``fixed``; not = r1.not; Token = r1.Token |> List.filter(fun tf -> subtr |> List.exists(fun te -> te = tf) |> not)}
    static member (+) (r1:OneInSet, r2:OneInSet) =
        {mainset = r1.mainset + r2.mainset; subtractset = r1.subtractset + r2.subtractset; not = r1.not; Token = r1.Token @ r2.Token }
    static member (+) (_:OneInSet, _:Plain) =
        failwith "Unsupported RGX addition"

    static member Create r tl = {mainset= r; subtractset = ""; not = false; Token = tl}

    member this.Not() = 
        {this with not = true}


type RGXType =
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

    override this.ToString() = 
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

    static member private DoConcat (r1:RGXType, r2:RGXType) = 
        match (r1,r2) with
        |   (Concat c1, _) -> Concat(r2 :: c1)
        |   _   -> Concat([r2; r1])

    static member (|||) (r1:RGXType, r2:RGXType) =
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


let AssesInputPostParseCondition (condition: RollingStream<TokenData> * TokenData -> bool) (rs:RollingStream<TokenData>) (rg:RGXType) =
    let rec parse rgx tkl =
        let conditionalParse rgx tk =
            let p = rs.Position
            let r = parse rgx tk 
            if not(fst(r)) then rs.Position <- p
            r

        let mkResult t tkl =
            function 
            | true ->  (true, t :: tkl) 
            | false -> (false, tkl)

        let rec repeatWhileMatching t acc =
            let pr = conditionalParse t tkl
            let nwa = (snd pr @ acc)
            if (fst pr) && nwa.Length > acc.Length then repeatWhileMatching t nwa
            else acc //|> List.rev

        let checkParseCondition() =
            let noToken = TokenData.Create Token.NoToken ""
            rs.PeekPrevious()
            |>  function
                |   Some x  -> condition (rs, x)
                |   None    -> condition (rs, noToken)

        if rs.EOF then (false, [])
        else
            match rgx with
            |   OneInSet ois    -> 
                if checkParseCondition() then
                    rs.Get() |> fun i -> ois.Token |> List.exists(fun e -> e=i.Token) |> mkResult i tkl
                else
                    (false, [])
            |   Plain pl        -> 
                if checkParseCondition() then
                    match (pl.``fixed``, pl.Token) with
                    |   ("^",[Token.NoToken]) ->
                        let pk = rs.PeekPrevious()
                        (pk = None || pk.Value.Token = Token.NewLine), []
                    |   (_, [Token.EOF]) ->
                        let isEof = rs.Peek().Token = Token.EOF
                        (isEof, [])
                    |   (_, []) -> true, []
                    | _ ->
                        if pl.``fixed``.Length > 1 then
                            let unescapedString =
                                pl.``fixed``
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
                            let concat = 
                                unescapedString.ToCharArray()
                                |>  List.ofArray
                                |>  List.map(fun c -> Plain <| Plain.Create (c.ToString()) pl.Token)
                                |>  List.rev
                            parse (Concat concat) tkl
                        else
                            rs.Get() |> fun i -> pl.``fixed`` = i.Source |> mkResult i tkl
                else
                    (false, [])
            |   Or rl           -> 
                let rec pickFirst l =
                    match l with
                    |   h::tl -> 
                        let rs = conditionalParse h tkl
                        if fst(rs) then (true, snd rs @ tkl)
                        else pickFirst tl
                    |   [] -> (false, tkl)
                rl |> List.rev |> pickFirst
            |   Concat rl       -> 
                let rec pickAll acc l =
                    match l with
                    |   h::tl -> 
                        let rs = parse h tkl
                        if fst(rs) then pickAll (snd rs @ acc) tl
                        else (false, tkl)
                    |   [] -> (true, acc)
                rl |> List.rev |> pickAll []
            |   IterRange (irx,mxo,mno) -> 
                let dec a = if a>=0 then (a-1) else a
                let rec repeatRange min max rx acc =
                    if max>0 then
                        let pr = conditionalParse rx tkl
                        let nwacc = (snd pr @ acc)
                        if (fst pr) && nwacc.Length > acc.Length && max>0 then repeatRange (dec min) (dec max) rx nwacc
                        else (min<=0),acc
                    else
                        true, acc
                match mno with
                |   Some minVal -> repeatRange minVal mxo irx []
                |   None        -> repeatRange mxo mxo irx []
            |   ZeroOrMore t        -> true, repeatWhileMatching t tkl
            |   ZeroOrMoreNonGreedy t -> true, repeatWhileMatching t tkl
            |   OneOrMore t         ->  repeatWhileMatching t tkl |> fun l -> (l.Length>=1), l
            |   OneOrMoreNonGreedy t -> repeatWhileMatching t tkl |> fun l -> (l.Length>=1), l
            |   Optional t          -> true, conditionalParse t tkl |> snd
            |   Group t             -> parse t tkl
    parse rg []
    |> fun (b,t) -> b, t |> List.rev

let AssesInput (rs:RollingStream<TokenData>) (rg:RGXType) = AssesInputPostParseCondition (fun _ -> true) rs rg

let TokenDataToString =
    function
    |   (true, tkl) -> (tkl |> List.map(fun td -> td.Source) |> List.fold(fun (str:StringBuilder) i -> str.Append(i)) (StringBuilder())).ToString() |> Some
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
    //|> TokenDataToString
    //|>  function
    //|   Some mts -> 
    //    let ml = Match(mts, p)
    //    ml.Length > 0
    //    |>  function
    //        |   true -> true
    //        |   false -> failwith "Difference between assesinput and regex"
    //| None -> false
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
    AssesInput s p 
    |>  fun (b, tkl) -> b, (tkl |> List.map(fun td -> td.Source) |> List.fold(fun (str:StringBuilder) i -> str.Append(i)) (StringBuilder())).ToString()
    //|> TokenDataToString
    //|>  function
    //|   Some mts -> 
    //    let ml = Match(mts, p)
    //    if ml.Length > 0 then
    //        let m0 = ml.[0]
    //        (true, m0)
    //    else
    //        failwith "Difference between assesinput and regex"
    //        //(false, "")
    //|   None -> (false, "")

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
            //None
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



