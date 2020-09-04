module Legivel.Utilities.RegexDSL 

#nowarn "52" // "value has been copied to ensure the original is not mutated"

open System.Diagnostics
open System.Text.RegularExpressions
open System.Text
open System
open System.Globalization


exception RegexException of string


type Plain =
    private {
        ``fixed`` : string
        mutable optimized : RGXType list
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
                |>  List.map(fun c -> Plain <| Plain.Create (c.ToString()) )
                |>  List.rev

    static member (+) (r1:Plain, r2:Plain) = 
        let appd = r1.``fixed`` + r2.``fixed``
        {``fixed`` = appd; ``optimized`` = [] }

    static member Create r = {``fixed`` = r;  optimized = []}


and OneInSet =
    private {
        not      : bool
        mainset  : string
        subtractset : string
        OneInSet : Lazy<string>
    }

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
        let ms = r1.mainset
        let ss =  r1.subtractset + r2.mainset
        {mainset = ms; subtractset = ss; OneInSet = lazy(OneInSet.OptimizeSet ms ss); not = r1.not }

    static member (-) (r1:OneInSet, r2:Plain) =
        let ms = r1.mainset
        let ss = r1.subtractset + r2.``fixed``
        {mainset = ms; subtractset = ss; OneInSet = lazy(OneInSet.OptimizeSet ms ss); not = r1.not }

    static member (+) (r1:OneInSet, r2:OneInSet) =
        let ms = r1.mainset + r2.mainset
        let ss = r1.subtractset + r2.subtractset
        {mainset = ms; subtractset = ss; OneInSet = lazy(OneInSet.OptimizeSet ms ss); not = r1.not }

    static member (+) (_:OneInSet, _:Plain) =
        failwith "Unsupported RGX addition"

    static member Create r = 
        {mainset= r; subtractset = ""; OneInSet = lazy(OneInSet.OptimizeSet r ""); not = false}

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
let RGP c = Plain(Plain.Create c)

/// One in Set regex pattern, eg: RGO("a-zA-Z") := [a-zA-Z]
let RGO c = OneInSet(OneInSet.Create c)

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


type ParseInput = {
    InputYaml   : string 
    Position    : int
    Length      : int
}
with
    static member Create s = { InputYaml = s ; Position = 0; Length = s.Length }
    member this.Advance i = { this with Position = this.Position + i }
    member this.SetPosition i = { this with Position = i }
    member this.Peek() = this.InputYaml.[this.Position]
    member this.EOF with get() = this.Position >= this.Length

//type ParseResult = {
//    Groups  : (TokenData list) list;
//    Match   : (TokenData list)
//}
//with
//    static member Empty = {Groups = []; Match = []}
//    static member Create m g = {Groups = g; Match = m}


//type ParseOutput = bool * ParseResult

//let AssesInputPostParseCondition (condition: RollingStream<TokenData> * TokenData -> bool) (rs:RollingStream<TokenData>) (rg:RGXType) =
//    let rec parse rgx tkl gl : ParseOutput =
//        let conditionalParse rgx tk gl : ParseOutput =
//            let p = rs.Position
//            let r = parse rgx tk gl
//            if not(fst(r)) then rs.Position <- p
//            r

//        let mkResult t tkl gl =
//            function 
//            | true ->  (true,  ParseResult.Create (t :: tkl) gl) 
//            | false -> (false, ParseResult.Create tkl gl)

//        let rec repeatWhileMatching t acc gl =
//            let pr = conditionalParse t tkl gl
//            let added = (snd pr).Match
//            if (fst pr) && added <> [] then 
//                let nwa = (added @ acc)
//                repeatWhileMatching t nwa gl
//            else ParseResult.Create acc gl

//        let checkParseCondition() =
//            let noToken = TokenData.Create Token.NoToken ""
//            rs.PeekPrevious()
//            |>  function
//                |   Some x  -> condition (rs, x)
//                |   None    -> condition (rs, noToken)

//        if rs.EOF then 
//            (false, ParseResult.Empty)
//        else
//            match rgx with
//            |   OneInSet ois    -> 
//                if checkParseCondition() then
//                    rs.Get() 
//                    |> fun i -> 
//                        if uint32(i.Token) >= 0b0100_0000_0000_0000_0000_0000_0000_0000u then
//                            ois.Token |> List.exists(fun e -> e=i.Token)
//                        else
//                            let checkItWith = ois.TokenQuickCheck.Force()
//                            if (checkItWith &&& uint32(i.Token) > 0u) then
//                                //let p = ois.OneInSet.Force()
//                                //let r = p.Contains(i.Source)    // not ready yet to check one in set char-by-char
//                                true
//                            else
//                                false
//                        |> mkResult i tkl gl
//                else
//                    (false, ParseResult.Empty)
//            |   Plain pl        -> 
//                if checkParseCondition() then
//                    match (pl.``fixed``, pl.Token) with
//                    |   ("^",[Token.NoToken]) ->
//                        let pk = rs.PeekPrevious()
//                        (pk = None || pk.Value.Token = Token.NewLine), ParseResult.Empty
//                    |   (_, [Token.EOF]) ->
//                        let isEof = rs.Peek().Token = Token.EOF
//                        (isEof, ParseResult.Empty)
//                    |   (_, []) -> true, ParseResult.Empty
//                    | _ ->
//                        if pl.``fixed``.Length > 1 then
//                            pl.OptimizeOnce()
//                            parse (Concat pl.optimized) tkl gl
//                        else
//                            rs.Get() |> fun i -> pl.``fixed`` = i.Source |> mkResult i tkl gl
//                else
//                    (false, ParseResult.Empty)
//            |   Or rl           -> 
//                let rec pickFirst l =
//                    match l with
//                    |   h::tl -> 
//                        let rs = conditionalParse h tkl gl
//                        if fst(rs) then (true, ParseResult.Create ((snd rs).Match @ tkl) gl)
//                        else pickFirst tl
//                    |   [] -> (false, ParseResult.Empty)
//                rl |> List.rev |> pickFirst
//            |   Concat rl       -> 
//                let rec pickAll acc gi l  =
//                    match l with
//                    |   h::tl -> 
//                        let suc, rs = parse h tkl gi
//                        if suc then pickAll (rs.Match @ acc) (rs.Groups) tl 
//                        else (false, ParseResult.Create tkl gi)
//                    |   [] -> (true, ParseResult.Create acc gi)
//                rl |> List.rev |> pickAll [] gl
//            |   IterRange (irx,mxo,mno) -> 
//                let dec a = if a>=0 then (a-1) else a
//                let rec repeatRange min max rx acc =
//                    if max>0 then
//                        let pr = conditionalParse rx tkl gl
//                        let nwacc = ((snd pr).Match @ acc)
//                        if (fst pr) && nwacc.Length > acc.Length && max>0 then repeatRange (dec min) (dec max) rx nwacc
//                        else (min<=0), ParseResult.Create acc gl
//                    else
//                        true, ParseResult.Create acc gl
//                match mno with
//                |   Some minVal -> repeatRange minVal mxo irx []
//                |   None        -> repeatRange mxo mxo irx []
//            |   ZeroOrMore t         -> true, repeatWhileMatching t tkl gl 
//            |   ZeroOrMoreNonGreedy t -> true, repeatWhileMatching t tkl gl
//            |   OneOrMore t          ->  repeatWhileMatching t tkl gl |> fun l -> (l.Match.Length>=1), l
//            |   OneOrMoreNonGreedy t -> repeatWhileMatching t tkl gl |> fun l -> (l.Match.Length>=1), l
//            |   Optional t           -> true, conditionalParse t tkl gl |> snd
//            |   Group t              -> 
//                let s, res = parse t tkl gl
//                if s then
//                    s, { res with Groups = (res.Match |> List.rev)  :: res.Groups }
//                else
//                    s, res

//    parse rg [] []
//    |> fun (b,pr) -> b, { pr with Match = pr.Match |> List.rev}

let AssesInput (rs:ParseInput) (rg:RGXType) = 
    let rgx = new Regex(rg.ToString())
    rgx.Match(rs.InputYaml, rs.Position)


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
let IsMatch(s:ParseInput, p) = AssesInput s p |> fun m -> m.Success


[<DebuggerStepThrough>]
let IsMatchStr(s, p) = 
    let ml = Match(s, p)
    ml.Length > 0

/// Checks for matches of pattern p in string s.
/// If matched, returns (true, <match-string>, <rest-string>), otherwise (false, "",s)
[<DebuggerStepThrough>]
let HasMatches(s, p) = AssesInput s p
//    let m =  AssesInput s p
//    if not m.Success then
//        (false, String.Empty)
//    else
//        m.Value
//        |>  fun i -> i.Match
//        |> List.map(fun td -> td.Source)
//        |> List.fold(fun (str:StringBuilder) i -> str.Append(i)) (StringBuilder())
//        |> fun sb -> s, sb.ToString()

[<DebuggerStepThrough>]
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.Multiline)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

[<DebuggerStepThrough>]
let (|Regex2|_|) (pattern:RGXType) (input:ParseInput) =
    AssesInput input pattern 
    |>  function
        |   m when     m.Success ->
            let lst = [ for g in m.Groups -> g.Value ]
            let fullMatch = lst |> List.head
            let groups = lst |> List.tail
            Some(MatchResult.Create fullMatch groups)
        |   _ -> None


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



