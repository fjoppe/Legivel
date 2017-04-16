module RegexDSL

open System.Diagnostics
open System.Text
open System.IO
open System.Text.RegularExpressions
open System
open System.Globalization


exception RegexException of string

type Plain =
    private {
        ``fixed`` : string
    }
    override this.ToString() = sprintf "%s" this.``fixed``

    static member (+) (r1:Plain, r2:Plain) = {``fixed`` = r1.``fixed`` + r2.``fixed``}

    static member Create r = {``fixed`` = r}


type OneInSet =
    private {
        not      : bool
        mainset  : string
        subtractset : string
    }
    override this.ToString() =
        let subtract = this.subtractset <> ""
        match (subtract, this.not) with 
        //  https://msdn.microsoft.com/en-us/library/20bw873z(v=vs.110).aspx#Anchor_13
        |   (true, true)    -> sprintf "[%s-[^%s]]" (this.subtractset) (this.mainset)
        |   (true, false)   -> sprintf "[%s-[%s]]"  (this.mainset) (this.subtractset)
        |   (false, true)   -> sprintf "[^%s]" (this.mainset)
        |   (false, false)  -> sprintf "[%s]" (this.mainset) 
    static member (-) (r1:OneInSet, r2:OneInSet) =
        {mainset = r1.mainset; subtractset = r1.subtractset + r2.mainset; not = r1.not}
    static member (-) (r1:OneInSet, r2:Plain) =
        {mainset = r1.mainset; subtractset = r1.subtractset + r2.``fixed``; not = r1.not}
    static member (+) (r1:OneInSet, r2:OneInSet) =
        {mainset = r1.mainset + r2.mainset; subtractset = r1.subtractset + r2.subtractset; not = r1.not}
    static member (+) (r1:OneInSet, r2:Plain) =
        raise (RegexException "not supported")

    static member Create r =
         {mainset= r; subtractset = ""; not = false}
    member this.Not() = 
        {this with not = true}


type RGXType =
    |   Plain of Plain
    |   OneInSet   of OneInSet
    |   Or         of RGXType list
    |   Concat     of RGXType list
    |   IterRange  of RGXType * int * (int option)
    |   ZeroOrMore of RGXType
    |   OneOrMore  of RGXType
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
        |   OneOrMore  t -> sprintf "(?:%O)+" t
        |   Optional   t -> sprintf "(?:%O)?" t
        |   Group      t -> sprintf "(%O)" t
    static member (|||) (r1:RGXType, r2:RGXType) =
        match r1 with
        | Or     l ->   Or(r2 :: l)
        | _       ->    Or([r2; r1])

    static member private DoConcat (r1:RGXType, r2:RGXType) = 
        match (r1,r2) with
        |   (Concat c1, _) -> Concat(r2 :: c1)
        |   _   -> Concat([r2; r1])

    static member (-) (r1:RGXType, r2:RGXType) =
        match (r1,r2) with
        |   (OneInSet o1, OneInSet o2)  -> OneInSet(o1 - o2)
        |   (OneInSet o1,    Plain p1)  -> OneInSet(o1 - p1)
        |   _   -> raise (RegexException "These cannot be subtracted")

    static member (+) (r1:RGXType, r2:RGXType) =
        match (r1,r2) with
        |   (Plain p1   , Plain p2)      -> Plain(p1 + p2)
        |   (OneInSet o1, OneInSet o2)   -> OneInSet(o1 + o2)
        |   _   ->  RGXType.DoConcat(r1, r2)

    member this.Not = 
        match this with
        |   OneInSet o1 ->  OneInSet(o1.Not())
        |   _   -> raise (RegexException "Cannot invert this")


/// Regex pattern must repeat exactly given value, eg: Range(RGP("abc"), 2) := (abc){2}
let Repeat(t, mx) = IterRange(t, mx, None)

/// Regex pattern may repeat within given range, eg: Range(RGP("abc"), 2, 3) := (abc){2,3}
let Range(t, mn, mx) = IterRange(t, mx, Some(mn))

/// Regex pattern may repeat zero or more, eg: ZOM(RGP("abc")) := (abc)*
let ZOM(t) = ZeroOrMore(t)

/// Regex pattern may repeat once or more, eg: OOM(RGP("abc")) := (abc)+
let OOM(t) = OneOrMore(t)

/// Make Regex optional, eg: OPT(RGP("abc")) := (abc)?
let OPT(t) = Optional(t)

/// Plain regex pattern, eg: RGP("abc") := abc
let RGP c = Plain(Plain.Create c)

/// One in Set regex pattern, eg: RGO("a-zA-Z") := [a-zA-Z]
let RGO c = OneInSet(OneInSet.Create c)

/// Exclude Set regex pattern, eg: NOT(RGO("a-zA-Z")) := [^a-zA-Z]
let NOT (c:RGXType) = c.Not

/// Regex ToString
let RGS p = sprintf "\\A(%O)" p

/// Creates Regex group, eg GRP(RGP("abc")) := (abc)
let GRP p = Group(p)

/// Returns rest-string, where match 'm' is removed from source 's'
let Advance(m : string, s : string) =  s.Substring(m.Length)


type MatchResult = {
        FullMatch   : string
        Rest        : string
        Groups      : string list
    }
    with
        static member Create f r g = { FullMatch = f; Rest = r ; Groups = g }
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
        [ for g in mt -> g.Value ] //   |> List.tail

/// Returns whether pattern p matches on string s
//[<DebuggerStepThrough>]
let IsMatch(s, p) = 
    let ml = Match(s, p)
    ml.Length > 0
   

/// Checks for matches of pattern p in string s.
/// If matched, returns (true, <match-string>, <rest-string>), otherwise (false, "",s)
[<DebuggerStepThrough>]
let HasMatches(s,p) = 
    let ml = Match(s, p)
    if ml.Length > 0 then
        let m0 = ml.[0]
        (true, m0, Advance(m0, s))
    else
        (false, "",s)

/// Checks for matches of pattern p in string s.
/// If matched, returns rest-string, otherwise s.
/// This function may be useful to skip whitespace.
[<DebuggerStepThrough>]
let SkipIfMatch (s:string) (p:RGXType) =
    match (HasMatches(s, p)) with
    |   (true, mt,frs)  -> frs
    |   (false, _,_)    -> s

[<DebuggerStepThrough>]
let ``value or zero`` sv =
    match sv with
    |   Some(v) -> v
    |   None    -> 0


[<DebuggerStepThrough>]
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.Multiline)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

[<DebuggerStepThrough>]
let (|Regex2|_|) (pattern:RGXType) input =
    let m = Regex.Match(input, RGS(pattern), RegexOptions.Multiline)
    if m.Success then 
        let lst = [ for g in m.Groups -> g.Value ]
        let fullMatch = lst |> List.head
        let rest = Advance(fullMatch, input)
        let groups = lst |> List.tail
        Some(MatchResult.Create fullMatch rest groups)
    else None

let DecodeEncodedUnicodeCharacters value =
    Regex.Replace(value,
        @"(\\u(?<Value>[a-zA-Z0-9]{4}))|(\\U(?<Value>[a-zA-Z0-9]{8}))",
        (fun (m:Match) -> (char(Int64.Parse(m.Groups.["Value"].Value, NumberStyles.HexNumber))).ToString()))

let DecodeEncodedHexCharacters value =
    Regex.Replace(value,
        @"\\x(?<Value>[a-fA-F0-9]{2})",
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

[<DebuggerStepThrough>]
let (|Parse|_|) func ps = func ps
[<DebuggerStepThrough>]
let (|Eval|_|)  func x = func x

