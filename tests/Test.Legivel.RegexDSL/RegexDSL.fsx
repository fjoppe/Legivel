module RegexDSL

#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#r @"bin/Debug/Legivel.Tokenizer.dll"

open Legivel.Tokenizer


type Plain =
    {
        ``fixed`` : string
        Token     : Token list
    }
    override this.ToString() = sprintf "%s" this.``fixed``

    static member (+) (r1:Plain, r2:Plain) = {``fixed`` = r1.``fixed`` + r2.``fixed``; Token = r1.Token @ r2.Token}

    static member Create r t = {``fixed`` = r; Token = t}


type OneInSet =
    {
        not      : bool
        mainset  : string
        subtractset : string
        Token     : Token list
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
        {mainset = r1.mainset; subtractset = r1.subtractset + r2.mainset; not = r1.not; Token = r1.Token |> List.filter(fun tf -> r2.Token |> List.exists(fun te -> te = tf))}
    static member (-) (r1:OneInSet, r2:Plain) =
        {mainset = r1.mainset; subtractset = r1.subtractset + r2.``fixed``; not = r1.not; Token = r1.Token |> List.filter(fun tf -> r2.Token |> List.exists(fun te -> te = tf))}
    static member (+) (r1:OneInSet, r2:OneInSet) =
        {mainset = r1.mainset + r2.mainset; subtractset = r1.subtractset + r2.subtractset; not = r1.not; Token = r1.Token @ r2.Token |> List.distinct}
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