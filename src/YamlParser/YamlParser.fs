module YamlParse

open System
open System.Text
open System.IO
open System.Text.RegularExpressions
open NLog
open NLog.FSharp
open System.Diagnostics

exception ParseException of string

type Plain =
    private {
        ``fixed`` : string
    }
    override this.ToString() =
        sprintf "%s" this.``fixed``
    static member (+) (r1:Plain, r2:Plain) =
        {``fixed`` = r1.``fixed`` + r2.``fixed``}
    static member Create r =
        {``fixed`` = r}


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
        raise (ParseException "not supported")

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
        |   _   -> raise (ParseException "These cannot be subtracted")

    static member (+) (r1:RGXType, r2:RGXType) =
        match (r1,r2) with
        |   (Plain p1   , Plain p2)      -> Plain(p1 + p2)
        |   (OneInSet o1, OneInSet o2)   -> OneInSet(o1 + o2)
        |   _   ->  RGXType.DoConcat(r1, r2)

    member this.Not = 
        match this with
        |   OneInSet o1 ->  OneInSet(o1.Not())
        |   _   -> raise (ParseException "Cannot invert this")


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
        static member Create f r g =
            { FullMatch = f; Rest = r ; Groups = g }

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


//[<DebuggerStepThrough>]
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


[<DebuggerStepThrough>]
let (|Parse|_|) func ps = func ps
[<DebuggerStepThrough>]
let (|Eval|_|)  func x = func x

type Context = ``Block-out`` | ``Block-in`` | ``Flow-out`` | ``Flow-in`` | ``Block-key`` | ``Flow-key``

type Chomping = ``Strip`` | ``Clip`` | ``Keep``

let ``start-of-line`` = RGP "\n" ||| RGP "^"

type TagScope =
    | Global
    | Local

type TagKind = 
    | Mapping
    | Sequence
    | Scalar

type Tag = {
        Scope   : TagScope
        Kind    : TagKind
        Uri     : string
        Short   : string
        Regex   : string
        canonFn : string -> string
    }
    with
        static member Create (scope, kind, uri, short, rgx, canon) =
            { 
                Scope = scope; 
                Kind = kind; 
                Uri = uri; 
                Short = short; 
                Regex = sprintf "^(%s)$" rgx
                canonFn = canon
            }

        static member Create (scope, kind, uri, short, rgx) =
            Tag.Create (scope, kind, uri, short, rgx, fun s -> s)

        static member Create (scope, kind, uri, short) =
            Tag.Create (scope, kind, uri, short, ".*", fun s -> s)

        static member Create (scope, kind, uri) =
            Tag.Create (scope, kind, uri, uri, ".*", fun s -> s)

        member this.Canonical s = this.canonFn s
        

type NodeData<'T> = {
        Tag  : Tag
        Data : 'T
    }
    with
        static member Create t d =
            { Tag = t; Data = d}

        member this.SetTag t = 
            { this with Tag = t}

type Node =
    | SeqNode of NodeData<Node list>
    | MapNode of NodeData<(Node*Node) list>
    | ScalarNode of NodeData<string>
    with
        member this.Indent l =
            [1 .. l] |> List.fold(fun s _ -> s + "  ") ""

        member this.ToCanonical l =
            match this with
            |   SeqNode n ->
                let ind0 = this.Indent l
                let head = sprintf "%s%s [\n" (ind0) (n.Tag.Short)
                let content = n.Data |> List.fold(fun s ni -> s + (sprintf "%s,\n" (ni.ToCanonical(l+1)))) ""
                let tail = sprintf "%s]\n" ind0
                sprintf "%s%s%s" head content tail
            |   MapNode n -> 
                let ind0 = this.Indent l
                let ind1 = this.Indent (l+1)
                let head = sprintf "%s%s {\n" (ind0) (n.Tag.Short)
                let content = 
                    n.Data 
                    |> List.fold(
                        fun s (k,v) -> 
                            let kc = k.ToCanonical(l+1)
                            let vc = v.ToCanonical(l+1)
                            match (k,v) with
                            |   (ScalarNode(_),ScalarNode(_))   -> s + sprintf "%s? %s\t: %s,\n" ind1 kc vc
                            |   _ -> s + sprintf "%s? %s\n%s: %s,\n" ind1 kc ind1 vc
                        ) ""
                let tail = sprintf "%s}\n" ind0
                sprintf "%s%s%s" head content tail
            |   ScalarNode n ->
                let ind0 = this.Indent l
                sprintf "%s%s \"%s\"" ind0 (n.Tag.Short) (n.Data)
        
        member this.SetTag t = 
            match this with
            |   SeqNode n       -> SeqNode(n.SetTag t)
            |   MapNode n       -> MapNode(n.SetTag t)
            |   ScalarNode n    -> ScalarNode(n.SetTag t)

        member this.NodeTag 
            with get() =
                match this with
                |   SeqNode n       -> n.Tag
                |   MapNode n       -> n.Tag
                |   ScalarNode n    -> n.Tag


type Legend = {
        YamlVersion : string
    }

//    Failsafe schema:  http://www.yaml.org/spec/1.2/spec.html#id2802346
let MappingGlobalTag =  Tag.Create(Global, Mapping, "tag:yaml.org,2002:map", "!!map")
let SequenceGlobalTag =  Tag.Create(Global, Sequence, "tag:yaml.org,2002:seq", "!!seq")
let StringGlobalTag = Tag.Create(Global, Scalar, "tag:yaml.org,2002:str", "!!str")


//    Json schema:  http://www.yaml.org/spec/1.2/spec.html#id2803231
let NullGlobalTag =
    Tag.Create(Global, Scalar, "tag:yaml.org,2002:null", "!!null", "~|null|Null|NULL",
        (fun s -> 
                match s with
                | Regex "~|null|Null|NULL" _ -> "~"
                | _ -> raise (ParseException (sprintf "Cannot convert to null: %s" s))
        )
    )

let BooleanGlobalTag = 
    Tag.Create(Global, Scalar, "tag:yaml.org,2002:bool", "!!bool",
        "y|Y|yes|Yes|YES|n|N|no|No|NO|true|True|TRUE|false|False|FALSE|on|On|ON|off|Off|OFF",
        (fun s -> 
            match s with
            | Regex "y|Y|yes|Yes|YES|true|True|TRUE|on|On|ON" _ -> "true"
            | Regex "n|N|no|No|NO|false|False|FALSE|off|Off|OFF" _ -> "false"
            | _ -> raise (ParseException (sprintf "Cannot convert to boolean: %s" s))
        )
    )

let IntegerGlobalTag = 
    Tag.Create(Global, Scalar, "tag:yaml.org,2002:int", "!!int",
        "[-+]?0b[0-1_]+|[-+]?0[0-7_]+|[-+]?(0|[1-9][0-9_]*)|[-+]?0x[0-9a-fA-F_]+|[-+]?[1-9][0-9_]*(:[0-5]?[0-9])+",
        (fun s ->
            // used for both digit and hex conversion
            let digitToValue c = if c >= 'A' then (int c)-(int 'A') else (int c)-(int '0')
            let convertToCanonical sign number = sprintf "%+d" (Int32.Parse(String.Concat(sign, number.ToString())))
            match s with
            | Regex "^([-+])?0b([0-1_]+)$" [sign; bs] -> 
                let ps = bs.Replace("_","").ToCharArray() |> List.ofArray
                let ic = ps |> List.fold(fun s c -> (s <<< 1) + (digitToValue  c)) 0
                convertToCanonical sign ic
            | Regex "^([-+])?0([0-7_]+)$"  [sign; os] -> 
                let ps = os.Replace("_","").ToCharArray() |> List.ofArray
                let ic = ps |> List.fold(fun s c -> (s <<< 3) + (digitToValue  c)) 0
                convertToCanonical sign ic
            | Regex "^([-+])?(0|[1-9][0-9_]*)$" [sign; is] -> sprintf "%+d" (Int32.Parse(String.Concat(sign, is.Replace("_",""))))
            | Regex "^([-+])?(0x[0-9a-fA-F_]+)$"  [sign; hs] -> 
                let ps = hs.Substring(2).ToUpper().Replace("_","").ToCharArray() |> List.ofArray
                let ic = ps |> List.fold(fun s c -> (s <<< 4) + (digitToValue  c)) 0
                convertToCanonical sign ic
            | Regex "^([-+])?([1-9][0-9_]*(:[0-5]?[0-9])+)$" ssl ->
                let sign = List.item 0 ssl
                let ss   = List.item 1 ssl
                let ps = ss.Replace("_","").Split([|":"|], StringSplitOptions.RemoveEmptyEntries)
                let ic = ps |> List.ofArray  |> List.fold(fun s t -> (s * 60) + (Int32.Parse(t))) 0
                convertToCanonical sign ic
            | _ -> raise (ParseException (sprintf "Cannot convert to integer: %s" s))
        )
    )

let FloatGlobalTag = 
    Tag.Create(Global, Scalar, "tag:yaml.org,2002:float", "!!float",
        "[-+]?([0-9][0-9_]*)?\.[0-9.]*([eE][-+][0-9]+)?|[-+]?[0-9][0-9_]*(:[0-5]?[0-9])+\.[0-9_]*|[-+]?\.(inf|Inf|INF)|\.(nan|NaN|NAN)",
        (fun s -> 
            let canonicalSign sign = if sign = "-" then "-" else "+"
            match s with
            | Regex "^([-+])?(0*)([1-9][0-9_]*)?\.(0*)([1-9][0-9.]*)(?:[eE]([-+])([0-9]+))?$" [sign; zmantissa; mantissa; zprec; prec; esign; exp] ->
                let cleanMantissa = mantissa.Replace("_","")
                let expCorrection, canMantissa = 
                    match cleanMantissa.Length with
                    | 0 -> (cleanMantissa.Length,  cleanMantissa + zprec)
                    | _ -> (-zprec.Length, "")
                let canExp = int(esign + "0" + exp) + expCorrection
                let canSign = canonicalSign sign
                sprintf "%s0.%s%se%+04d" canSign canMantissa prec canExp
            | Regex "^([-+]?)((?:[0-9][0-9_]*)(?::[0-5]?[0-9])+)\.([0-9_]*)$"  [sign; mantissa; prec] -> 
                let ps = mantissa.Replace("_","").Split([|":"|], StringSplitOptions.RemoveEmptyEntries)
                let ic = ps |> List.ofArray  |> List.fold(fun s t -> (s * 60) + (Int32.Parse(t))) 0
                let canSign = canonicalSign sign
                let canMantissa = ic.ToString()
                let canExp = canMantissa.Length
                sprintf "%s0.%s%se%+04d" canSign canMantissa (prec.Replace("_","")) canExp
            | Regex "^([-+]?)\.(?:inf|Inf|INF)$" [sign] ->
                let canSign = canonicalSign sign
                sprintf "%s.inf" canSign
            | Regex "^(\.(nan|NaN|NAN))$" _ -> ".nan"
            | _ -> raise (ParseException (sprintf "Cannot convert to float: %s" s))
        )
    )

//    FloatGlobalTag.Canonical "81.23"
//    FloatGlobalTag.Canonical "0.008123"
//    FloatGlobalTag.Canonical "1.008123"
//    FloatGlobalTag.Canonical "0.8123"
//FloatGlobalTag.Canonical "0o7"
//Regex.Matches("0o7", FloatGlobalTag.Regex)

//
//    float("+0.8123e+002") = float("81.23")
//    float("+0.8123e-002") = float("0.008123")
//    float("+0.1008123e+001") = float("1.008123")
//    float("0.8123") = float("+0.8123e+000")
//
//    FloatGlobalTag.Canonical "190:20:30.15"
//    float("+0.68523015e+006") = float("685230.15")

//    FloatGlobalTag.Canonical ".inf"
//    FloatGlobalTag.Canonical "-.inf"
//    FloatGlobalTag.Canonical ".nan"

//    Core Schema:  http://www.yaml.org/spec/1.2/spec.html#id2804923


let MapScalar s =
    match s with
    |   Regex (NullGlobalTag.Regex)     _ -> ScalarNode(NodeData<string>.Create NullGlobalTag s)
    |   Regex (BooleanGlobalTag.Regex)  _ -> ScalarNode(NodeData<string>.Create BooleanGlobalTag s)
    |   Regex (IntegerGlobalTag.Regex)  _ -> ScalarNode(NodeData<string>.Create IntegerGlobalTag s)
    |   Regex (FloatGlobalTag.Regex)    _ -> ScalarNode(NodeData<string>.Create FloatGlobalTag s)
    |   _ -> ScalarNode(NodeData<string>.Create StringGlobalTag s)

let NullScalarNode = ScalarNode(NodeData<string>.Create NullGlobalTag "")

let CreateMapNode d = MapNode(NodeData<(Node*Node) list>.Create MappingGlobalTag d)

let CreateSeqNode d = SeqNode(NodeData<Node list>.Create SequenceGlobalTag d)


type ParseState = {
        LineNumber  : int
        InputString : string
        n           : int
        m           : int
        c           : Context
        t           : Chomping
        Anchors     : Map<string, Node>
    }
    with
        member this.SetRestString s = { this with InputString = s}
        member this.AddAnchor s n = 
            let a = 
                if this.Anchors.ContainsKey s then this.Anchors.Remove(s).Add(s, n)
                else this.Anchors.Remove(s).Add(s, n)
            {this with Anchors = a}
        member this.GetAnchor s =
            if this.Anchors.ContainsKey s then this.Anchors.[s]
            else raise (ParseException (sprintf "Referenced anchor '%s' is unknown in line %d" s this.LineNumber))
        
        member this.Advance m = this.SetRestString (Advance(m, this.InputString))

        member this.SkipIfMatch p = this.SetRestString(SkipIfMatch (this.InputString) p)

        member  this.SetStyleContext cn = { this with c = cn}

        member this.SetIndent nn = { this with n = nn}

        member this.SetSubIndent mn = { this with m = mn}

        member this.FullIndented = { this with n = this.n + this.m; m=0 }

        member this.SetChomping tn = { this with t = tn }

        static member Create s =
            { LineNumber = 0; InputString = s; n=0; m=0; c=``Block-out``; t=``Clip``; Anchors = Map.empty}





let stringPosition (s:string) =
    s.Substring(0, 10)

let restString i o =
    match o with 
    |   None         -> i
    |   Some(c, ors) -> ors.InputString


//let s = "[ a , b]"
//let p = RGS((RGP "\\[") + OPT(``s-separate`` 0 ``Flow-in``))
//let groups = Regex.Match(s, RGS(p)).Groups
//[ for g in groups -> g.Value ]
//
//HasMatches(s, RGS((RGP "\\[") + OPT(``s-separate`` 0 ``Flow-in``)))
//Match(s, (RGP "\\[") + OPT(``s-separate`` 0 ``Flow-in``))
//Regex.Matches(s, RGS(p)).Count

//  Flow Collection styles: http://www.yaml.org/spec/1.2/spec.html#id2790088

type ParseFuncSingleResult = (Node * ParseState) option         //  returns parsed node, if possible
type ParseFuncListResult = (Node * ParseState) option           //  returns parsed node, if possible
type ParseFuncMappedResult = (Node * Node * ParseState) option  //  returns parsed key-value pair, if possible
type ParsFuncSig = (ParseState -> ParseFuncSingleResult)

type BlockFoldPrevType = EmptyAfterFolded | Empty | Indented | TextLine

type FlowCollectionStyles() =
    let logger = LogManager.GetCurrentClassLogger()

    //  Utility functions
    member this.``split by linefeed`` s = 
        Regex.Split(s, this.``b-as-line-feed``.ToString()) |> List.ofArray

    member this.``auto detect indent in block`` n (slst: string list) = 
        let ``match indented content`` s = 
            let icp = GRP(ZOM(RGP this.``s-space``)) + OOM(this.``ns-char``)
            match s with
            | Regex2(icp) mt -> Some(mt.ge1.Length - n)
            | _-> None
        slst
        |> List.tryPick(``match indented content``)
        |> function
            | Some v -> v 
            | None   -> 0

    member this.``auto detect indent in line`` ps =
        let icp = GRP(ZOM(RGP this.``s-space``)) + OOM(this.``ns-char``)
        match ps.InputString with
        | Regex2(icp) mt -> (mt.ge1.Length - ps.n)
        | _-> raise (ParseException(sprintf "Cannot detect indentation at '%s'" (stringPosition ps.InputString)))
        

    member this.``content with properties`` (``follow up func``: ParsFuncSig) ps =
            match (this.``c-ns-properties`` ps) with
            |   Some(prs, tag, anchor) -> 
                match (HasMatches(prs.InputString, (this.``s-separate`` prs))) with
                |   (true, mt, frs2) -> 
                    let prs = prs.SetRestString frs2
                    match (prs) with
                    |   Parse(``follow up func``) (c, prs) -> 
                        let t = Tag.Create(TagScope.Local, c.NodeTag.Kind, tag)
                        let c = c.SetTag(t)
                        Some(c, prs)
                    |   _ -> None
                |   (fase, _, _)    -> Some(NullScalarNode, prs)   //  ``e-scalar``
            |   None    -> None

    member this.``content with optional properties`` (``follow up func``: ParsFuncSig) ps =
            match (this.``c-ns-properties`` ps) with
            |   Some(prs, tag, anchor) -> 
                match (HasMatches(prs.InputString, (this.``s-separate`` prs))) with
                |   (true, mt, frs2) -> 
                    let prs = prs.SetRestString frs2
                    match (prs) with
                    |   Parse(``follow up func``) (c, prs) -> 
                        let t = Tag.Create(TagScope.Local, c.NodeTag.Kind, tag)
                        let c = c.SetTag(t)
                        Some(c, prs)
                    |   _ -> None
                |   (fase, _, _)    -> Some(NullScalarNode, prs)   //  ``e-scalar``
            |   None    -> ``follow up func`` ps
          
    //  [1] http://www.yaml.org/spec/1.2/spec.html#c-printable
    member this.``c-printable`` = 
        RGO ("\u0009\u000a\u000d\u0020-\u007e" +   // 8 - bit, #x9 | #xA | #xD | [#x20-#x7E]
             "\u0085\u00a0-\ud7ff\ue000-\ufffd")   // 16- bit, #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD]
                                                   //  32-bit -> currently not supported because .Net does not encode naturally. Yaml: [#x10000-#x10FFFF]

    //  [2] http://www.yaml.org/spec/1.2/spec.html#nb-json
    member ths.``nb-json`` = RGO "\u0009\u0020-\uffff"

    //  [3] http://www.yaml.org/spec/1.2/spec.html#c-byte-order-mark
    member this.``c-byte-order-mark`` = "\ufeff"

    //  [4] http://www.yaml.org/spec/1.2/spec.html#c-sequence-entry
    member this.``c-sequence-entry`` = "-"

    //  [5] http://www.yaml.org/spec/1.2/spec.html#c-mapping-key
    member this.``c-mapping-key`` = "?"

    //  [6] http://www.yaml.org/spec/1.2/spec.html#c-mapping-value
    member this.``c-mapping-value`` = ":"

    //  [7] http://www.yaml.org/spec/1.2/spec.html#c-collect-entry
    member this.``c-collect-entry`` = ","

    //  [8] http://www.yaml.org/spec/1.2/spec.html#c-sequence-start
    member this.``c-sequence-start`` = "["

    //  [9] http://www.yaml.org/spec/1.2/spec.html#c-sequence-end
    member this.``c-sequence-end`` = "]"

    //  [10]    http://www.yaml.org/spec/1.2/spec.html#c-mapping-start
    member this.``c-mapping-start`` = "{"

    //  [11]    http://www.yaml.org/spec/1.2/spec.html#c-mapping-end
    member this.``c-mapping-end`` = "}"

    //  [12]    http://www.yaml.org/spec/1.2/spec.html#c-comment
    member this.``c-comment`` = "#"

    //  [13]    http://www.yaml.org/spec/1.2/spec.html#c-anchor
    member this.``c-anchor`` = "&"

    //  [14]    http://www.yaml.org/spec/1.2/spec.html#c-alias
    member this.``c-alias`` = "*"

    //  [15]    http://www.yaml.org/spec/1.2/spec.html#c-tag
    member this.``c-tag`` = "!"

    //  [16]    http://www.yaml.org/spec/1.2/spec.html#c-literal
    member this.``c-literal`` = "|"

    //  [17]    http://www.yaml.org/spec/1.2/spec.html#c-folded
    member this.``c-folded`` = ">"

    //  [18]    http://www.yaml.org/spec/1.2/spec.html#c-single-quote
    member this.``c-single-quote`` = "\""

    //  [19]    http://www.yaml.org/spec/1.2/spec.html#c-double-quote
    member this.``c-double-quote`` = "\""

    //  [20]    http://www.yaml.org/spec/1.2/spec.html#c-directive
    member this.``c-directive`` = "%"

    //  [21]    http://www.yaml.org/spec/1.2/spec.html#c-reserved
    member this.``c-reserved`` = RGO "\u0040\u0060"

    //  [22]    http://www.yaml.org/spec/1.2/spec.html#c-indicator
    member this.``c-indicator`` = RGO  "\-\?:,\[\]\{\}#&\*!;>\'\"%@`"

    //  [23]    http://www.yaml.org/spec/1.2/spec.html#c-flow-indicator
    member this.``c-flow-indicator`` = RGO  @",\[\]\{\}"

    //  [24]    http://www.yaml.org/spec/1.2/spec.html#b-line-feed
    member this.``b-line-feed`` = RGP "\u000a"

    //  [25]    http://www.yaml.org/spec/1.2/spec.html#b-carriage-return
    member this.``b-carriage-return`` = RGP "\u000d"

    //  [i26]   http://www.yaml.org/spec/1.2/spec.html#b-char
    member this.``b-char`` = this.``b-line-feed`` + this.``b-carriage-return``

    //  [27]    http://www.yaml.org/spec/1.2/spec.html#nb-char
    member this.``nb-char``  = this.``c-printable`` - this.``b-char``

    //  [28]    http://www.yaml.org/spec/1.2/spec.html#b-break
    member this.``b-break`` = 
            (this.``b-carriage-return`` + this.``b-line-feed``) |||  //  DOS, Windows
            this.``b-carriage-return``                          |||  //  MacOS upto 9.x
            this.``b-line-feed``                                     //  UNIX, MacOS X

    //  [29]    http://www.yaml.org/spec/1.2/spec.html#b-as-line-feed
    member this.``b-as-line-feed`` = this.``b-break``

    //  [30]    http://www.yaml.org/spec/1.2/spec.html#b-non-content
    member this.``b-non-content`` = this.``b-break``

    //  [31]    http://www.yaml.org/spec/1.2/spec.html#s-space
    member this.``s-space`` = "\u0020"  // space

    //  [32]    http://www.yaml.org/spec/1.2/spec.html#s-tab
    member this.``s-tab`` = "\u0009"    // tab

    //  [33]    http://www.yaml.org/spec/1.2/spec.html#s-white
    member this.``s-white`` = RGO(this.``s-space`` + this.``s-tab``)

    //  [34]    http://www.yaml.org/spec/1.2/spec.html#ns-char
    member this.``ns-char`` = this.``nb-char`` - this.``s-white``

    //  [35]    http://www.yaml.org/spec/1.2/spec.html#ns-dec-digit
    member this.``ns-dec-digit`` = RGO "\u0030-\u0039"      //  0-9

    //  [36]    http://www.yaml.org/spec/1.2/spec.html#ns-hex-digit
    member this.``ns-hex-digit`` =
            this.``ns-dec-digit`` +
            RGO "\u0041-\u0046"  +  //  A-F
            RGO "\u0061-\u0066"     //  a-f

    //  [37]    http://www.yaml.org/spec/1.2/spec.html#ns-ascii-letter
    member this.``ns-ascii-letter`` = 
        RGO "\u0041-\u005A" +   //  A-Z
        RGO "\u0061-\u007A"     //  a-z

    //  [38]    http://www.yaml.org/spec/1.2/spec.html#ns-word-char
    member this.``ns-word-char`` =
        this.``ns-dec-digit`` + (RGO @"\-") + this.``ns-ascii-letter``

    //  [39]    http://www.yaml.org/spec/1.2/spec.html#ns-uri-char
    member this.``ns-uri-char`` = 
        (RGP @"%") + this.``ns-hex-digit`` + this.``ns-hex-digit``  |||
        (RGO @"#;/?:@&=+$,_.!~*\'\(\)\[\]") + this.``ns-word-char``

    //  [40]    http://www.yaml.org/spec/1.2/spec.html#ns-tag-char
    member this.``ns-tag-char`` = 
        (RGP @"%") + this.``ns-hex-digit`` + this.``ns-hex-digit``  |||
        (RGO @"#;/?:@&=+$_.~*\'\(\)") + this.``ns-word-char``

    //  [41]    http://www.yaml.org/spec/1.2/spec.html#c-escape
    member this.``c-escape`` = RGP "\\"

    //  [42]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-null
    member this.``ns-esc-null`` = RGP "0"

    //  [43]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-bell
    member this.``ns-esc-bell`` = RGP "a"

    //  [44]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-backspace
    member this.``ns-esc-backspace`` = RGP "b"

    //  [45]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-horizontal-tab
    member this.``ns-esc-horizontal-tab`` = RGP "t"

    //  [46]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-line-feed
    member this.``ns-esc-line-feed`` = RGP "n"

    //  [47]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-vertical-tab
    member this.``ns-esc-vertical-tab`` = RGP "v"

    //  [48]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-form-feed
    member this.``ns-esc-form-feed`` = RGP "f"

    //  [49]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-carriage-return
    member this.``ns-esc-carriage-return`` = RGP "r"

    //  [50]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-escape
    member this.``ns-esc-escape`` = RGP "e"

    //  [51]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-space
    member this.``ns-esc-space`` = RGP "\u0020"

    //  [52]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-double-quote
    member this.``ns-esc-double-quote`` = RGP "\""

    //  [53]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-slash
    member this.``ns-esc-slash`` = RGP "/"

    //  [54]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-backslash
    member this.``ns-esc-backslash`` = RGP "\\\\"

    //  [55]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-next-line
    member this.``ns-esc-next-line`` = RGP "N"

    //  [56]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-non-breaking-space
    member this.``ns-esc-non-breaking-space`` = RGP "_"

    //  [57]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-line-separator
    member this.``ns-esc-line-separator`` = RGP "L"

    //  [58]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-paragraph-separator
    member this.``ns-esc-paragraph-separator`` = RGP "P"

    //  [59]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-8-bit
    member this.``ns-esc-8-bit`` = (RGP "x") + Repeat(this.``ns-hex-digit``,2)

    //  [60]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-16-bit
    member this.``ns-esc-16-bit`` = RGP "u" + Repeat(this.``ns-hex-digit``,4)

    //  [61]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-32-bit
    member this.``ns-esc-32-bit`` = RGP "U" + Repeat(this.``ns-hex-digit``,8) // currently not supported

    //  [62]    http://www.yaml.org/spec/1.2/spec.html#c-ns-esc-char
    member this.``c-ns-esc-char`` = 
        RGP ("\\\\") +
            (this.``ns-esc-null``             |||
             this.``ns-esc-bell``             |||
             this.``ns-esc-backspace``        |||
             this.``ns-esc-horizontal-tab``   |||
             this.``ns-esc-line-feed``        |||
             this.``ns-esc-vertical-tab``     |||
             this.``ns-esc-form-feed``        |||
             this.``ns-esc-carriage-return``  |||
             this.``ns-esc-escape``           |||
             this.``ns-esc-space``            |||
             this.``ns-esc-double-quote``     |||
             this.``ns-esc-slash``            |||
             this.``ns-esc-backslash``        |||
             this.``ns-esc-next-line``        |||
             this.``ns-esc-non-breaking-space``|||
             this.``ns-esc-line-separator``   |||
             this.``ns-esc-paragraph-separator``|||
             this.``ns-esc-8-bit``            |||
             this.``ns-esc-16-bit``           |||
             this.``ns-esc-32-bit``)

    //  [63]    http://www.yaml.org/spec/1.2/spec.html#s-indent(n)
    member this.``s-indent(n)`` ps = Repeat(RGP this.``s-space``, ps.n)

    //  [64]    http://www.yaml.org/spec/1.2/spec.html#s-indent(<n)
    member this.``s-indent(<n)`` ps = Range(RGP this.``s-space``, 0, (ps.n-1)) (* Where m < n *)

    //  [65]    http://www.yaml.org/spec/1.2/spec.html#s-indent(≤n)
    member this.``s-indent(<=n)`` ps = Range(RGP this.``s-space``, 0, ps.n)  (* Where m ≤ n *)

    //  [66]    http://www.yaml.org/spec/1.2/spec.html#s-separate-in-line
    member this.``s-separate-in-line`` = OOM(this.``s-white``) ||| ``start-of-line``

    //  [67]    http://www.yaml.org/spec/1.2/spec.html#s-line-prefix(n,c)
    member this.``s-line-prefix`` ps =
        logger.Trace  "s-line-prefix"
        match ps.c with
        | ``Block-out`` ->  this.``s-block-line-prefix`` ps
        | ``Block-in``  ->  this.``s-block-line-prefix`` ps
        | ``Flow-out``  ->  this.``s-flow-line-prefix`` ps
        | ``Flow-in``   ->  this.``s-flow-line-prefix`` ps
        | _             ->  raise(ParseException "The context 'block-key' and 'flow-key' are not supported at this point")

    //  [68]    http://www.yaml.org/spec/1.2/spec.html#s-block-line-prefix(n)
    member this.``s-block-line-prefix`` ps = this.``s-indent(n)`` ps

    //  [69]    http://www.yaml.org/spec/1.2/spec.html#s-flow-line-prefix(n)
    member this.``s-flow-line-prefix`` ps = (this.``s-indent(n)`` ps) + OPT(this.``s-separate-in-line``)

    //  [70]    http://www.yaml.org/spec/1.2/spec.html#l-empty(n,c)
    member this.``l-empty`` ps = ((this.``s-line-prefix`` ps) ||| (this.``s-indent(<n)`` ps)) + this.``b-as-line-feed``

    //  [71]    http://www.yaml.org/spec/1.2/spec.html#b-l-trimmed(n,c)
    member this.``b-l-trimmed`` ps = this.``b-non-content`` + OOM(this.``l-empty`` ps)

    //  [72]    http://www.yaml.org/spec/1.2/spec.html#b-as-space
    member this.``b-as-space`` = this.``b-break``

    //  [73]    http://www.yaml.org/spec/1.2/spec.html#b-l-folded(n,c)
    member this.``b-l-folded`` ps = (this.``b-l-trimmed`` ps) ||| this.``b-as-space``

    //  [74]    http://www.yaml.org/spec/1.2/spec.html#s-flow-folded(n)
    member this.``s-flow-folded`` (ps:ParseState) =
        OPT(this.``s-separate-in-line``) + (this.``b-l-folded`` (ps.SetStyleContext ``Flow-in``)) + (this.``s-flow-line-prefix`` ps)

    //  [75]    http://www.yaml.org/spec/1.2/spec.html#c-nb-comment-text
    member this.``c-nb-comment-text`` = RGP("#") + ZOM(this.``nb-char``)

    //  [76]    http://www.yaml.org/spec/1.2/spec.html#b-comment
    member this.``b-comment`` = this.``b-non-content`` ||| RGP("\\z") // EOF..

    //  [77]    http://www.yaml.org/spec/1.2/spec.html#s-b-comment
    member this.``s-b-comment`` = OPT(this.``s-separate-in-line`` + OPT(this.``c-nb-comment-text``)) + this.``b-comment`` 

    //  [78]    http://www.yaml.org/spec/1.2/spec.html#l-comment
    member this.``l-comment`` = this.``s-separate-in-line`` + OPT(this.``c-nb-comment-text``) + this.``b-comment``

    //  [79]    http://www.yaml.org/spec/1.2/spec.html#s-l-comments
    member this.``s-l-comments`` = (this.``s-b-comment`` ||| ``start-of-line``) + ZOM(this.``l-comment``)

    //  [80]    http://www.yaml.org/spec/1.2/spec.html#s-separate(n,c)
    member this.``s-separate`` ps = 
        logger.Trace  "s-separate"
        match ps.c with 
        | ``Block-out`` ->  this.``s-separate-lines`` ps
        | ``Block-in``  ->  this.``s-separate-lines`` ps
        | ``Flow-out``  ->  this.``s-separate-lines`` ps
        | ``Flow-in``   ->  this.``s-separate-lines`` ps
        | ``Block-key`` ->  this.``s-separate-in-line``
        | ``Flow-key``  ->  this.``s-separate-in-line``

    //  [81]    http://www.yaml.org/spec/1.2/spec.html#s-separate-lines(n)
    member this.``s-separate-lines`` ps = (this.``s-l-comments`` + (this.``s-flow-line-prefix`` ps)) ||| this.``s-separate-in-line``

    //  [82]    http://www.yaml.org/spec/1.2/spec.html#l-directive
    member this.``l-directive`` = 
        (RGP "%") + (this.``ns-yaml-directive`` ||| this.``ns-tag-directive`` ||| this.``ns-reserved-directive``) + this.``s-l-comments``

    //  [83]    http://www.yaml.org/spec/1.2/spec.html#ns-reserved-directive
    member this.``ns-reserved-directive`` = 
        this.``ns-directive-name`` + ZOM(this.``s-separate-in-line`` + this.``ns-directive-parameter``)

    //  [84]    http://www.yaml.org/spec/1.2/spec.html#ns-directive-name
    member this.``ns-directive-name`` = OOM(this.``ns-char``)

    //  [85]    http://www.yaml.org/spec/1.2/spec.html#ns-directive-parameter
    member this.``ns-directive-parameter`` = OOM(this.``ns-char``)

    //  [86]    http://www.yaml.org/spec/1.2/spec.html#ns-yaml-directive
    member this.``ns-yaml-directive`` = RGP("YAML") + this.``s-separate-in-line`` + this.``ns-yaml-version``

    //  [87]    http://www.yaml.org/spec/1.2/spec.html#ns-yaml-version
    member this.``ns-yaml-version`` = OOM(this.``ns-dec-digit``) + RGP("\\.") + OOM(this.``ns-dec-digit``)

    //  [88]    http://www.yaml.org/spec/1.2/spec.html#ns-tag-directive
    member this.``ns-tag-directive`` = 
        (RGP "TAG") + this.``s-separate-in-line`` + this.``c-tag-handle`` + this.``s-separate-in-line`` + this.``ns-tag-prefix``

    //  [89]    http://www.yaml.org/spec/1.2/spec.html#c-tag-handle
    member this.``c-tag-handle`` = this.``c-named-tag-handle`` ||| this.``c-secondary-tag-handle`` ||| this.``c-primary-tag-handle``

    //  [90]    http://www.yaml.org/spec/1.2/spec.html#c-primary-tag-handle
    member this.``c-primary-tag-handle`` = RGP "!"

    //  [91]    http://www.yaml.org/spec/1.2/spec.html#c-secondary-tag-handle
    member this.``c-secondary-tag-handle`` = RGP "!!"

    //  [92]    http://www.yaml.org/spec/1.2/spec.html#c-named-tag-handle
    member this.``c-named-tag-handle`` = (RGP "!") + OOM(this.``ns-word-char``) + (RGP "!") 

    //  [93]    http://www.yaml.org/spec/1.2/spec.html#ns-tag-prefix
    member this.``ns-tag-prefix`` = this.``c-ns-local-tag-prefix`` ||| this.``ns-global-tag-prefix``

    //  [94]    http://www.yaml.org/spec/1.2/spec.html#c-ns-local-tag-prefix
    member this.``c-ns-local-tag-prefix`` = (RGP "!") + ZOM(this.``ns-uri-char``)

    //  [95]    http://www.yaml.org/spec/1.2/spec.html#ns-global-tag-prefix
    member this.``ns-global-tag-prefix`` = this.``ns-tag-char`` + ZOM(this.``ns-uri-char``)

    //  [96]    http://www.yaml.org/spec/1.2/spec.html#c-ns-properties(n,c)
    member this.``c-ns-properties`` ps : (ParseState * string * string) option =
        logger.Trace  "c-ns-properties"
        let ``tag anchor`` = GRP(this.``c-ns-tag-property``) + OPT((this.``s-separate`` ps) + GRP(this.``c-ns-anchor-property``))
        let ``anchor tag`` = GRP(this.``c-ns-anchor-property``) + OPT((this.``s-separate`` ps) + GRP(this.``c-ns-tag-property``))
        match (ps.InputString) with
        |   Regex2(``tag anchor``) mt -> 
            let (tag, anchor) = mt.ge2
            let prs = ps.SetRestString (SkipIfMatch (ps.InputString) ``tag anchor``)
            Some(prs, tag, anchor.Substring(1))
        |   Regex2(``anchor tag``) mt -> 
            let (anchor, tag) = mt.ge2
            let prs = ps.SetRestString (SkipIfMatch (ps.InputString) ``anchor tag``)
            Some(prs, tag, anchor.Substring(1))
        |   _ -> None

    //  [97]    http://www.yaml.org/spec/1.2/spec.html#c-ns-tag-property
    member this.``c-ns-tag-property`` = this.``c-verbatim-tag`` ||| this.``c-ns-shorthand-tag`` ||| this.``c-non-specific-tag``

    //  [98]    http://www.yaml.org/spec/1.2/spec.html#c-verbatim-tag
    member this.``c-verbatim-tag`` = (RGP "!<") + OOM(this.``ns-uri-char``) + (RGP ">") 

    //  [99]    http://www.yaml.org/spec/1.2/spec.html#c-ns-shorthand-tag
    member this.``c-ns-shorthand-tag`` = this.``c-tag-handle`` + OOM(this.``ns-tag-char``)

    //  [100]   http://www.yaml.org/spec/1.2/spec.html#c-non-specific-tag
    member this.``c-non-specific-tag`` = RGP "!"

    //  [101]   http://www.yaml.org/spec/1.2/spec.html#c-ns-anchor-property
    member this.``c-ns-anchor-property`` = (RGP "&") + this.``ns-anchor-name``

    //  [102]   http://www.yaml.org/spec/1.2/spec.html#ns-anchor-char
    member this.``ns-anchor-char`` =  this.``ns-char`` - this.``c-flow-indicator``

    //  [103]   http://www.yaml.org/spec/1.2/spec.html#ns-anchor-name
    member this.``ns-anchor-name`` = OOM(this.``ns-anchor-char``)

    //  [104]   http://www.yaml.org/spec/1.2/spec.html#c-ns-alias-node
    member this.``c-ns-alias-node`` ps : ParseFuncSingleResult =
        logger.Trace  "c-ns-alias-node"
        match (HasMatches(ps.InputString, (RGP "\\*"))) with
        |   (true, mt, frs) -> 
            match (HasMatches(frs, (this.``ns-anchor-name``))) with
            |   (true, mt, frs2) -> Some((ps.GetAnchor mt), ps.SetRestString frs2)
            |   (fase, _, _)    -> None            
        |   (fase, _, _)    -> None

    //  [105]   http://www.yaml.org/spec/1.2/spec.html#e-scalar
    member this.``e-scalar`` = RGP String.Empty     // we'll see if this works..

    //  [106]   http://www.yaml.org/spec/1.2/spec.html#e-node
    member this.``e-node`` = this.``e-scalar``

    //  [107]   http://www.yaml.org/spec/1.2/spec.html#nb-double-char
    member this.``nb-double-char`` = this.``c-ns-esc-char`` ||| (this.``nb-json`` - RGO("\\\\\""))

    //  [108]   http://www.yaml.org/spec/1.2/spec.html#ns-double-char
    member this.``ns-double-char`` = this.``c-ns-esc-char`` |||  (this.``nb-json`` - RGO("\\\\\"") - this.``s-white``)

    //  [109]   http://www.yaml.org/spec/1.2/spec.html#c-double-quoted(n,c)
    member this.``c-double-quoted`` ps : ParseFuncSingleResult = 
        logger.Trace  "c-double-quoted"
        let patt = RGS((RGP "\"") + GRP(this.``nb-double-text`` ps) + (RGP "\""))
        match ps.InputString with 
        |   Regex(patt)  [full; content] ->
            let n = MapScalar(content.Replace("\"\"", "\""))
            let prs = ps.Advance full
            Some(n, prs)
        |   _ -> None

    //  [110]   http://www.yaml.org/spec/1.2/spec.html#nb-double-text(n,c)
    member this.``nb-double-text`` ps =
        match ps.c with
        | ``Flow-out``  ->  this.``nb-double-multi-line`` ps
        | ``Flow-in``   ->  this.``nb-double-multi-line`` ps
        | ``Block-key`` ->  this.``nb-double-one-line``
        | ``Flow-key``  ->  this.``nb-double-one-line``
        | _             ->  raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")

    //  [111]   http://www.yaml.org/spec/1.2/spec.html#nb-double-one-line
    member this.``nb-double-one-line`` = ZOM(this.``nb-double-char``)

    //  [112]   http://www.yaml.org/spec/1.2/spec.html#s-double-escaped(n)
    member this.``s-double-escaped`` (ps:ParseState) = ZOM(this.``s-white``) + (RGP "\\\\") + this.``b-non-content`` + ZOM(this.``l-empty`` (ps.SetStyleContext ``Flow-in``)) + (this.``s-flow-line-prefix`` ps)

    //  [113]   http://www.yaml.org/spec/1.2/spec.html#s-double-break(n)
    member this.``s-double-break`` ps = (this.``s-double-escaped`` ps) ||| (this.``s-flow-folded`` ps)

    //  [114]   http://www.yaml.org/spec/1.2/spec.html#nb-ns-double-in-line
    member this.``nb-ns-double-in-line`` = ZOM(ZOM(this.``s-white``) + this.``ns-double-char``)

    //  [115]   http://www.yaml.org/spec/1.2/spec.html#s-double-next-line(n)
    member this.``s-double-next-line`` ps =  //  note, spec is recursive, below is an attempt to rewrite recursive regex 
        ZOM((this.``s-double-break`` ps) + this.``ns-double-char`` + this.``nb-ns-double-in-line``) + (this.``s-double-break`` ps) |||
        OOM((this.``s-double-break`` ps) + this.``ns-double-char`` + this.``nb-ns-double-in-line``) + ZOM(this.``s-white``)

    //  [116]   http://www.yaml.org/spec/1.2/spec.html#nb-double-multi-line(n)
    member this.``nb-double-multi-line`` ps = this.``nb-ns-double-in-line`` + ((this.``s-double-next-line`` ps) ||| ZOM(this.``s-white``))

    //  [117]    http://www.yaml.org/spec/1.2/spec.html#c-quoted-quote
    member this.``c-quoted-quote`` = RGP "\'\'"

    //  [118]   http://www.yaml.org/spec/1.2/spec.html#nb-single-char
    member this.``nb-single-char`` = this.``c-quoted-quote`` ||| (this.``nb-json`` - (RGP "\'"))

    //  [119]   http://www.yaml.org/spec/1.2/spec.html#ns-single-char
    member this.``ns-single-char`` = // this.``nb-single-char`` - this.``s-white``
        this.``c-quoted-quote`` ||| (this.``nb-json`` - (RGO "\'") - this.``s-white``)

    //  [120]   http://www.yaml.org/spec/1.2/spec.html#c-single-quoted(n,c)
    member this.``c-single-quoted`` ps : ParseFuncSingleResult = 
        logger.Trace  "c-single-quoted"
        let patt = RGS((RGP "\'") + GRP(this.``nb-single-text`` ps) + (RGP "\'"))
        match ps.InputString with
        |   Regex(patt)  [full; content] ->
            let n = MapScalar(content.Replace("''", "'"))
            let prs = ps.Advance full
            Some(n, prs)
        |   _ -> None

    //  [121]   http://www.yaml.org/spec/1.2/spec.html#nb-single-text(n,c)
    member this.``nb-single-text`` ps =
        logger.Trace  "nb-single-text"
        match ps.c with
        |   ``Flow-out``    -> this.``nb-single-multi-line`` ps
        |   ``Flow-in``     -> this.``nb-single-multi-line`` ps
        |   ``Block-key``   -> this.``nb-single-one-line``
        |   ``Flow-key``    -> this.``nb-single-one-line``
        | _             ->  raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")

    //  [122]   http://www.yaml.org/spec/1.2/spec.html#nb-single-one-line    
    member this.``nb-single-one-line`` = ZOM(this.``nb-single-char``)

    //  [123]   http://www.yaml.org/spec/1.2/spec.html#nb-ns-single-in-line
    member this.``nb-ns-single-in-line`` = ZOM(ZOM(this.``s-white``) + this.``ns-single-char``)

    //  [124]   http://www.yaml.org/spec/1.2/spec.html#s-single-next-line(n)
    member this.``s-single-next-line`` ps = OPT((this.``s-flow-folded`` ps) + ZOM(this.``ns-single-char`` + this.``nb-ns-single-in-line``) + ZOM(this.``s-white``))

    //  [125]   http://www.yaml.org/spec/1.2/spec.html#nb-single-multi-line(n)
    member this.``nb-single-multi-line`` ps = this.``nb-ns-single-in-line`` + ((this.``s-single-next-line`` ps) ||| ZOM(this.``s-white``))

    //  [126]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-first(c)
    member this.``ns-plain-first`` ps = (this.``ns-char`` - this.``c-indicator``) ||| ((RGP "\\?") ||| (RGP ":") ||| (RGP "-")) + (this.``ns-plain-safe`` ps)

    //  [127]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-safe(c)
    member this.``ns-plain-safe`` ps =
        logger.Trace  "ns-plain-safe"
        match ps.c with
        |   ``Flow-out``    -> this.``ns-plain-safe-out``
        |   ``Flow-in``     -> this.``ns-plain-safe-in``
        |   ``Block-key``   -> this.``ns-plain-safe-out``
        |   ``Flow-key``    -> this.``ns-plain-safe-in``
        | _             ->  raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")

    //  [128]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-safe-out
    member this.``ns-plain-safe-out`` = this.``ns-char``

    //  [129]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-safe-in
    member this.``ns-plain-safe-in`` = this.``ns-char`` - this.``c-flow-indicator``

    //  [130]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-char(c)
    member this.``ns-plain-char`` ps = (this.``ns-plain-safe`` ps) - (RGO ":#") ||| (this.``ns-char`` + (RGP "#")) ||| (RGP ":") + (this.``ns-plain-safe`` ps)

    //  [131]   http://www.yaml.org/spec/1.2/spec.html#ns-plain(n,c)
    member this.``ns-plain`` ps =
        logger.Trace  "ns-plain"
        match ps.c with
        | ``Flow-out``  -> this.``ns-plain-multi-line`` ps
        | ``Flow-in``   -> this.``ns-plain-multi-line`` ps
        | ``Block-key`` -> this.``ns-plain-one-line`` ps
        | ``Flow-key``  -> this.``ns-plain-one-line`` ps
        | _              -> raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")

    //  [132]   http://www.yaml.org/spec/1.2/spec.html#nb-ns-plain-in-line(c)
    member this.``nb-ns-plain-in-line`` ps = ZOM(ZOM(this.``s-white``) + (this.``ns-plain-char`` ps))

    //  [133]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-one-line(c)
    member this.``ns-plain-one-line`` ps = (this.``ns-plain-first`` ps) + (this.``nb-ns-plain-in-line`` ps)

    //  [134]   http://www.yaml.org/spec/1.2/spec.html#s-ns-plain-next-line(n,c)
    member this.``s-ns-plain-next-line`` ps = (this.``s-flow-folded`` ps) + (this.``ns-plain-char`` ps) + (this.``nb-ns-plain-in-line`` ps)

    //  [135]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-multi-line(n,c)
    member this.``ns-plain-multi-line`` ps = (this.``ns-plain-one-line`` ps) + ZOM(this.``s-ns-plain-next-line`` ps)

    //  [136]   http://www.yaml.org/spec/1.2/spec.html#in-flow(c)
    member this.``in-flow`` ps =
        match ps.c with
        |   ``Flow-out`` -> ``Flow-in``
        |   ``Flow-in``  -> ``Flow-in``
        |   ``Block-key``-> ``Flow-key``
        |   ``Flow-key`` -> ``Flow-key``
        | _              -> raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")

    //  [137]   http://www.yaml.org/spec/1.2/spec.html#c-flow-sequence(n,c)
    member this.``c-flow-sequence`` ps : ParseFuncSingleResult=
        logger.Trace  "c-flow-sequence"
        match (HasMatches(ps.InputString, RGS((RGP "\\[") + OPT(this.``s-separate`` ps)))) with
        | (false, _, _) ->  None
        | (true, m, inputrs)  -> 
            let prs = ps.SetRestString inputrs
            match (prs.SetStyleContext(this.``in-flow`` prs)) with
            |   Parse(this.``ns-s-flow-seq-entries``) (c, prs2) ->  
                match (HasMatches(prs2.InputString, (RGP "\\]"))) with
                |  (true, mt, frs)  -> Some(c, prs2.SetRestString frs)
                |  (false, _,_)    -> raise (ParseException(sprintf "Expected ']' at \"%s\"" (prs2.InputString)))
            |   _ ->
                match (HasMatches(prs.InputString, (RGP "\\]"))) with
                |  (true, mt, frs) ->  Some((CreateSeqNode []),(ps.SetRestString frs))
                |  (false, _,_)    -> raise (ParseException(sprintf "Expected ']' at \"%s\"" (prs.InputString)))
                
    //  [138]   http://www.yaml.org/spec/1.2/spec.html#ns-s-flow-seq-entries(n,c)
    member this.``ns-s-flow-seq-entries`` ps : ParseFuncSingleResult =
        logger.Trace  "ns-s-flow-seq-entries"
        let rec ``ns-s-flow-seq-entries`` (ps:ParseState) (lst:Node list) : ParseFuncSingleResult =
            match ps with
            |   Parse(this.``ns-flow-seq-entry``) (entry, prs) ->
                let lst = entry :: lst
                let rs = SkipIfMatch (prs.InputString) (OPT(this.``s-separate`` prs))
                match (HasMatches(rs, (RGP ",") + OPT(this.``s-separate`` prs))) with 
                |   (true, mt, frs) ->  
                    ``ns-s-flow-seq-entries`` (prs.SetRestString frs) lst
                |   (false, _,_)    -> Some(CreateSeqNode(lst |> List.rev), (prs.SetRestString rs))
            |   _ -> None   // empty sequence
        ``ns-s-flow-seq-entries`` ps []

    //  [139]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-seq-entry(n,c)
    member this.``ns-flow-seq-entry`` ps : ParseFuncSingleResult =
        logger.Trace  "ns-flow-seq-entry"
        match ps with
        |   Parse(this.``ns-flow-pair``) (ck, cv, prs) -> Some(CreateMapNode[(ck,cv)], prs)
        |   Parse(this.``ns-flow-node``) (c, prs) -> Some(c, prs)
        |   _ -> None

    //  [140]   http://www.yaml.org/spec/1.2/spec.html#c-flow-mapping(n,c)
    member this.``c-flow-mapping`` ps : ParseFuncSingleResult =
        logger.Trace  "c-flow-mapping"
        match (HasMatches(ps.InputString, RGS((RGP "\\{") + ZOM(this.``s-separate`` ps)))) with
        |   (true, mt, frs) -> 
            let prs = ps.SetRestString frs
            let prs, mappings =
                match (prs.SetStyleContext(this.``in-flow`` prs)) with
                |   Parse(this.``ns-s-flow-map-entries``) (c, prs2) -> (prs2, c)
                |   _ -> (prs, CreateMapNode [])
            match (HasMatches(prs.InputString, (RGP "\\}"))) with
            |   (true, mt, frs2) -> 
                let prs2 = prs.SetRestString frs2
                Some(mappings, prs2)
            |   (fase, _, _)    -> raise (ParseException(sprintf "Expected '}' at \"%s\"" (prs.InputString.Substring(6))))
        |   (fase, _, _)    -> None
            
    //  [141]   http://www.yaml.org/spec/1.2/spec.html#ns-s-flow-map-entries(n,c)
    member this.``ns-s-flow-map-entries`` ps : ParseFuncListResult =
        logger.Trace  "ns-s-flow-map-entries"
        let rec ``ns-s-flow-map-entries`` (ps:ParseState) (lst:(Node*Node) list) : ParseFuncSingleResult =
            match (ps) with
            |   Parse(this.``ns-flow-map-entry``) (ck, cv, prs) ->
                let lst = (ck, cv) :: lst
                let prs = prs.SkipIfMatch (OPT(this.``s-separate`` prs))
                match (HasMatches(prs.InputString, (RGP ",") + ZOM(this.``s-separate`` prs))) with 
                |   (true, mt, frs) ->  
                    ``ns-s-flow-map-entries`` (prs.SetRestString frs) lst
                |   (false, _,_)    -> Some(CreateMapNode(lst |> List.rev), prs)
            |   _ -> None   // empty sequence
        ``ns-s-flow-map-entries`` ps []

    //  [142]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-map-entry(n,c)
    member this.``ns-flow-map-entry`` ps : ParseFuncMappedResult =
        logger.Trace  "ns-flow-map-entry"
        let ``ns-flow-map-explicit-entry`` ps = 
            match (HasMatches(ps.InputString, RGP "\\?" + (this.``s-separate`` ps))) with
            | (false, _, _) ->  None
            | (true, m, inputrs)  -> this.``ns-flow-map-explicit-entry`` (ps.SetRestString inputrs)
        match (ps) with
        |   Parse(``ns-flow-map-explicit-entry``)       (ck, cv, prs) -> Some(ck, cv, prs)
        |   Parse(this.``ns-flow-map-implicit-entry``)  (ck, cv, prs) -> Some(ck, cv, prs)
        |   _ -> None

    //  [143]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-map-explicit-entry(n,c)
    member this.``ns-flow-map-explicit-entry`` ps : ParseFuncMappedResult =
        logger.Trace  "ns-flow-map-explicit-entry"
        match (ps) with
        |   Parse(this.``ns-flow-map-implicit-entry``) (ck, cv, prs) -> Some(ck, cv, prs)
        |   _ -> Some(NullScalarNode, NullScalarNode, ps)       // ( ``e-node`` + ``e-node``)

    //  [144]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-map-implicit-entry(n,c)
    member this.``ns-flow-map-implicit-entry`` ps : ParseFuncMappedResult =
        logger.Trace  "ns-flow-map-implicit-entry"
        match (ps) with
        |   Parse(this.``ns-flow-map-yaml-key-entry``)      (ck, cv, prs) -> Some(ck, cv, prs)
        |   Parse(this.``c-ns-flow-map-empty-key-entry``)   (ck, cv, prs) -> Some(ck, cv, prs)
        |   Parse(this.``c-ns-flow-map-json-key-entry``)    (ck, cv, prs) -> Some(ck, cv, prs)
        |   _ -> None

    //  [145]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-map-yaml-key-entry(n,c)
    member this.``ns-flow-map-yaml-key-entry`` ps =
        logger.Trace  "ns-flow-map-yaml-key-entry"
        match (ps) with
        |   Parse(this.``ns-flow-yaml-node``) (ck, prs) -> 
            let prs = prs.SkipIfMatch (OPT(this.``s-separate`` prs))
            match (prs) with
            |   Parse(this.``c-ns-flow-map-separate-value``) (cv, prs2) -> Some(ck,cv, prs2)
            |   _ -> Some(ck, NullScalarNode, prs)  //  ``e-node``
        |   _   -> None

    //  [146]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-map-empty-key-entry(n,c)
    member this.``c-ns-flow-map-empty-key-entry`` ps : ParseFuncMappedResult =
        logger.Trace  "c-ns-flow-map-empty-key-entry"
        match (ps) with
        |   Parse(this.``c-ns-flow-map-separate-value``) (c, prs) -> Some(NullScalarNode, c, prs)   //  ``e-node``
        |   _ -> None

    //  [147]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-map-separate-value(n,c)
    member this.``c-ns-flow-map-separate-value`` ps : ParseFuncSingleResult =
        logger.Trace  "c-ns-flow-map-separate-value"
        match (HasMatches(ps.InputString, (RGP ":") (* + NOT(``ns-plain-safe`` c) *) )) with
        |   (true, mt, frs) -> 
            match (HasMatches(frs, (this.``s-separate`` ps))) with
            |   (true, mt, frs2) -> 
                let prs = ps.SetRestString frs2
                match (prs) with
                |   Parse(this.``ns-flow-node``) (c, rs) -> Some(c, rs)
                |   _ -> None
            |   (fase, _, _)    -> Some(NullScalarNode, ps.SetRestString frs)   //  ``e-node``
        |   (fase, _, _)    -> None

    //  [148]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-map-json-key-entry(n,c)
    member this.``c-ns-flow-map-json-key-entry`` ps =
        logger.Trace  "c-ns-flow-map-json-key-entry"
        match (ps) with
        |   Parse(this.``c-flow-json-node``) (ck, prs) -> 
            let prs = prs.SkipIfMatch (OPT(this.``s-separate`` prs))
            match (prs) with
            |   Parse(this.``c-ns-flow-map-adjacent-value``) (cv, prs2) -> Some(ck,cv, prs2)
            |   _ -> Some(ck, NullScalarNode, prs)  //  ``e-node``
        |   _    -> None

    //  [149]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-map-adjacent-value(n,c)
    member this.``c-ns-flow-map-adjacent-value`` ps =
        logger.Trace  "c-ns-flow-map-adjacent-value"
        match (HasMatches(ps.InputString, (RGP ":"))) with
        |   (true, mt, frs) -> 
            let prs = ps.SetRestString frs
            let prs = prs.SkipIfMatch (OPT(this.``s-separate`` ps))
            match (prs) with
            |   Parse(this.``ns-flow-node``) (c, prs2) -> Some(c, prs2)
            |   _ -> Some(NullScalarNode, prs)  //  ``e-node``
        |   (fase, _, _)    -> None

    //  [150]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-pair(n,c)
    member this.``ns-flow-pair`` ps : ParseFuncMappedResult =
        logger.Trace  "ns-flow-pair"
        let ``ns-flow-map-explicit-entry`` ps = 
            match (HasMatches(ps.InputString, RGP "\\?" + (this.``s-separate`` ps))) with
            | (false, _, _) ->  None
            | (true, m, inputrs)  -> this.``ns-flow-map-explicit-entry`` (ps.SetRestString inputrs)
        match (ps) with
        |   Parse(``ns-flow-map-explicit-entry``)   (ck, cv, prs) -> Some(ck, cv, prs)
        |   Parse(this.``ns-flow-pair-entry``)      (ck, cv, prs) -> Some(ck, cv, prs)
        |   _ -> None

    //  [151]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-pair-entry(n,c)
    member this.``ns-flow-pair-entry`` ps : ParseFuncMappedResult =
        logger.Trace  "ns-flow-pair-entry"
        match (ps) with
        |   Parse(this.``ns-flow-pair-yaml-key-entry``)     (ck, cv, prs) -> Some(ck, cv, prs)
        |   Parse(this.``c-ns-flow-map-empty-key-entry``)   (ck, cv, prs) -> Some(ck, cv, prs)
        |   Parse(this.``c-ns-flow-pair-json-key-entry``)   (ck, cv, prs) -> Some(ck, cv, prs)
        |   _ -> None

    //  [152]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-pair-yaml-key-entry(n,c)
    member this.``ns-flow-pair-yaml-key-entry`` ps : ParseFuncMappedResult =
        logger.Trace  "ns-flow-pair-yaml-key-entry"
        let ``ns-s-implicit-yaml-key`` (ps:ParseState) = (this.``ns-s-implicit-yaml-key`` (ps.SetStyleContext ``Flow-key``))
        match (ps) with
        |   Parse(``ns-s-implicit-yaml-key``) (ck, prs) -> 
            match (prs) with
            |   Parse(this.``c-ns-flow-map-separate-value``) (cv, prs2) -> Some(ck, cv, prs2)
            |   _ ->    None
        |   _ -> None

    //  [153]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-pair-json-key-entry(n,c)
    member this.``c-ns-flow-pair-json-key-entry`` ps : ParseFuncMappedResult =
        logger.Trace  "c-ns-flow-pair-json-key-entry"
        match (this.``c-s-implicit-json-key`` (ps.SetStyleContext ``Flow-key``)) with
        |   Some(ck, prs) ->
            match (this.``c-ns-flow-map-adjacent-value`` prs) with
            |   Some(cv, prs2) -> Some(ck, cv, prs2)
            |   None -> None
        |   None -> None

    //  [154]   http://www.yaml.org/spec/1.2/spec.html#ns-s-implicit-yaml-key(c)
    member this.``ns-s-implicit-yaml-key`` ps : ParseFuncSingleResult =
        logger.Trace  "ns-s-implicit-yaml-key"
        let ``n/a`` = 0
        match (ps.SetIndent ``n/a``) with
        |   Parse(this.``ns-flow-yaml-node``) (ck, prs) -> 
            let prs = prs.SetRestString(SkipIfMatch prs.InputString (OPT(this.``s-separate-in-line``)))
            Some(ck, prs)
        |   _ -> None

    //  [155]   http://www.yaml.org/spec/1.2/spec.html#c-s-implicit-json-key(c)
    member this.``c-s-implicit-json-key`` ps : ParseFuncSingleResult = (* At most 1024 characters altogether *)
        logger.Trace  "c-s-implicit-json-key"
        let ``n/a`` = 0
        match (ps.SetIndent ``n/a``) with
        |   Parse(this.``c-flow-json-node``)    (c, prs) -> 
            let prs = prs.SkipIfMatch (OPT(this.``s-separate-in-line``))
            Some(c, prs)
        |   _   -> None

    //  [156]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-yaml-content(n,c)
    member this.``ns-flow-yaml-content`` ps : ParseFuncSingleResult = 
        logger.Trace  "ns-flow-yaml-content"
        match (HasMatches(ps.InputString, RGS(this.``ns-plain`` ps))) with
        |   (true, mt, frs) -> Some(MapScalar(mt), ps.SetRestString frs)
        |   (fase, _, _)    -> None

    //  [157]   http://www.yaml.org/spec/1.2/spec.html#c-flow-json-content(n,c)
    member this.``c-flow-json-content`` ps : ParseFuncSingleResult =
        logger.Trace  "c-flow-json-content"
        match (ps) with
        |   Parse(this.``c-single-quoted``)  (c, prs) -> Some(c, prs)
        |   Parse(this.``c-double-quoted``)  (c, prs) -> Some(c, prs)
        |   Parse(this.``c-flow-mapping``)   (c, prs) -> Some(c, prs)
        |   Parse(this.``c-flow-sequence``)  (c, prs) -> Some(c, prs)
        |   _ -> None
        
    //  [158]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-content(n,c)
    member this.``ns-flow-content`` ps : ParseFuncSingleResult =
        logger.Trace  "ns-flow-content"
        match (ps) with
        |   Parse(this.``c-flow-json-content``)  (c, prs) -> Some(c, prs)
        |   Parse(this.``ns-flow-yaml-content``) (c, prs) -> Some(c, prs)
        |   _ -> None

    //  [159]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-yaml-node(n,c)
    member this.``ns-flow-yaml-node`` ps : ParseFuncSingleResult =
        logger.Trace  "ns-flow-yaml-node"
        match (ps) with
        |   Parse(this.``c-ns-alias-node``)      (c, rs) -> Some(c, rs)
        |   Parse(this.``ns-flow-yaml-content``) (c, rs) -> Some(c, rs)
        |   Parse(this.``content with properties`` this.``ns-flow-yaml-content``)  (c, prs) -> Some(c, prs)
        |   _ -> None

    //  [160]   http://www.yaml.org/spec/1.2/spec.html#c-flow-json-node(n,c)
    member this.``c-flow-json-node`` ps : ParseFuncSingleResult =
        logger.Trace  "c-flow-json-node"
        match (ps) with
        |   Parse(this.``content with optional properties`` this.``c-flow-json-content``) (c, prs) -> Some(c, prs)
        |   _ -> None
    
    //  [161]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-node(n,c)
    member this.``ns-flow-node`` ps : ParseFuncSingleResult =
        logger.Trace  "ns-flow-node"
        match (ps) with
        |   Parse(this.``c-ns-alias-node``)     (c, prs) -> Some(c, prs)
        |   Parse(this.``ns-flow-content``)     (c, prs) -> Some(c, prs)
        |   Parse(this.``content with properties`` this.``ns-flow-content``)  (c, prs) -> Some(c, prs)
        |   _ -> None

    //  [162]   http://www.yaml.org/spec/1.2/spec.html#c-b-block-header(m,t)
    member this.``c-b-block-header`` ps = 
        logger.Trace  "c-b-block-header"
        let chomp indicator =
            match indicator with
            |   "-" -> ``Strip``
            |   "+" -> ``Keep``
            |   ""  -> ``Clip``
            |   _ -> raise (ParseException(sprintf "Illegal chomp indicator '%s'" indicator))
        let indent i = 
            if i = "" then None    
            else Some(Int32.Parse i)

        let ``indent chomp`` ps : (int option * ParseState) option = 
            let p = GRP(this.``c-indentation-indicator``) + GRP(this.``c-chomping-indicator``) + this.``s-b-comment``
            match ps.InputString with
            | Regex2(p)  mt -> 
                let (i, c) = mt.ge2
                Some(indent  i, (ps.SetRestString mt.Rest).SetChomping (chomp c))
            |   _ -> None
        let ``chomp indent`` ps : (int option * ParseState) option = 
            let p = GRP(this.``c-chomping-indicator``) + GRP(this.``c-indentation-indicator``) + this.``s-b-comment``
            match ps.InputString with
            | Regex2(p)  mt -> 
                let (c, i) = mt.ge2
                Some(indent  i, (ps.SetRestString mt.Rest).SetChomping (chomp c))
            |   _ -> None
        match (ps) with
        |   Eval(``indent chomp``)  value -> value
        |   Eval(``chomp indent``)  value -> value
        |   _ ->    (None, ps.SetChomping ``Clip``)

    //  [163]   http://www.yaml.org/spec/1.2/spec.html#c-indentation-indicator(m)
    member this.``c-indentation-indicator`` = OPT(this.``ns-dec-digit``)

    //  [164]   http://www.yaml.org/spec/1.2/spec.html#c-chomping-indicator(t)
    member this.``c-chomping-indicator`` = OPT(RGP("\\+") ||| RGP("-"))

    //  [165]   http://www.yaml.org/spec/1.2/spec.html#b-chomped-last(t)
    member this.``b-chomped-last`` ps =
        logger.Trace  "b-chomped-last"
        match ps.t with
        |   ``Strip``   -> this.``b-non-content``    ||| RGP("\\z")
        |   ``Clip``    -> this.``b-as-line-feed``   ||| RGP("\\z")
        |   ``Keep``    -> this.``b-as-line-feed``   ||| RGP("\\z")

    //  [166]   http://www.yaml.org/spec/1.2/spec.html#l-chomped-empty(n,t)
    member this.``l-chomped-empty`` (ps:ParseState) =
        logger.Trace  "l-chomped-empty"
        match ps.t with
        |   ``Strip``   -> this.``l-strip-empty`` ps
        |   ``Clip``    -> this.``l-strip-empty`` ps
        |   ``Keep``    -> this.``l-keep-empty`` ps

    //  [167]   http://www.yaml.org/spec/1.2/spec.html#l-strip-empty(n)
    member this.``l-strip-empty`` ps = ZOM((this.``s-indent(<=n)`` ps) + this.``b-non-content``) + OPT(this.``l-trail-comments`` ps)

    //  [168]   http://www.yaml.org/spec/1.2/spec.html#l-keep-empty(n)
    member this.``l-keep-empty`` ps = ZOM(this.``l-empty`` (ps.SetStyleContext ``Block-in``)) + OPT(this.``l-trail-comments`` ps)

    //  [169]   http://www.yaml.org/spec/1.2/spec.html#l-trail-comments(n)
    member this.``l-trail-comments`` ps = (this.``s-indent(<n)`` ps) + this.``c-nb-comment-text`` + this.``b-comment``

    //  [170]   http://www.yaml.org/spec/1.2/spec.html#c-l+literal(n)
    member this.``c-l+literal`` ps = 
        logger.Trace  "c-l+literal"
        let trimIndent n (slst: string list) =
            let rec processlLine n l frst (src: string list) dst =
                let join sl = String.Join("\n", sl |> List.rev)
                let cont v =
                    if src.Length > 0 then processlLine n (src.Head) false (src.Tail) (v :: dst)
                    else join (v :: dst)
                let ws2 = GRP(ZOM(RGP this.``s-space``)) + GRP(ZOM(this.``nb-char`` - RGO(this.``s-space``)))
                match l with
                |   Regex2(ws2) mt -> 
                    let (w, c) = mt.ge2
                    if frst then
                        if w.Length > n then 
                            raise (ParseException "A leading all-space line must not have too many spaces")
                        else
                            if c = "" then cont ""
                            else cont (l.Substring(n))
                    else
                        if w.Length < n then 
                            if w = "" && c = "" then cont ""
                            else raise (ParseException "A following text line must not be less indented.")
                        else cont (l.Substring(n))
                | _ -> raise (ParseException (sprintf "Unexpected characters, at %s" l))
            
            processlLine n (slst.Head) true (slst.Tail) []

        match (HasMatches(ps.InputString, RGP "\\|")) with
        | (false, _, _)   ->  None
        | (true, mt, frs) ->
            let ``literal-content`` (ps:ParseState) =
                let p = this.``l-literal-content`` ps
                match ps.InputString  with
                |   Regex2(p)  m -> Some(m.ge1, ps.SetRestString m.Rest)
                |   _ -> None

            let prs = ps.SetRestString frs
            let (pm, prs2) = (this.``c-b-block-header`` prs)

            let m = match pm with
                    |   Some(m) -> m
                    |   None    ->
                    match (``literal-content`` prs2) with
                    |   Some(ms, prs3) ->  
                        let split = ms |> this.``split by linefeed`` 
                        let aut = split |> this.``auto detect indent in block`` prs2.n
                        if aut < 0 then raise (ParseException "Lesser indented than expected")
                        aut
                    |   None  -> raise (ParseException "Could not detect indentation of literal block scalar after '|'")

            match (``literal-content`` (prs2.SetIndent (prs2.n+m))) with
            |   Some(ms, ps2) ->  
                let split = ms |> this.``split by linefeed`` 
                let s = split |> trimIndent (ps2.n)
                Some(s, ps2)
            |   None  -> None

    //  [171]   http://www.yaml.org/spec/1.2/spec.html#l-nb-literal-text(n)
    member this.``l-nb-literal-text`` (ps:ParseState) = ZOM(this.``l-empty`` (ps.SetStyleContext ``Block-in``)) + (this.``s-indent(n)`` ps) + OOM(this.``nb-char``)

    //  [172]   http://www.yaml.org/spec/1.2/spec.html#b-nb-literal-next(n)
    member this.``b-nb-literal-next`` ps = this.``b-as-line-feed`` + (this.``l-nb-literal-text`` ps)
    
    //  [173]   http://www.yaml.org/spec/1.2/spec.html#l-literal-content(n,t)
    member this.``l-literal-content`` (ps:ParseState) = 
        GRP(OPT((this.``l-nb-literal-text`` ps) + ZOM(this.``b-nb-literal-next`` ps) + (this.``b-chomped-last`` ps))) + (this.``l-chomped-empty`` ps)

    //  [174]   http://www.yaml.org/spec/1.2/spec.html#c-l+folded(n)
    member this.``c-l+folded`` ps =
        logger.Trace  "c-l+folded"
        let foldLines ps (strlst: string list) =
            let ws2 = GRP(ZOM(this.``s-white``)) + GRP(ZOM(this.``ns-char``))
            let rec doFold prev (res:StringBuilder)  (lst: string list) =
                match lst with
                |   []  -> 
                    match prev with
                    | Empty | EmptyAfterFolded   -> res.ToString()
                    | _ -> if res.ToString().EndsWith("\n") then res.Remove(res.Length-1,1).ToString() else res.ToString()
                |   curr :: tail ->
                    match curr with
                    |   Regex2(ws2) mt -> 
                        let standardContentAppedAndContinue mode (res:StringBuilder) = doFold mode (res.Append(curr.Substring(ps.n)).Append("\n")) tail
                        logger.Trace(sprintf "%s" (res.ToString().Replace("\n","\\n")))
                        let (w, c) = mt.ge2
                        if c = "" then
                            if w.Length > ps.n then standardContentAppedAndContinue Indented res
                            else
                                match prev with
                                | Empty | Indented | EmptyAfterFolded  -> doFold Empty (res.Append("\n")) tail
                                | TextLine          -> doFold EmptyAfterFolded (res)   tail
                        else
                            if w.Length < ps.n then raise (ParseException(sprintf "Incorrect indentation at: '%s'" curr))
                            else 
                                if w.Length > ps.n then
                                    match prev with
                                    | Empty | Indented | TextLine          -> standardContentAppedAndContinue Indented res
                                    | EmptyAfterFolded  -> standardContentAppedAndContinue Indented (res.Append("\n"))
                                else    //  w.Length = n 
                                    match prev with
                                    | Empty | Indented | EmptyAfterFolded  -> standardContentAppedAndContinue TextLine res
                                    | TextLine          ->
                                        let res = if res.ToString().EndsWith("\n") then res.Remove(res.Length-1,1).Append(" ") else res
                                        doFold TextLine (res.Append(curr.Substring(ps.n)).Append("\n") ) tail
                    |   _ -> raise (ParseException(sprintf "Incorrect pattern: '%s'" curr))
            let stripAll lst = lst |> List.rev |> List.skipWhile(fun s -> String.IsNullOrWhiteSpace(s)) |> List.rev
            match ps.t with
            | ``Strip`` -> strlst |> stripAll |> doFold Empty (new StringBuilder())
            | ``Clip``  -> if (String.IsNullOrWhiteSpace(List.last strlst)) then 
                                List.append (strlst |> stripAll) [""] |> doFold Empty (new StringBuilder())
                           else 
                                strlst |> doFold Empty (new StringBuilder())
            | ``Keep``  -> strlst |> doFold Empty (new StringBuilder())

        match (HasMatches(ps.InputString, RGP ">")) with
        | (false, _, _)   ->  None
        | (true, mt, frs) ->
            let ``folded-content`` (ps:ParseState) =
                let p = this.``l-folded-content`` (ps.FullIndented)
                match ps.InputString  with
                |   Regex2(p)  m -> Some(m.ge1, ps.SetRestString m.Rest)
                |   _ -> None
            
            let prs = ps.SetRestString frs
            let (pm, prs2) = (this.``c-b-block-header`` prs)

            let m = match pm with
                    |   Some(m) -> m
                    |   None    ->
                    match (``folded-content`` prs2) with
                    |   Some(ms, prs3) ->  
                        let split = ms |> this.``split by linefeed`` 
                        let aut = split |> this.``auto detect indent in block`` prs2.n
                        if aut < 0 then raise (ParseException "Lesser indented than expected")
                        aut
                    |   None  -> raise (ParseException "Could not detect indentation of literal block scalar after '>'")

            match (``folded-content`` (prs2.SetIndent (prs2.n+m))) with
            |   Some(ms, ps2) ->  
                let split = ms |> this.``split by linefeed`` 
                let s = split |> foldLines (ps2)
                Some(s, ps2)
            |   None  -> None


    //  [175]   http://www.yaml.org/spec/1.2/spec.html#s-nb-folded-text(n)
    member this.``s-nb-folded-text`` ps = (this.``s-indent(n)`` ps) + ZOM(this.``nb-char``)

    //  [176]   http://www.yaml.org/spec/1.2/spec.html#l-nb-folded-lines(n)
    member this.``l-nb-folded-lines`` (ps:ParseState) = (this.``s-nb-folded-text`` ps) + ZOM((this.``b-l-folded`` (ps.SetStyleContext ``Block-in``)) + this.``s-nb-folded-text`` ps)

    //  [177]   http://www.yaml.org/spec/1.2/spec.html#s-nb-spaced-text(n)
    member this.``s-nb-spaced-text`` ps = (this.``s-indent(n)`` ps) + this.``s-white`` + ZOM(this.``nb-char``)

    //  [178]   http://www.yaml.org/spec/1.2/spec.html#b-l-spaced(n)
    member this.``b-l-spaced`` (ps:ParseState) = this.``b-as-line-feed`` + ZOM(this.``l-empty`` (ps.SetStyleContext ``Block-in``))

    //  [179]   http://www.yaml.org/spec/1.2/spec.html#l-nb-spaced-lines(n)
    member this.``l-nb-spaced-lines`` ps = (this.``s-nb-spaced-text`` ps) + ZOM((this.``b-l-spaced``ps) + (this.``s-nb-spaced-text`` ps))

    //  [180]   http://www.yaml.org/spec/1.2/spec.html#l-nb-same-lines(n)
    member this.``l-nb-same-lines`` (ps:ParseState) = 
        ZOM(this.``l-empty`` (ps.SetStyleContext ``Block-in``)) + ((this.``l-nb-folded-lines`` ps) ||| (this.``l-nb-spaced-lines`` ps))

    //  [181]   http://www.yaml.org/spec/1.2/spec.html#l-nb-diff-lines(n)
    member this.``l-nb-diff-lines`` ps = (this.``l-nb-same-lines`` ps) + ZOM(this.``b-as-line-feed`` + (this.``l-nb-same-lines`` ps))

    //  [182]   http://www.yaml.org/spec/1.2/spec.html#l-folded-content(n,t)
    member this.``l-folded-content`` (ps:ParseState) =
        GRP(OPT((this.``l-nb-diff-lines`` ps) + (this.``b-chomped-last`` ps))) + (this.``l-chomped-empty`` ps)

    //  [183]   http://www.yaml.org/spec/1.2/spec.html#l+block-sequence(n)
    member this.``l+block-sequence`` (ps:ParseState) = 
        logger.Trace  "l+block-sequence"
        let m = this.``auto detect indent in line`` ps
        if m < 1 then None // raise (ParseException "Incorrect indentation")
        else
            let ps = ps.SetSubIndent m
            let rec ``l+block-sequence`` ps (acc: Node list) =
                let contentOrNone = 
                    if (acc.Length = 0) then None
                    else Some(CreateSeqNode (List.rev acc), ps)
                match HasMatches(ps.InputString, RGS((this.``s-indent(n)`` (ps.FullIndented)))) with
                |   (true, mt, frs) -> 
                    let prs = ps.SetRestString frs
                    match (this.``c-l-block-seq-entry`` ps.FullIndented) with
                    |   Some(c, prs2) -> ``l+block-sequence`` (prs2.InputString  |> ps.SetRestString) (c :: acc)
                    |   _ -> contentOrNone
                |   (false, _, _) -> contentOrNone 
            ``l+block-sequence`` ps []

    //  [184]   http://www.yaml.org/spec/1.2/spec.html#c-l-block-seq-entry(n)
    member this.``c-l-block-seq-entry`` ps =
        logger.Trace  "c-l-block-seq-entry"
        match (HasMatches(ps.InputString, RGS(RGP("-")))) with
        |   (true, mt, frs) -> 
            let prs = ps.SetRestString frs
            let prs = prs.SetStyleContext ``Block-in``
            this.``s-l+block-indented`` prs
        |   (false, _, _) -> None

    //  [185]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-indented(n,c)
    member this.``s-l+block-indented`` ps =
        logger.Trace  "s-l+block-indented"
        match HasMatches(ps.InputString, RGS((this.``s-indent(n)`` (ps.SetIndent ps.m)))) with
        |   (true, mt, frs) -> 
            let prs = ps.SetRestString frs
            let prs = prs.SetIndent (prs.n+1+prs.m)
            match (prs) with
            |   Parse(this.``ns-l-compact-sequence``)   (c, prs2) -> Some(c, prs2)
            |   Parse(this.``ns-l-compact-mapping``)    (c, prs2) -> Some(c, prs2)
            |   Parse(this.``s-l+block-node``)      (c, prs2) -> Some(c, prs2)
            |   _ -> 
                let prs2 = prs.SkipIfMatch (this.``e-node`` + this.``s-l-comments``)
                Some(NullScalarNode , prs2)
        |   (false, _, _) -> None

    //  [186]   http://www.yaml.org/spec/1.2/spec.html#ns-l-compact-sequence(n)
    member this.``ns-l-compact-sequence`` ps = 
        logger.Trace  "ns-l-compact-sequence"
        let rec ``ns-l-compact-sequence`` ps (acc: Node list) =
            let contentOrNone = 
                if (acc.Length = 0) then None
                else Some(CreateSeqNode (List.rev acc), ps)
            match HasMatches(ps.InputString, RGS((this.``s-indent(n)`` ps))) with
            |   (true, mt, frs) -> 
                let prs = ps.SetRestString frs
                match (this.``c-l-block-seq-entry`` prs) with
                |   Some(c, prs2) -> ``ns-l-compact-sequence`` prs2 (c :: acc)
                |   _ -> contentOrNone
            |   (false, _, _) -> contentOrNone
        match ps with
        |   Parse(this.``c-l-block-seq-entry``) (c, prs) -> ``ns-l-compact-sequence`` prs [c]
        |   _ ->    None

    //  [187]   http://www.yaml.org/spec/1.2/spec.html#l+block-mapping(n)
    member this.``l+block-mapping`` ps =
        logger.Trace  "l+block-mapping"
        let m = this.``auto detect indent in line`` ps
        if m < 1 then None // raise (ParseException "Incorrect indentation")
        else
            let ps = ps.SetSubIndent m
            let rec ``l+block-mapping`` ps (acc:(Node*Node) list) = 
                let contentOrNone = 
                    if (acc.Length = 0) then None
                    else Some(CreateMapNode (List.rev acc), ps)
                match (HasMatches(ps.InputString, RGS((this.``s-indent(n)`` (ps.FullIndented))))) with
                |   (true, mt, frs) -> 
                    match (this.``ns-l-block-map-entry`` (ps.FullIndented)) with
                    |   Some(c, prs)    ->  ``l+block-mapping`` prs (c :: acc)
                    |   None            ->  contentOrNone
                |   (false, _, _) -> contentOrNone
            ``l+block-mapping`` ps []

    //  [188]   http://www.yaml.org/spec/1.2/spec.html#ns-l-block-map-entry(n)
    member this.``ns-l-block-map-entry`` ps = 
        logger.Trace  "ns-l-block-map-entry"
        match ps with
        |   Parse(this.``c-l-block-map-explicit-entry``)    (ck, cv, prs1) -> Some((ck, cv), prs1)
        |   Parse(this.``ns-l-block-map-implicit-entry``)   (ck, cv, prs1) -> Some((ck, cv), prs1)
        |   _    ->  None

    //  [189]   http://www.yaml.org/spec/1.2/spec.html#c-l-block-map-explicit-entry(n)
    member this.``c-l-block-map-explicit-entry`` ps : ParseFuncMappedResult=
        logger.Trace  "c-l-block-map-explicit-entry"
        match (this.``c-l-block-map-explicit-key`` ps) with
        |   Some(ck, prs1) ->
            match (prs1) with
            |   Parse(this.``l-block-map-explicit-value``) (cv, prs1) -> Some(ck, cv, prs1)
            |   _   ->
                match HasMatches(prs1.InputString, RGS(this.``e-node``)) with
                |   (true, mt, frs) -> Some(ck, NullScalarNode, prs1.SetRestString frs)
                |   _ -> raise (ParseException "Cannot identify mapping value")
        |   None -> None

    //  [190]   http://www.yaml.org/spec/1.2/spec.html#c-l-block-map-explicit-key(n)
    member this.``c-l-block-map-explicit-key`` ps : ParseFuncSingleResult =
        logger.Trace  "c-l-block-map-explicit-key"
        match HasMatches(ps.InputString, RGS(RGP("\\?"))) with
        |   (true, mt, frs) -> 
            let prs = ps.SetRestString frs
            let prs = prs.SetStyleContext ``Block-out``
            this.``s-l+block-indented`` prs
        |   (false, _, _) -> None

    //  [191]   http://www.yaml.org/spec/1.2/spec.html#l-block-map-explicit-value(n)
    member this.``l-block-map-explicit-value`` ps = 
        logger.Trace  "l-block-map-explicit-value"
        match HasMatches(ps.InputString, RGS((this.``s-indent(n)`` ps) + RGP(":"))) with
        |   (true, mt, frs) -> 
            let prs = ps.SetRestString frs
            let prs = prs.SetStyleContext ``Block-out``
            this.``s-l+block-indented`` prs
        |   (false, _, _) -> None

    //  [192]   http://www.yaml.org/spec/1.2/spec.html#ns-l-block-map-implicit-entry(n)
    member this.``ns-l-block-map-implicit-entry`` ps : ParseFuncMappedResult =
        logger.Trace  "ns-l-block-map-implicit-entry"
        let matchValue (ck, prs) =
            match prs with
            |   Parse(this.``c-l-block-map-implicit-value``) (cv, prs2) -> Some(ck, cv, prs2)
            |   _ ->  None //  raise (ParseException "Expecting an implicit mapping value")
        match (ps) with
        |   Parse(this.``ns-s-block-map-implicit-key``) (ck, prs1) -> matchValue(ck, prs1)
        |   _   ->
            match HasMatches(ps.InputString, RGS(this.``e-node``)) with
            |   (true, mt, frs) -> matchValue (NullScalarNode, (ps.SetRestString frs))
            |   _ -> None

    //  [193]   http://www.yaml.org/spec/1.2/spec.html#ns-s-block-map-implicit-key
    member this.``ns-s-block-map-implicit-key`` ps = 
        logger.Trace  "ns-s-block-map-implicit-key"
        let prs = ps.SetStyleContext ``Block-key``
        match prs with
        |   Parse(this.``c-s-implicit-json-key``)   v -> Some(v)
        |   Parse(this.``ns-s-implicit-yaml-key``)  v -> Some(v)
        |   _ -> None

    //  [194]   http://www.yaml.org/spec/1.2/spec.html#c-l-block-map-implicit-value(n)
    member this.``c-l-block-map-implicit-value`` (ps:ParseState) : ParseFuncSingleResult=
        logger.Trace  "c-l-block-map-implicit-value"
        match (HasMatches(ps.InputString, RGS(RGP(":")))) with
        |   (true, mt, frs) -> 
            let prs = ps.SetRestString frs
            let prs = prs.SetStyleContext ``Block-out``
            match (prs) with
            |   Parse(this.``s-l+block-node``)  (c, prs2) -> Some(c, prs2)
            |   _ ->
                match (HasMatches(ps.InputString, RGS((this.``e-node`` +  this.``s-l-comments``)))) with
                |   (true, mt, frs2) -> Some(NullScalarNode, prs.SetRestString frs2)
                |   (false, _, _) -> None
        |   (false, _, _) -> None

    //  [195]   http://www.yaml.org/spec/1.2/spec.html#ns-l-compact-mapping(n)
    member this.``ns-l-compact-mapping`` ps =
        logger.Trace  "ns-l-compact-mapping"
        let rec ``ns-l-compact-mapping`` ps (acc: (Node * Node) list) =
            let contentOrNone = 
                if (acc.Length = 0) then None
                else Some(CreateMapNode (List.rev acc), ps)
            match HasMatches(ps.InputString, RGS((this.``s-indent(n)`` ps))) with
            |   (true, mt, frs) -> 
                let prs = ps.SetRestString frs
                match (this.``ns-l-block-map-entry`` prs) with
                |   Some(c, prs2) -> ``ns-l-compact-mapping`` prs2 (c :: acc)
                |   _ -> contentOrNone
            |   (false, _, _) -> contentOrNone
        match ps with
        |   Parse(this.``ns-l-block-map-entry``) (c, prs) -> ``ns-l-compact-mapping`` prs [c]
        |   _ ->    None

    //  [196]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-node(n,c)
    member this.``s-l+block-node`` ps : ParseFuncSingleResult =
        logger.Trace  "s-l+block-node"
        match (ps) with
        |   Parse(this.``s-l+block-in-block``) value -> Some(value)
        |   Parse(this.``s-l+flow-in-block``)  value -> Some(value)
        |   _ -> None

    //  [197]   http://www.yaml.org/spec/1.2/spec.html#s-l+flow-in-block(n)
    member this.``s-l+flow-in-block`` (ps:ParseState) : ParseFuncSingleResult =
        logger.Trace  "s-l+flow-in-block"
        let prs = ps.SetIndent (ps.n + 1)
        let prs = prs.SetStyleContext ``Flow-out``
        match (HasMatches(prs.InputString, RGS(this.``s-separate`` prs))) with
        |   (true, mt, frs) -> 
            let prs = prs.SetRestString frs
            match (prs) with
            |   Parse(this.``ns-flow-node``) (c, prs2) -> 
                match (HasMatches(prs2.InputString, RGS(this.``s-l-comments``))) with
                |   (true, mt, frs2) -> Some(c, prs2.SetRestString frs2)
                |   (false, _, _) -> None
            |   _ -> None
        |   (false, _, _)   -> None

    //  [198]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-in-block(n,c)
    member this.``s-l+block-in-block`` ps : ParseFuncSingleResult =
        logger.Trace  "s-l+block-in-block"
        match (ps) with
        |   Parse(this.``s-l+block-scalar``)     value -> Some(value)
        |   Parse(this.``s-l+block-collection``) value -> Some(value)
        |   _ -> None

    //  [199]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-scalar(n,c)
    member this.``s-l+block-scalar`` ps : ParseFuncSingleResult =
        logger.Trace  "s-l+block-scalar"
        let psp1 = ps.SetIndent (ps.n + 1)
        let ``literal or folded`` ps =
            match ps with
            |   Parse(this.``c-l+literal``) (s, prs) -> Some(MapScalar(s), prs)
            |   Parse(this.``c-l+folded``)  (s, prs) -> Some(MapScalar(s), prs)
            |   _ -> None
        match HasMatches(psp1.InputString, RGS(this.``s-separate`` psp1)) with
        |   (true, mt, frs) -> 
            let prs = psp1.SetRestString frs
            match prs with
            |   Parse(this.``content with optional properties`` ``literal or folded``) value -> Some(value)
            |   _ -> None
        |   (false, _, _) -> None

    //  [200]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-collection(n,c)
    member this.``s-l+block-collection`` (ps:ParseState) : ParseFuncSingleResult =
        logger.Trace  "s-l+block-collection"
        let omit f d1 d2 = f d1
        let psp1 = ps.SetIndent (ps.n + 1)
        let ``seq or map`` (pls:ParseState) =
            let pls = pls.SkipIfMatch(this.``s-l-comments``) 
            let pls = pls.SetIndent (ps.n)
            match pls with
            |   Parse(omit this.``l+block-sequence`` (this.``seq-spaces`` pls)) value -> Some(value)
            |   Parse(this.``l+block-mapping``)                                value -> Some(value)
            |   _ -> None
            
        match HasMatches(psp1.InputString, RGS(this.``s-separate`` psp1)) with
        |   (true, mt, frs) -> 
            let prs = psp1.SetRestString frs
            match prs with
            |   Parse(this.``content with optional properties`` ``seq or map``) value -> Some(value)
            |   _ -> None
        |   (false, _, _) -> None

    //  [201]   http://www.yaml.org/spec/1.2/spec.html#seq-spaces(n,c)
    member this.``seq-spaces`` ps = 
        match ps.c with
        |   ``Block-out``   ->  ps.SetIndent (ps.n-1)
        |   ``Block-in``    ->  ps
        |   _ ->    raise (ParseException "Case is not supported")

