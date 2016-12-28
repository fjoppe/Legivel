open System
open System.Text
open System.IO
open System.Text.RegularExpressions

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
        let not = if this.not then "^" else ""
        if this.subtractset <> "" then
            sprintf "[%s%s-[%s]]" (not) (this.mainset) (this.subtractset)
        else
            sprintf "[%s%s]" (not) (this.mainset)
    static member (-) (r1:OneInSet, r2:OneInSet) =
        {mainset = r1.mainset; subtractset = r1.subtractset + r2.mainset; not = r1.not}
    static member (-) (r1:OneInSet, r2:Plain) =
        {mainset = r1.mainset; subtractset = r2.``fixed``; not = r1.not}
    static member (+) (r1:OneInSet, r2:OneInSet) =
        {mainset = r1.mainset + r2.mainset; subtractset = r1.subtractset; not = r1.not}
    static member (+) (r1:OneInSet, r2:Plain) =
        {mainset = r1.mainset + r2.``fixed``; subtractset = r1.subtractset; not = r1.not}
    static member Create r =
         {mainset= r; subtractset = ""; not = false}
    member this.Not = 
        {this with not = true}

type RGXType =
    |   Plain of Plain
    |   OneInSet  of OneInSet
    |   Or        of RGXType list
    |   Concat    of RGXType list
    |   IterRange of RGXType * int * (int option)
    |   ZeroOrMore of RGXType
    |   OneOrMore  of RGXType
    |   Optional   of RGXType
    override this.ToString() =
        match this with
        |   Plain    r -> r.ToString()
        |   OneInSet r ->r.ToString()
        |   Or       l ->
                let l = l |> List.rev
                let body = l.Tail |> List.fold(fun s e -> sprintf "%s|%O" s e) (sprintf "%O" l.Head)
                sprintf "(%s)" body
        |   Concat   l ->
                let l = l |> List.rev
                l.Tail |> List.fold(fun s e -> sprintf "%s%O" s e) (sprintf "%O" l.Head)
        |   IterRange(t,mx,mno) ->
                match mno with
                |   Some(mn) ->  sprintf "(%O){%d,%d}" t mn mx
                |   None     ->  sprintf "(%O){%d}" t mx
        |   ZeroOrMore t -> sprintf "(%O)*" t
        |   OneOrMore  t -> sprintf "(%O)+" t
        |   Optional   t -> sprintf "(%O)?" t
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
        |   (OneInSet o1,    Plain p1)   -> OneInSet(o1 + p1)
        |   _   ->  RGXType.DoConcat(r1, r2)

    member this.Not = 
        match this with
        |   OneInSet o1 ->  OneInSet(o1.Not)
        |   _   -> raise (ParseException "Cannot invert this")

let Repeat(t, mx) = IterRange(t, mx, None)
let Range(t, mn, mx) = IterRange(t, mx, Some(mn))
let ZOM(t) = ZeroOrMore(t)
let OOM(t) = OneOrMore(t)
let OPT(t) = Optional(t)
let RGP c = Plain(Plain.Create c)
let RGO c = OneInSet(OneInSet.Create c)
let NOT (c:RGXType) = c.Not

let RGC p = sprintf "^%O" p

let Advance(m : string, s : string) =  s.Substring(m.Length)
let Match(s, p) = [ for g in (Regex.Matches(s, RGC(p))) -> g.Value ] |> List.tail
let HasMatches(s,p) = 
    let ml = Match(s, p)
    if ml.Length > 0 then
        let m0 = ml.[0]
        (true, m0, Advance(m0, s))
    else
        (false, "",s)

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None


type Context = ``Block-out`` | ``Block-in`` | ``Flow-out`` | ``Flow-in`` | ``Block-key`` | ``Flow-key``

let ``start-of-line`` = RGP "^.?"

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


        member this.Canonical s = this.canonFn s
        

type NodeData<'T> = {
        Tag  : Tag
        Data : 'T
    }
    with
        static member Create t d =
            { Tag = t; Data = d}

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
                let head = sprintf "%s%s\n [" (ind0) (n.Tag.Short)
                let content = n.Data |> List.fold(fun s ni -> s + (sprintf "%s," (ni.ToCanonical(l+1)))) ""
                let tail = sprintf "%s]\n" ind0
                sprintf "%s%s%s" head content tail
            |   MapNode n -> 
                let ind0 = this.Indent l
                let head = sprintf "%s%s {\n" (ind0) (n.Tag.Short)
                let content = 
                    n.Data 
                    |> List.fold(
                        fun s (k,v) -> 
                            let kc = k.ToCanonical(l+1)
                            let vc = v.ToCanonical(l+1)
                            s + sprintf "? %s\n: %s," kc vc
                        ) ""
                let tail = sprintf "%s}\n" ind0
                sprintf "%s%s%s" head content tail
            |   ScalarNode n ->
                let ind0 = this.Indent l
                sprintf "%s%s \"%s\"\n" ind0 (n.Tag.Short) (n.Data)

type Legend = {
        YamlVersion : string
    }

//    Failsafe schema:  http://www.yaml.org/spec/1.2/spec.html#id2802346
let MappingGlobalTag =  Tag.Create(Global, Mapping, "tag:yaml.org,2002:map", "!!map")
let SequenceGlobalTag =  Tag.Create(Global, Sequence, "tag:yaml.org,2002:seq", "!!seq")
let StringGlobalTag = Tag.Create(Global, Scalar, "tag:yaml.org,2002:str", "!!str")
 
//    Json schema:  http://www.yaml.org/spec/1.2/spec.html#id2803231
let NullGlobalTag =
    Tag.Create(Global, Scalar, "tag:yaml.org,2002:null", "!!bool", "~|null|Null|NULL",
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

//    IntegerGlobalTag.Canonical "5"
//    IntegerGlobalTag.Canonical "0b101"
//    IntegerGlobalTag.Canonical "017"
//    IntegerGlobalTag.Canonical "0x12"
//    IntegerGlobalTag.Canonical "190:20:30"


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



//  Character set:  http://www.yaml.org/spec/1.2/spec.html#id2770814
let ``c-printable`` = 
    RGO ("\u0009\u000a\u000d\u0020-\u007e" +   // 8 - bit, #x9 | #xA | #xD | [#x20-#x7E]
         "\u0085\u00a0-\ud7ff\ue000-\ufffd")   // 16- bit, #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD]
                                               //  32-bit -> currently not supported because .Net does not encode naturally. Yaml: [#x10000-#x10FFFF]
let ``nb-json`` = RGO "\u0009\u0020-\uffff"

//  Indicator Characters:   http://www.yaml.org/spec/1.2/spec.html#id2772075
let ``c-sequence-entry`` = RGP "-"
let ``c-mapping-key`` = RGP "?"
let ``c-mapping-value`` = RGP ":"
let ``c-collect-entry`` = RGP ","
let ``c-sequence-start`` = RGP "["
let ``c-sequence-end`` = RGP "]"
let ``c-mapping-start`` = RGP "{"
let ``c-mapping-end`` = RGP "}"
let ``c-comment`` = RGP "#"
let ``c-anchor`` = RGP "#"
let ``c-alias`` = RGP "*"
let ``c-tag`` = RGP "!"
let ``c-literal`` = RGP "|"
let ``c-folded`` = RGP ">"
let ``c-single-quote`` = RGP "\""
let ``c-double-quote`` = RGP "\""
let ``c-directive`` = RGP "%"
let ``c-reserved`` = RGO "\u0040\u0060"
let ``c-indicator`` = RGO  @"\-?:,\[\]\{\}#\&\*!;>\'""%@`"
let ``c-flow-indicator`` = RGO  @",\[\]\{\}"


//  Line break characters:  http://www.yaml.org/spec/1.2/spec.html#id2774608
let  ``b-line-feed`` = RGP "\u000a"
let  ``b-carriage-return`` = RGP "\u000d"
let ``b-char`` = ``b-line-feed`` + ``b-carriage-return``
let ``nb-char``  = ``c-printable`` - ``b-char`` //  https://msdn.microsoft.com/en-us/library/20bw873z(v=vs.110).aspx#Anchor_13

let ``b-break`` = 
        (``b-carriage-return`` + ``b-line-feed``)  |||  //  DOS, Windows
        ``b-carriage-return``                      |||  //  MacOS upto 9.x
        ``b-line-feed``                                 //  UNIX, MacOS X


let ``b-as-line-feed`` = ``b-break``
let ``b-non-content`` = ``b-break``


//  White space characters: http://www.yaml.org/spec/1.2/spec.html#id2775170
let  ``s-space`` = RGP "\u0020"  // space
let  ``s-tab`` = RGP "\u0009"    // tab
let ``s-white`` = (RGO "") + ``s-space`` + ``s-tab``
let ``ns-char`` = ``nb-char`` - ``s-white``


//  Misc characters:    http://www.yaml.org/spec/1.2/spec.html#id2775468
let ``ns-dec-digit`` = RGO "\u0030-\u0039"      //  0-9
let ``ns-hex-digit`` =
        ``ns-dec-digit`` +
        RGO "\u0041-\u0046"  +  //  A-F
        RGO "\u0061-\u0066"     //  a-f

let ``ns-ascii-letter`` = 
    RGO "\u0041-\u005A" +   //  A-Z
    RGO "\u0061-\u007A"     //  a-z

let ``ns-word-char`` =
    ``ns-dec-digit`` + (RGO @"\-") + ``ns-ascii-letter``

let ``ns-uri-char`` = 
    (RGP @"%") + ``ns-hex-digit`` + ``ns-hex-digit``  |||
    (RGO @"#;/?:@&=+$,_.!~*\'\(\)\[\]") + ``ns-word-char``


let ``ns-tag-char`` = 
    (RGP @"%") + ``ns-hex-digit`` + ``ns-hex-digit``  |||
    (RGO @"#;/?:@&=+$_.~*\'\(\)") + ``ns-word-char``


//  Escaped characters: http://www.yaml.org/spec/1.2/spec.html#id2776092
let  ``c-escape`` = RGP "\\"
let  ``ns-esc-null`` = RGP "0"
let  ``ns-esc-bell`` = RGP "a"
let  ``ns-esc-backspace`` = RGP "b"
let  ``ns-esc-horizontal-tab`` = RGP "t"
let  ``ns-esc-line-feed`` = RGP "n"
let  ``ns-esc-vertical-tab`` = RGP "v"
let  ``ns-esc-form-feed`` = RGP "f"
let  ``ns-esc-carriage-return`` = RGP "r"
let  ``ns-esc-escape`` = RGP "e"
let  ``ns-esc-space`` = RGP "\u0020"
let  ``ns-esc-double-quote`` = RGP "\""
let  ``ns-esc-slash`` = RGP "/"
let  ``ns-esc-backslash`` = RGP "\\"
let  ``ns-esc-next-line`` = RGP "N"
let  ``ns-esc-non-breaking-space`` = RGP "_"
let  ``ns-esc-line-separator`` = RGP "L"
let  ``ns-esc-paragraph-separator`` = RGP "P"
let  ``ns-esc-8-bit`` = (RGP "x") + Repeat(``ns-hex-digit``,2)
let  ``ns-esc-16-bit`` = RGP "u" + Repeat(``ns-hex-digit``,4)
let  ``ns-esc-32-bit`` = RGP "U" + Repeat(``ns-hex-digit``,8) // currently not supported


let ``c-ns-esc-char`` = 
    RGP (@"\\") +
        (``ns-esc-null``             |||
         ``ns-esc-bell``             |||
         ``ns-esc-backspace``        |||
         ``ns-esc-horizontal-tab``   |||
         ``ns-esc-line-feed``        |||
         ``ns-esc-vertical-tab``     |||
         ``ns-esc-form-feed``        |||
         ``ns-esc-carriage-return``  |||
         ``ns-esc-escape``           |||
         ``ns-esc-space``            |||
         ``ns-esc-double-quote``     |||
         ``ns-esc-slash``            |||
         ``ns-esc-backslash``        |||
         ``ns-esc-next-line``        |||
         ``ns-esc-non-breaking-space``|||
         ``ns-esc-line-separator``   |||
         ``ns-esc-paragraph-separator``|||
         ``ns-esc-8-bit``            |||
         ``ns-esc-16-bit``           |||
         ``ns-esc-32-bit``)


//  Indentation spaces: http://www.yaml.org/spec/1.2/spec.html#id2777534
let ``s-indent(n)`` n = Repeat(``s-space``, n)
let ``s-indent(<n)`` n = Range(``s-space``, 0, (n-1))
let ``s-indent(<=n)`` n = Range(``s-space``,0,n)

//  Seperation spaces: http://www.yaml.org/spec/1.2/spec.html#id2778241
let ``s-separate-in-line`` = OOM(``s-white``) ||| ``start-of-line``

//  Line prefixes: http://www.yaml.org/spec/1.2/spec.html#id2778481
let ``s-block-line-prefix`` n = ``s-indent(n)`` n
let ``s-flow-line-prefix`` n = (``s-indent(n)`` n) + OPT(``s-separate-in-line``)


let ``s-line-prefix`` n c =
    match c with
    | ``Block-out`` ->  ``s-block-line-prefix`` n
    | ``Block-in``  ->  ``s-block-line-prefix`` n
    | ``Flow-out``  ->  ``s-flow-line-prefix`` n
    | ``Flow-in``   ->  ``s-flow-line-prefix`` n
    | _             ->  raise(ParseException "The context 'block-key' and 'flow-key' are not supported at this point")

//  Empty lines: http://www.yaml.org/spec/1.2/spec.html#id2778853
let ``l-empty`` n c = 
    ((``s-line-prefix`` n c) ||| (``s-indent(<n)`` n)) + ``b-as-line-feed``


//  Line Folding: http://www.yaml.org/spec/1.2/spec.html#id2779048
let ``b-l-trimmed`` n c = 
    ``b-non-content`` + OOM(``l-empty`` n c)

let ``b-as-space`` = ``b-break``

let ``b-l-folded`` n c =
    (``b-l-trimmed`` n c ) ||| ``b-as-space``

let ``s-flow-folded`` n =
    OPT(``s-separate-in-line``) + (``b-l-folded`` n ``Flow-in``) + (``s-flow-line-prefix`` n)


//  Comments:   http://www.yaml.org/spec/1.2/spec.html#id2780069
let ``c-nb-comment-text`` =
    RGP("#") + ZOM(``nb-char``)

let ``b-comment`` = ``b-non-content`` //    or EOF..

let ``s-b-comment`` =
    OPT(``s-separate-in-line`` + OPT(``c-nb-comment-text``)) + ``b-comment`` 

let ``l-comment`` = 
    ``s-separate-in-line`` + OPT(``c-nb-comment-text``) + ``b-comment``

let ``s-l-comments`` =
    (``s-b-comment`` ||| ``start-of-line``) + ``l-comment``


//  Seperation lines:   http://www.yaml.org/spec/1.2/spec.html#id2780810
let ``s-separate-lines`` n = 
    (``s-l-comments`` + (``s-flow-line-prefix`` n)) ||| ``s-separate-in-line``

let ``s-separate`` n c = 
    match c with 
    | ``Block-out`` ->  ``s-separate-lines`` n
    | ``Block-in``  ->  ``s-separate-lines`` n
    | ``Flow-out``  ->  ``s-separate-lines`` n
    | ``Flow-in``   ->  ``s-separate-lines`` n
    | ``Block-key`` ->  ``s-separate-in-line``
    | ``Flow-key``  ->  ``s-separate-in-line``


//  Directives: http://www.yaml.org/spec/1.2/spec.html#id2781147
let ``ns-directive-name`` = OOM(``ns-char``)

let ``ns-directive-parameter`` = OOM(``ns-char``)

let ``ns-reserved-directive`` = 
    ``ns-directive-name`` + ZOM(``s-separate-in-line`` + ``ns-directive-parameter``)

let ``ns-yaml-version`` = 
    OOM(``ns-dec-digit``) + RGP("\\.") + OOM(``ns-dec-digit``)

let ``ns-yaml-directive`` = 
    RGP("YAML") + ``s-separate-in-line`` + ``ns-yaml-version``


let ``c-primary-tag-handle`` = RGP "!"

let ``c-secondary-tag-handle`` = RGP "!!"

let ``c-named-tag-handle`` = 
    (RGP "!") + OOM(``ns-word-char``) + (RGP "!") 

let ``c-tag-handle`` =
    ``c-named-tag-handle`` ||| ``c-secondary-tag-handle`` ||| ``c-primary-tag-handle``

let ``c-ns-local-tag-prefix`` = 
    (RGP "!") + ZOM(``ns-uri-char``)

let ``ns-global-tag-prefix`` =
    ``ns-tag-char`` + ZOM(``ns-uri-char``)

let ``ns-tag-prefix`` = 
    ``c-ns-local-tag-prefix`` ||| ``ns-global-tag-prefix``

let ``ns-tag-directive`` = 
    (RGP "TAG") + ``s-separate-in-line`` + ``c-tag-handle`` + ``s-separate-in-line`` + ``ns-tag-prefix``

let ``l-directive`` = 
    (RGP "%") + (``ns-yaml-directive`` ||| ``ns-tag-directive`` ||| ``ns-reserved-directive``) + ``s-l-comments``

// Node Properties: http://www.yaml.org/spec/1.2/spec.html#id2783797
let ``c-verbatim-tag`` = 
    (RGP "!<") + OOM(``ns-uri-char``) + (RGP ">") 

let ``c-ns-shorthand-tag`` = 
    ``c-tag-handle`` + OOM(``ns-tag-char``)

let ``c-non-specific-tag`` = RGP "!"

let ``c-ns-tag-property`` =
    ``c-verbatim-tag`` ||| ``c-ns-shorthand-tag`` ||| ``c-non-specific-tag``

let ``ns-anchor-char`` = 
    ``ns-char`` - ``c-flow-indicator``

let ``ns-anchor-name`` = 
    OOM(``ns-anchor-char``)

let ``c-ns-anchor-property`` =
    (RGP "&") + ``ns-anchor-name``

let ``c-ns-properties`` n c=
    (``c-ns-tag-property``    + OPT((``s-separate`` n c) + ``c-ns-anchor-property``)) |||
    (``c-ns-anchor-property`` + OPT((``s-separate`` n c) + ``c-ns-tag-property``))


//  Flow Styles:    http://www.yaml.org/spec/1.2/spec.html#Flow
//  Alias Nodes:    http://www.yaml.org/spec/1.2/spec.html#id2786196
let ``c-ns-alias-node`` =
    (RGP "\\*") + ``ns-anchor-name``


//  Empty Nodes:    http://www.yaml.org/spec/1.2/spec.html#id2786563
let ``e-scalar`` = RGP String.Empty     // we'll see if this works..

let ``e-node`` = ``e-scalar``


//  Flow Scalar styles: http://www.yaml.org/spec/1.2/spec.html#id2786942
let ``nb-double-char`` = 
    ``c-ns-esc-char`` ||| (``nb-json`` - RGP("\\\\\""))

let ``ns-double-char`` =
    ``c-ns-esc-char`` |||  ``nb-json`` - ``s-white`` 

let ``nb-double-one-line`` =
    ZOM(``nb-double-char``)

let ``s-double-escaped`` n =
    ZOM(``s-white``) + (RGP "\\\\") + ``b-non-content`` + ZOM(``l-empty`` n ``Flow-in``) + (``s-flow-line-prefix`` n)

let ``s-double-break`` n =
    (``s-double-escaped`` n) ||| (``s-flow-folded`` n)

let ``nb-ns-double-in-line`` =
    ZOM(``s-white``) + ``ns-double-char``

let ``s-double-next-line`` n =  //  note, spec is recursive, the below is an attempt to rewrite that / may require some extra attention
    (``s-double-break`` n) + OOM(``ns-double-char`` + ``nb-ns-double-in-line``) ||| ZOM(``s-white``)

let ``nb-double-multi-line`` n =
    ``nb-ns-double-in-line`` + ((``s-double-next-line`` n) ||| ``s-white``)

let ``nb-double-text`` n c =
    match c with
    | ``Flow-out``  ->  ``nb-double-multi-line`` n
    | ``Flow-in``   ->  ``nb-double-multi-line`` n
    | ``Block-key`` ->  ``nb-double-one-line``
    | ``Flow-key``  ->  ``nb-double-one-line``
    | _             ->  raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")

let ``c-double-quoted`` n c =
    (RGP "\"") + (``nb-double-text`` n c ) + (RGP "\"")


//  Single Quote Style:   http://www.yaml.org/spec/1.2/spec.html#id2788097
let ``c-quoted-quote`` = RGP "\'\'"

let ``nb-single-char`` = 
    ``c-quoted-quote`` ||| (``nb-json`` - (RGP "\'"))

let ``ns-single-char`` =
    ``c-quoted-quote`` ||| (``nb-json`` - (RGP "\'") - ``s-white``) //  ns-single-char	::=	nb-single-char - s-white

let ``nb-single-one-line`` =
    ZOM(``nb-single-char``)

let ``nb-ns-single-in-line`` =
    ZOM(ZOM(``s-white``) + ``ns-single-char``)

let ``s-single-next-line`` n =
    OPT((``s-flow-folded`` n) + ZOM(``ns-single-char`` + ``nb-ns-single-in-line``) + ZOM(``s-white``))

let ``nb-single-multi-line`` n =
    ``nb-ns-single-in-line`` + ((``s-single-next-line`` n) ||| ZOM(``s-white``))

let ``nb-single-text`` n c =
    match c with
    |   ``Flow-out``    -> ``nb-single-multi-line`` n
    |   ``Flow-in``     -> ``nb-single-multi-line`` n
    |   ``Block-key``   -> ``nb-single-one-line``
    |   ``Flow-key``    -> ``nb-single-one-line``
    | _             ->  raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")

let ``c-single-quoted`` n c =
    (RGP "\'") + (``nb-single-text`` n c) + (RGP "\'")


//  Plain Style:    http://www.yaml.org/spec/1.2/spec.html#id2788859
let ``ns-plain-safe-out``   =   ``ns-char``

let ``ns-plain-safe-in`` =
    ``ns-char`` - ``c-flow-indicator``

let ``ns-plain-safe`` c =
    match c with
    |   ``Flow-out``    -> ``ns-plain-safe-out``
    |   ``Flow-in``     -> ``ns-plain-safe-in``
    |   ``Block-key``   -> ``ns-plain-safe-out``
    |   ``Flow-key``    -> ``ns-plain-safe-in``
    | _             ->  raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")

let ``ns-plain-char`` c = 
    (``ns-plain-safe`` c) - (RGP ":#") ||| (``ns-char`` + (RGP "#")) ||| (RGP ":") + (``ns-plain-safe`` c)

let ``ns-plain-first`` c =
    (``ns-char`` - ``c-indicator``) ||| (RGO "\\?:\\-") + (``ns-plain-safe`` c)


let ``in-flow`` c =
    match c with
    |   ``Flow-out`` -> ``Flow-in``
    |   ``Flow-in``  -> ``Flow-in``
    |   ``Block-key``-> ``Flow-key``
    |   ``Flow-key`` -> ``Flow-key``
    | _              -> raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")


let ``nb-ns-plain-in-line`` c =
    ZOM(ZOM(``s-white``) + (``ns-plain-char`` c))

let ``ns-plain-one-line`` c =
    (``ns-plain-first`` c) + (``nb-ns-plain-in-line`` c)

let ``s-ns-plain-next-line`` n c =
    (``s-flow-folded`` n) + (``ns-plain-char`` c) + (``nb-ns-plain-in-line`` c)

let ``ns-plain-multi-line`` n c =
    (``ns-plain-one-line`` c) + ZOM(``s-ns-plain-next-line`` n c)


let ``ns-plain`` n c =
    match c with
    | ``Flow-out``  -> ``ns-plain-multi-line`` n c
    | ``Flow-in``   -> ``ns-plain-multi-line`` n c
    | ``Block-key`` -> ``ns-plain-one-line`` c
    | ``Flow-key``  -> ``ns-plain-one-line`` c
    | _              -> raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")


let MapScalar s =
    match s with
    |   Regex (NullGlobalTag.Regex)     _ -> ScalarNode(NodeData<string>.Create NullGlobalTag s)
    |   Regex (BooleanGlobalTag.Regex)  _ -> ScalarNode(NodeData<string>.Create BooleanGlobalTag s)
    |   Regex (IntegerGlobalTag.Regex)  _ -> ScalarNode(NodeData<string>.Create IntegerGlobalTag s)
    |   Regex (FloatGlobalTag.Regex)    _ -> ScalarNode(NodeData<string>.Create FloatGlobalTag s)
    |   _ -> ScalarNode(NodeData<string>.Create StringGlobalTag s)


//  Flow Collection styles: http://www.yaml.org/spec/1.2/spec.html#id2790088

type FlowCollectionStyles() =
    member this.``c-flow-sequence`` s n c =
        match (HasMatches(s, RGC((RGP "\\[") + OPT(``s-separate`` n c)))) with
        | (false, _, _) ->  None
        | (true, m, inputrs)  -> 
            let ``ns-s-flow-seq-entries`` = this.``ns-s-flow-seq-entries`` inputrs n (``in-flow`` c)
            let rs = 
                match ``ns-s-flow-seq-entries`` with 
                |   None         -> inputrs
                |   Some(c, ors) -> ors
             
             match (HasMatches(rs, (RGP "\\]"))) with
             |  (false, _,_)    -> raise (sprintf "Expected ']' at \"%s\"" (rs.Substring(6)))
             |  (true, mt,frs)  ->
                match ``ns-s-flow-seq-entries`` with 
                |   None         -> (SeqNode(NodeData.Create SequenceGlobalTag []),  frs)
                |   Some(c, ors) -> (SeqNode(NodeData.Create SequenceGlobalTag [c]), frs)

    member this.``ns-s-flow-seq-entries`` s n c = None



// Flow nodes: http://www.yaml.org/spec/1.2/spec.html#id2792977


let ``n/a`` = 0

let ``ns-flow-yaml-content`` n c =
    ``ns-plain`` n c

let ``ns-s-implicit-yaml-key`` c =
    (``ns-flow-yaml-node`` ``n/a`` c) + OPT(``s-separate-in-line``) (* At most 1024 characters altogether *)


let ``ns-flow-yaml-node`` n c =
    ``c-ns-alias-node`` ||| (``ns-flow-yaml-content`` n c) |||
    (
        (``c-ns-properties`` n c) + 
        (((``s-separate`` n c) + (``ns-flow-yaml-content`` n c)) ||| ``e-scalar``)
    )

let ``ns-flow-map-implicit-entry`` n c =
    (``ns-flow-map-yaml-key-entry`` n c) |||  (``c-ns-flow-map-empty-key-entry`` n c) ||| (``c-ns-flow-map-json-key-entry`` n c)



let ``ns-flow-map-explicit-entry`` n c =
    (``ns-flow-map-implicit-entry`` n c) ||| ( ``e-node`` + ``e-node``)

let ``ns-flow-pair`` n c =
    ( RGP "?" + (``s-separate`` n c) + (``ns-flow-map-explicit-entry`` n c) ) ||| (``ns-flow-pair-entry`` n c)

RGC((``ns-flow-pair`` n c) + OPT(``s-separate`` n c) + (RGP ",") + OPT(``s-separate`` n c))



let ``ns-flow-seq-entry`` n c =
    (``ns-flow-pair`` n c) ||| (``ns-flow-node`` n c)




type ProcessFunc = string -> int-> Context -> (Node * string)

type SubProcessor = {
        Identifier  : string
        Regex       : string
        parsefn     : ProcessFunc
    }
    with
        static member Create(ident, detect, parse) = 
            { Identifier = ident ; Regex = detect ; parsefn = parse }

        member this.Parse = this.parsefn



 let FlowSeqProcessor = 
    let regex = RGC((RGP "\\[") + OPT(``s-separate`` n c) + (``ns-flow-seq-entry`` n c) + OPT(``s-separate`` n c))
    
    let parseSeqEntry s = Match(s, RGC((``ns-flow-seq-entry`` n c))) |> List.Head

    let rec parseValues inputstring indent ctx nodes = 
        match inputstring with

        RGC((``ns-flow-seq-entry`` n c) + OPT(``s-separate`` n c) + (RGP ",") + OPT(``s-separate`` n c))

        | Regex (RGC((``ns-flow-seq-entry`` n c) + OPT(``s-separate`` n c))) matches ->
            let node = MapScalar (matches.Head)
            parseValues (Advance(matches.Head, inputstring)) indent ctx (node :: nodes)
        | Regex (RGC())

    let parse inputstring indent ctx = 
        let m1 = Match(inputstring, ((RGP "\\[") + OPT(``s-separate`` n c)))
        let inputstring = Advance(m1, inputstring)


    SubProcessor.Create(
        "c-flow-sequence",
        regex,
        parse
    )
    



let ``ns-flow-content`` n c =
    (``ns-flow-yaml-content`` n c)  ||| (``c-flow-json-content`` n c)




//	[138] ns-s-flow-seq-entries(n,c)	::=	ns-flow-seq-entry(n,c) s-separate(n,c)? ( “,” s-separate(n,c)? ns-s-flow-seq-entries(n,c)? )?
let ``ns-s-flow-seq-entries`` n c =
    (``ns-flow-seq-entry`` n c) + OPT(``s-separate`` n c) +
    OPT(
        ZOM((RGP ",") + OPT(``s-separate`` n c) + (``ns-flow-seq-entry`` n c) + OPT(``s-separate`` n c)) |||
        ((RGP ",") + OPT(``s-separate`` n c))
    )



let ``ns-flow-node`` n c =
    ``c-ns-alias-node`` ||| 
    (``ns-flow-content`` n c) |||
    (
        (``c-ns-properties`` n c) +
        (
            ((``s-separate`` n c) + (``ns-flow-content`` n c)) ||| ``e-scalar``
        )
    )

let ``c-ns-flow-map-separate-value`` n c =
    (RGP ":") + NOT(``ns-plain-safe`` c) + 
    (
        (( ``s-separate`` n c) + (``ns-flow-node`` n c)) ||| ``e-node``
    )

let ``ns-flow-pair-yaml-key-entry`` n c =
    (``ns-s-implicit-yaml-key`` ``Flow-key``) + (``c-ns-flow-map-separate-value`` n c)


let ``c-ns-flow-map-empty-key-entry`` n c =
    ``e-node`` + (``c-ns-flow-map-separate-value`` n c)

let ``ns-flow-map-yaml-key-entry`` n c =
    (``ns-flow-yaml-node`` n c) + 
    (
        (OPT(``s-separate`` n c) +  (``c-ns-flow-map-separate-value`` n c)) ||| ``e-node``
    )

let ``c-ns-flow-map-adjacent-value`` n c =
    (RGP ":") + 
    (
        (OPT(``s-separate`` n c) + (``ns-flow-node`` n c)) |||  ``e-node``
    )



let ``c-flow-json-content`` n c =
    (``c-flow-sequence`` n c) ||| (``c-flow-mapping`` n c) ||| (``c-single-quoted`` n c) ||| (``c-double-quoted`` n c)


let ``c-flow-json-node`` n c =
    OPT((``c-ns-properties`` n c)  + (``s-separate`` n c)) + (``c-flow-json-content`` n c)

let ``c-s-implicit-json-key`` c =
    (``c-flow-json-node`` ``n/a`` c) + OPT(``s-separate-in-line``) (* At most 1024 characters altogether *)

let ``c-ns-flow-pair-json-key-entry`` n c =
    (``c-s-implicit-json-key`` ``Flow-key``) + (``c-ns-flow-map-adjacent-value`` n c)

let ``c-ns-flow-map-json-key-entry`` n c =
    (``c-flow-json-node`` n c) +
    ((OPT( ``s-separate`` n c) + (``c-ns-flow-map-adjacent-value`` n c)) ||| ``e-node``)

let ``ns-flow-pair-entry`` n c =
    (``ns-flow-pair-yaml-key-entry`` n c) ||| (``c-ns-flow-map-empty-key-entry`` n c) ||| (``c-ns-flow-pair-json-key-entry`` n c)

let ``ns-flow-map-entry`` n c =
    ((RGP "\\?") + (``s-separate`` n c) + (``ns-flow-map-explicit-entry`` n c)) |||
    (``ns-flow-map-implicit-entry`` n c)

//  [141]	ns-s-flow-map-entries(n,c)	::=	ns-flow-map-entry(n,c) s-separate(n,c)? ( “,” s-separate(n,c)? ns-s-flow-map-entries(n,c)? )?
let ``ns-s-flow-map-entries`` n c =
    (``ns-flow-map-entry`` n c) + OPT(``s-separate`` n c) + 
    OPT(
        ZOM((RGP ",") + OPT(``s-separate`` n c) + (``ns-flow-map-entry`` n c) + OPT(``s-separate`` n c)) |||
        ((RGP ",") + OPT(``s-separate`` n c))
    )

let ``c-flow-mapping`` n c =
    (RGP "\\{") + OPT(``s-separate`` n c) + OPT(``ns-s-flow-map-entries`` n (``in-flow`` c)) + (RGP "\\}")



//    Block Styles: http://www.yaml.org/spec/1.2/spec.html#Block
//    Block Scalar Styles: http://www.yaml.org/spec/1.2/spec.html#id2793652

let ``c-b-block-header`` m t =
    (
    ((``c-indentation-indicator`` m) + (``c-chomping-indicator`` t)) ||| 
    ((``c-chomping-indicator`` t)    + (``c-indentation-indicator`` m))
    ) +
    ``s-b-comment``

let ``c-indentation-indicator`` m =
    ``ns-dec-digit`` m = ns-dec-digit - #x30
/* Empty */  ⇒ m = auto-detect()


let rxp = sprintf "^(%O)$" (``c-double-quoted`` 2 ``Flow-key``)
let r1 = new Regex(rxp, RegexOptions.None)
r1.IsMatch("\"implicit block key\"")

