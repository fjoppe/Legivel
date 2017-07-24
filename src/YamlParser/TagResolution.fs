module TagResolution

open System
open RepresentationGraph
open RegexDSL
open ErrorsAndWarnings
open YamlParser.Internals

exception TagResolutionException of string

[<NoEquality; NoComparison>]
type TagResolutionInfo = {
        NonSpecificTag  : string
        Path            : Node list
        Content         : Node
        NodeKind        : NodeKind
    }
    with
        static member Create nst p c nk =
            { NonSpecificTag = nst; Path = p; Content = c; NodeKind = nk }


type TagResolutionFunc = (TagResolutionInfo -> GlobalTag option)
type UnresolvedTagResolutionFunc = (NodeKind -> string -> GlobalTag)


[<NoEquality; NoComparison>]
type GlobalTagSchema = {
    GlobalTags              : GlobalTag list
    TagResolution           : TagResolutionFunc
    UnresolvedResolution    : UnresolvedTagResolutionFunc
    LocalTags               : LocalTagsFuncs
}


type TagShorthand 
    with
        static member Create (short, full) = { ShortHand = short; MappedTagBase = full}
        static member DefaultSecondaryTagHandler = { ShortHand = "!!" ; MappedTagBase = "tag:yaml.org,2002:"}


let private clearTrailingZeros (s:string) = 
    let charList = 
        s.ToCharArray()
        |> List.ofArray 
    let cleared = 
        (charList.Head) :: (
            charList.Tail
            |> List.rev 
            |> List.skipWhile(fun c -> c='0')
            |> List.rev
        )
    String.Join("", cleared)


module internal Failsafe =
    let getMapNode (n:Node) =
        match n with
        |   MapNode n ->  n.Data 
        |   _    -> failwith "YamlParser Defect: Tag-kind mismatch between node and tag, expecting a Map Node"

    let getSeqNode (n:Node) =
        match n with
        |   SeqNode n ->  n.Data 
        |   _    -> failwith "YamlParser Defect: Tag-kind mismatch between node and tag, expecting e Seq Node"

    let getScalarNode (n:Node) =
        match n with
        |   ScalarNode n ->  n.Data 
        |   _    -> failwith "YamlParser Defect: Tag-kind mismatch between node and tag, expecting a Scalar Node"

    let areUnorderedMappingsEqual (n1:Node) (n2:Node) = 
        n1.Hash = n2.Hash &&
        n1.Kind = n2.Kind &&
        n1.NodeTag = n2.NodeTag &&
        match (n1,n2) with
        |   (MapNode mn1, MapNode mn2)  ->
            mn1.Data.Length = mn2.Data.Length &&
            //  to check equality of unordered content, we order it!
            (mn1.Data |> List.sortBy(fun (k, _) -> k.Hash)) 
            |> List.zip (mn2.Data |> List.sortBy(fun (k, _) -> k.Hash))
            |> List.forall(fun ((kl,vl),(kr,vr)) -> kl.NodeTag.AreEqual kl kr && vl.NodeTag.AreEqual vl vr)
        |   _ -> false

    let getUnorderedMappingHash (n:Node) =
        (lazy(getMapNode n
                |> List.map(fun (k,_) -> k.Hash)
                |> List.sort
                |> NodeHash.Merge)
        )

    let findDuplicateKeys (nlst: Node list) =
        let areEqual (nl:Node list) (n: Node) =
            if nl.Length = 0 then false
            else n.NodeTag.AreEqual (nl.Head) n
        nlst
        |> List.groupBy(fun k -> k.Hash, k.NodeTag)
        |> List.map(snd)
        |> List.map(fun nl -> if (nl |> List.forall(fun n -> areEqual nl n)) then nl else [])
        |> List.filter(fun kl -> kl.Length > 1)

    let validateDuplicateKeys n (nlst: Node list) =
        nlst
        |> findDuplicateKeys
        |> function
            |   []  -> Value n
            |   dupLst ->
                let errs = [
                    for kt in dupLst do
                        let rf = kt.Head
                        for n in (kt |> List.skip 1) do
                            yield MessageAtLine.CreateContinue (n.ParseInfo.Start) ErrMapDuplicateKey (sprintf "Duplicate key for node %s at position: %s" (n.ToPrettyString()) (rf.ParseInfo.Start.ToPrettyString()))
                    ]
                ErrorResult errs


    let validateMappingForDuplicateKeys (n: Node) =
            getMapNode n |> List.map(fst) |> validateDuplicateKeys n


    let areUnorderedSequencesEqual (n1:Node) (n2:Node) = 
        n1.Hash = n2.Hash &&
        n1.Kind = n2.Kind &&
        n1.NodeTag = n2.NodeTag &&
        match (n1,n2) with
        |   (SeqNode mn1, SeqNode mn2)  ->
            mn1.Data.Length = mn2.Data.Length &&
            //  to check equality of unordered content, we order it!
            (mn1.Data |> List.sortBy(fun k -> k.Hash)) 
            |> List.zip (mn2.Data |> List.sortBy(fun k -> k.Hash))
            |> List.forall(fun (l,r) -> l.NodeTag.AreEqual l r)
        |   _ -> false

    let getUnorderedSeqenceHash (n:Node) = 
        (lazy(getSeqNode n
                |> List.map(fun e -> e.Hash)
                |> List.sort
                |> NodeHash.Merge)
        )

    let isUnorderedSequenceValid (n: Node) = Value(n)

    let areScalarsEqual (n1:Node) (n2:Node) = 
        n1.Hash = n2.Hash &&
        n1.Kind = n2.Kind &&
        n1.NodeTag = n2.NodeTag &&
        match (n1,n2) with
        |   (ScalarNode mn1, ScalarNode mn2)  -> 
            mn1.Tag = mn2.Tag &&
            (mn1.Tag.CanonFn (mn1.Data)) = (mn2.Tag.CanonFn (mn2.Data))
        |   _ -> false

    let getScalarHash (n:Node) = (lazy(NodeHash.Create (getScalarNode n)))

    let isScalarValid (n: Node) = Value(n)

    let isScalarMatch n t = 
        match n with
        |   ScalarNode nd ->  IsMatch (nd.Data, t.Regex)
        |   _    -> false

    let neverMatches _ _ = false

    let fsMappingTag = TagFunctions.Create areUnorderedMappingsEqual getUnorderedMappingHash validateMappingForDuplicateKeys neverMatches
    let fsSequenceTag = TagFunctions.Create areUnorderedSequencesEqual getUnorderedSeqenceHash isUnorderedSequenceValid neverMatches
    let fsScalarTag = TagFunctions.Create areScalarsEqual getScalarHash isScalarValid isScalarMatch


    let getUnresolvedTag nodeKind tagstr = 
        let tf =
            match nodeKind with
            |   NodeKind.Mapping    -> fsMappingTag
            |   NodeKind.Sequence   -> fsSequenceTag
            |   NodeKind.Scalar     -> fsScalarTag
        GlobalTag.Create (DecodeEncodedUriHexCharacters(tagstr), nodeKind, tf)


    let localTagsAreEqual n1 n2 = true

    let localTagsGetHash (n:Node) =
        match n with
        |   MapNode _  -> getUnorderedMappingHash n
        |   SeqNode _  -> getUnorderedSeqenceHash n
        |   ScalarNode _ -> getScalarHash n
    
    let localtagFunctions = { LocalTagsFuncs.AreEqual = localTagsAreEqual; GetHash = localTagsGetHash}

    let MappingGlobalTag =  GlobalTag.Create("tag:yaml.org,2002:map", Mapping, fsMappingTag)
    let SequenceGlobalTag = GlobalTag.Create("tag:yaml.org,2002:seq", Sequence, fsSequenceTag)
    let StringGlobalTag =   GlobalTag.Create("tag:yaml.org,2002:str", Scalar, fsScalarTag )

    let providedTags = [MappingGlobalTag; SequenceGlobalTag; StringGlobalTag]


module internal NonSpecific =
    let NonSpecificTagQT = TagKind.NonSpecific {Handle ="!"; LocalTag =Failsafe.localtagFunctions}
    let NonSpecificTagQM = TagKind.NonSpecific {Handle ="?"; LocalTag = Failsafe.localtagFunctions}
    let UnresolvedTag = TagKind.NonSpecific {Handle ="?"; LocalTag = Failsafe.localtagFunctions}


module internal JSON =
    let NullGlobalTag =
        GlobalTag.Create("tag:yaml.org,2002:null", Scalar, "null",
            (fun s -> 
                    match s with
                    | Regex "null" _ -> "null"
                    | _ -> failwith (sprintf "Cannot convert to null: %s" s)
            ), Failsafe.fsScalarTag
        )

    let BooleanGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:bool", Scalar, "true|false",
            (fun s -> 
                match s with
                | Regex "true" _ -> "true"
                | Regex "false" _ -> "false"
                | _ -> failwith (sprintf "Cannot convert to boolean: %s" s)
            ), Failsafe.fsScalarTag
        )

    let IntegerGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:int", Scalar, "[-]?(0|[1-9][0-9]*)",
            (fun s ->
                match s with
                | Regex "^([-])?(0|[1-9][0-9_]*)$" [sign; is] -> sprintf "%+d" (Int32.Parse(String.Concat(sign, is)))
                | _ -> failwith (sprintf "Cannot convert to integer: %s" s)
            ), Failsafe.fsScalarTag
        )

    let FloatGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:float", Scalar, "[-]?(0|[1-9][0-9]*)?(\.[0-9]*)?([eE][-+][0-9]+)?",
            (fun s -> 
                let canonicalSign sign = if sign = "-" then "-" else "+"
                match s with
                | Regex "^([-])?(0|[1-9][0-9]*)?(?:\.([0-9]*))?(?:[eE]([-+])([0-9]+))?$" [sign; mantissa; prec; esign; exp] ->
                    let canExp = int(esign + "0" + exp) + (mantissa.Length)
                    let fullMantissa = clearTrailingZeros (mantissa + prec)
                    let canSign = if fullMantissa = "0" then "+" else canonicalSign sign
                    sprintf "%s0.%se%+04d" canSign fullMantissa canExp
                | _ -> failwith (sprintf "Cannot convert to float: %s" s)
            ), Failsafe.fsScalarTag
        )

    let providedScalarTags = [NullGlobalTag; BooleanGlobalTag;IntegerGlobalTag;FloatGlobalTag]

module internal YamlCore =
    let NullGlobalTag =
        GlobalTag.Create("tag:yaml.org,2002:null", Scalar, "~|null|Null|NULL|^$",
            (fun s -> 
                    match s with
                    | Regex "~|null|Null|NULL|^$" _ -> "~"
                    | _ -> failwith (sprintf "Cannot convert to null: %s" s)
            ), Failsafe.fsScalarTag
        )

    let BooleanGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:bool", Scalar, "true|True|TRUE|false|False|FALSE",
            (fun s -> 
                match s with
                | Regex "true|True|TRUE"    _ -> "true"
                | Regex "false|False|FALSE" _ -> "false"
                | _ -> failwith (sprintf "Cannot convert to boolean: %s" s)
            ), Failsafe.fsScalarTag
        )

    let IntegerGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:int", Scalar, "0o[0-7]+|[-+]?([0-9]+)|0x[0-9a-fA-F]+",
            (fun s ->
                // used for both digit and hex conversion
                let digitToValue c = if c >= 'A' then 10+(int c)-(int 'A') else (int c)-(int '0')
                let convertToCanonical sign number = sprintf "%+d" (Int32.Parse(String.Concat(sign, number.ToString())))
                match s with
                | Regex "^0o([0-7]+)$"  [os] -> 
                    let ps = os.ToCharArray() |> List.ofArray
                    let ic = ps |> List.fold(fun s c -> (s <<< 3) + (digitToValue  c)) 0
                    convertToCanonical "" ic
                | Regex "^([-+])?([0-9]+)$" [sign; is] -> sprintf "%+d" (Int32.Parse(String.Concat(sign, is)))
                | Regex "^(0x[0-9a-fA-F]+)$"  [hs] -> 
                    let ps = hs.Substring(2).ToUpper().ToCharArray() |> List.ofArray
                    let ic = ps |> List.fold(fun s c -> (s <<< 4) + (digitToValue  c)) 0
                    convertToCanonical "" ic
                | _ -> failwith (sprintf "Cannot convert to integer: %s" s)
            ), Failsafe.fsScalarTag
        )

    let FloatGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:float", Scalar, "[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?|[-+]?\\.(inf|Inf|INF)|\\.(nan|NaN|NAN)",
            (fun s -> 
                let canonicalSign sign = if sign = "-" then "-" else "+"
                match s with
                | Regex "^([-+])?(0|[1-9][0-9]*)?(?:\\.([0-9]*))?(?:[eE]([-+])([0-9]+))?$" [sign; mantissa; prec; esign; exp] ->
                    let canExp = int(esign + "0" + exp) + (mantissa.Length)
                    let fullMantissa = clearTrailingZeros (mantissa + prec)
                    let canSign = if fullMantissa = "0" then "+" else canonicalSign sign
                    sprintf "%s0.%se%+04d" canSign fullMantissa canExp
                | Regex "^([-+]?)\\.(?:inf|Inf|INF)$" [sign] ->
                    let canSign = canonicalSign sign
                    sprintf "%s.inf" canSign
                | Regex "^(\\.(nan|NaN|NAN))$" _ -> ".nan"
                | _ -> failwith (sprintf "Cannot convert to float: %s" s)
            ), Failsafe.fsScalarTag
        )

    let providedScalarTags = [BooleanGlobalTag;IntegerGlobalTag;FloatGlobalTag;NullGlobalTag]

module internal YamlExtended =

    let getPairs nl =
        nl
        |> List.map(Failsafe.getMapNode)
        |>  List.concat

    let getKeysFromPairs nl =
        nl
        |> getPairs
        |> List.map(fst)

        
    let areOrderedMappingsEqual (n1:Node) (n2:Node) = 
        n1.Hash = n2.Hash &&
        n1.Kind = n2.Kind &&
        n1.NodeTag = n2.NodeTag &&
        match (n1,n2) with
        |   (SeqNode sn1, SeqNode sn2)  ->
            let len = sn1.Data.Length = sn2.Data.Length 
            
            let mn1 = sn1.Data |> getPairs
            let mn2 = sn2.Data |> getPairs

            len &&
            //  to check equality of ordered content, we keep the order of entry
            mn1 |> List.zip(mn2)
            |> List.forall(fun ((kl,vl),(kr,vr)) -> kl.NodeTag.AreEqual kl kr && vl.NodeTag.AreEqual vl vr)
        |   _ -> false


    let getOrderedMappingHash (n:Node) =
        (lazy(Failsafe.getSeqNode n
            |> getKeysFromPairs
            |> List.map(fun k -> k.Hash)
            |> NodeHash.Merge)
        )


    let isMatchSequenceOfPairs (n:Node) t = 
        let isSinglularMapping (ns:Node) =
            match ns with
            |   MapNode nd ->  nd.Data.Length = 1
            |   _ -> false
        match n with
        |   SeqNode nd -> nd.Data |> List.forall(isSinglularMapping)
        |   _    -> false

    let isMatchSequenceOfMappings (n:Node) t = 
        if (isMatchSequenceOfPairs n n.NodeTag) then
            match n with
            |   SeqNode nd ->
                nd.Data 
                |> getKeysFromPairs
                |> Failsafe.findDuplicateKeys
                |>  function
                    | []    -> true
                    |   _   -> false
            |   _   -> false
        else
            false


    let validateOrderedMappings (n:Node) =
        if (isMatchSequenceOfPairs n n.NodeTag) then
            (Failsafe.getSeqNode n)
            |> getKeysFromPairs 
            |> Failsafe.validateDuplicateKeys n
        else
            ErrorResult [MessageAtLine.CreateContinue (n.ParseInfo.Start) ErrTagSyntax (sprintf "Construct has incorrect syntax for tag %s until position: %s, 'omap' is a sequence of singular mappings, without duplicates." (n.NodeTag.ToPrettyString()) (n.ParseInfo.End.ToPrettyString()))]

        
    let validateOrderedPairs (n:Node) =
        if (isMatchSequenceOfPairs n n.NodeTag) then Value n
        else ErrorResult [MessageAtLine.CreateContinue (n.ParseInfo.Start) ErrTagSyntax (sprintf "Construct has incorrect syntax for tag %s until position: %s, 'pairs' is a sequence of singular mappings." (n.NodeTag.ToPrettyString()) (n.ParseInfo.End.ToPrettyString()))]

    let NullGlobalTag =
        GlobalTag.Create("tag:yaml.org,2002:null", Scalar, "~|null|Null|NULL|^$",
            (fun s -> 
                    match s with
                    | Regex "~|null|Null|NULL|^$" _ -> "~"
                    | _ -> failwith (sprintf "Cannot convert to null: %s" s)
            ), Failsafe.fsScalarTag
        )

    let BooleanGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:bool", Scalar, "y|Y|yes|Yes|YES|n|N|no|No|NO|true|True|TRUE|false|False|FALSE|on|On|ON|off|Off|OFF",
            (fun s -> 
                match s with
                | Regex "y|Y|yes|Yes|YES|true|True|TRUE|on|On|ON" _ -> "true"
                | Regex "n|N|no|No|NO|false|False|FALSE|off|Off|OFF" _ -> "false"
                | _ -> failwith (sprintf "Cannot convert to boolean: %s" s)
            ), Failsafe.fsScalarTag
        )

    let IntegerGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:int", Scalar, "[-+]?0b[0-1_]+|[-+]?0[0-7_]+|[-+]?(0|[1-9][0-9_]*)|[-+]?0x[0-9a-fA-F_]+|[-+]?[1-9][0-9_]*(:[0-5]?[0-9])+",
            (fun s ->
                // used for both digit and hex conversion
                let digitToValue c = if c >= 'A' then 10+(int c)-(int 'A') else (int c)-(int '0')
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
                | _ -> failwith (sprintf "Cannot convert to integer: %s" s)
            ), Failsafe.fsScalarTag
        )

    let FloatGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:float", Scalar, "[-+]?([0-9][0-9_]*)?\.[0-9.]*([eE][-+][0-9]+)?|[-+]?[0-9][0-9_]*(:[0-5]?[0-9])+\.[0-9_]*|[-+]?\.(inf|Inf|INF)|\.(nan|NaN|NAN)",
            (fun s -> 
                let canonicalSign sign = if sign = "-" then "-" else "+"
                match s with
                | Regex "^([-+])?(0*)([1-9][0-9_]*)?\.(0*)([1-9][0-9.]*)(?:[eE]([-+])([0-9]+))?$" [sign; _; mantissa; zprec; prec; esign; exp] ->
                    let cleanMantissa = mantissa.Replace("_","")
                    let expCorrection, canMantissa = 
                        match cleanMantissa.Length with
                        | 0 -> (-zprec.Length, "")
                        | _ -> (cleanMantissa.Length,  cleanMantissa + zprec)
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
                | _ -> failwith (sprintf "Cannot convert to float: %s" s)
            ), Failsafe.fsScalarTag
        )

    let TimestampGlobalTag = 
        let rgyear = Repeat(RGO("0-9"),4)
        let rgmonth = RGO("0-9") + OPT(RGO("0-9"))
        let rgmonthf = Repeat(RGO("0-9"),2)
        let rgday = RGO("0-9") + OPT(RGO("0-9"))
        let rgdayf = Repeat(RGO("0-9"),2)
        let rgdate = (GRP rgyear) + RGP("-") + (GRP rgmonth) + RGP("-") + (GRP rgday)
        let rgdatef = (GRP rgyear) + RGP("-") + (GRP rgmonthf) + RGP("-") + (GRP rgdayf)
        let rghour = RGO("0-9") + OPT(RGO("0-9"))
        let rgmin = Repeat(RGO "0-9", 2)
        let rgsec = Repeat(RGO "0-9", 2)
        let rgfrac= ZOM(RGO "0-9")
        let rgtime = (GRP rghour) + RGP(":") + (GRP rgmin) + RGP(":") + (GRP rgsec) + OPT(RGP("\.") + GRP(rgfrac))
        let rgztimez = RGP("Z")
        let rgdtimez = (RGO "-+") + rghour + OPT(RGP(":") + rgmin)
        let rgws = ZOM(RGO " \t")
 
        let rgISO8601 = rgdate + OPT(((RGO "Tt") ||| OOM(rgws)) + rgtime + OPT(rgws + GRP((rgztimez ||| rgdtimez))))
        let rgtimestamp = rgdate ||| rgISO8601

        let timestampToCanonical s =
            let canon y mo d h mi s fr tz = sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%d%s" y mo d h mi s fr tz
            let ToInt s = int("0"+s)

            match s with
            | Regex (RGSF rgdatef) [_; year; month; day] ->
                let dt = DateTime.Parse(canon (ToInt year) (ToInt month) (ToInt day) 0 0 0 0 "Z")
                dt.ToUniversalTime().ToString("o")
            | Regex(RGSF rgISO8601)  [_; year; month; day; hour; min; sec; fraction; tz] -> 
                let tzc = if tz = "" then "Z" else tz
                let dt = DateTime.Parse(canon (ToInt year) (ToInt month) (ToInt day) (ToInt hour) (ToInt min) (ToInt sec) (ToInt fraction) tzc)
                dt.ToUniversalTime().ToString("o")
            | _ -> failwith (sprintf "Cannot convert to timestamp: %s" s)

        let validateTimestamp n =
            let (isValid,str) = 
                let nd = Failsafe.getScalarNode n
                let str = timestampToCanonical nd
                (DateTime.TryParse(str) |> fst), nd
            if isValid then Value n 
            else 
                ErrorResult [MessageAtLine.CreateContinue (n.ParseInfo.Start) ErrTagBadFormat (sprintf "Timestamp has incorrect format: %s" str)]

        GlobalTag.Create("tag:yaml.org,2002:timestamp", Scalar, RGSF(rgtimestamp),
            (timestampToCanonical), { Failsafe.fsScalarTag with IsValid = validateTimestamp}
        )

    let isMatchUnorderedSet (n:Node) t = 
        let hasNoDuplicates nd =
            nd
            |> List.map(fst)
            |> Failsafe.findDuplicateKeys
            |>  function
                | []    -> true
                |   _   -> false
        let hasNoValues nd =
            nd
            |> List.map(snd)
            |> List.forall(fun (v:Node) -> 
                let res = v.NodeTag = Global NullGlobalTag
                res
                )
        match n with
        |   MapNode nd -> 
            hasNoDuplicates nd.Data &&
            hasNoValues nd.Data 
        |   _   -> false

    let validateUnorderedSet (n:Node) = 
        let hasNoValues nd =
            nd
            |> List.map(snd)
            |> List.forall(fun (v:Node) -> v.NodeTag = Global NullGlobalTag)
        if isMatchUnorderedSet n (n.NodeTag) then Value(n)
        else
            Failsafe.validateMappingForDuplicateKeys n
            |> FallibleOption.bind(fun _ ->
                if (hasNoValues (Failsafe.getMapNode n)) then Value(n)
                else 
                    ErrorResult [MessageAtLine.CreateContinue (n.ParseInfo.Start) ErrTagSyntax (sprintf "Construct has incorrect syntax for tag %s until position: %s, 'set' is a mapping without values, but not all values are null." (n.NodeTag.ToPrettyString()) (n.ParseInfo.End.ToPrettyString()))]
            )

    let yeOrderedMappingTag = TagFunctions.Create areOrderedMappingsEqual getOrderedMappingHash validateOrderedMappings isMatchSequenceOfMappings
    let yeOrderedPairsTag = TagFunctions.Create areOrderedMappingsEqual getOrderedMappingHash validateOrderedPairs isMatchSequenceOfPairs
    let yeUnorderedSetTag = TagFunctions.Create Failsafe.areUnorderedMappingsEqual Failsafe.getUnorderedMappingHash validateUnorderedSet isMatchUnorderedSet

    //  http://yaml.org/type/omap.html
    let OrderedMappingGlobalTag =  GlobalTag.Create("tag:yaml.org,2002:omap", Sequence, yeOrderedMappingTag)

    //  http://yaml.org/type/pairs.html
    let OrderedPairsGlobalTag =  GlobalTag.Create("tag:yaml.org,2002:pairs", Sequence, yeOrderedPairsTag)

    //  http://yaml.org/type/set.html
    let UnOrderedSetGlobalTag =  GlobalTag.Create("tag:yaml.org,2002:set", Mapping, yeUnorderedSetTag)

   
    //  order is important, !!pairs is a superset of !!omap
    let providedSeqTags = [OrderedMappingGlobalTag;OrderedPairsGlobalTag]
    let providedScalarTags = [BooleanGlobalTag; IntegerGlobalTag; FloatGlobalTag; TimestampGlobalTag; NullGlobalTag]
    let providedMappingTags = [UnOrderedSetGlobalTag]


let private tagResolution (mappingTags:GlobalTag list) (seqTags:GlobalTag list) (scalarTags:GlobalTag list) : TagResolutionFunc = fun nst -> 
    match nst.NonSpecificTag with
    |   "!" ->
        match nst.NodeKind with
        |   Mapping -> Some Failsafe.MappingGlobalTag
        |   Sequence-> Some Failsafe.SequenceGlobalTag
        |   Scalar  -> Some Failsafe.StringGlobalTag
    |   "?" ->  
        match nst.Content with
        |   MapNode _ -> 
            mappingTags
            |> List.tryFind(fun t -> t.IsMatch (nst.Content))
            |> Option.ifnone(Some Failsafe.MappingGlobalTag)
        |   SeqNode _ -> 
            seqTags
            |> List.tryFind(fun t -> t.IsMatch (nst.Content))
            |> Option.ifnone(Some Failsafe.SequenceGlobalTag)
        |   ScalarNode _ -> 
            scalarTags
            |> List.tryFind(fun t -> t.IsMatch (nst.Content))
            |> Option.ifnone(Some (Failsafe.StringGlobalTag))
    |   _ -> failwith (sprintf "Received illegal non-specific tag: %s" nst.NonSpecificTag)

    
//    Failsafe schema:  http://www.yaml.org/spec/1.2/spec.html#id2802346
let FailsafeSchema =
    {
        GlobalTags = Failsafe.providedTags
        TagResolution = tagResolution [] [] []
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }


//    Json schema:  http://www.yaml.org/spec/1.2/spec.html#id2803231
let JSONSchema =
    {
        GlobalTags = Failsafe.providedTags @ JSON.providedScalarTags 
        TagResolution = // TagResolution
            (fun nst ->
                match (nst.NonSpecificTag, nst.Content) with
                |   ("?", ScalarNode data) -> 
                    JSON.providedScalarTags  |> List.tryFind(fun t -> t.IsMatch (nst.Content))
                    |>  function
                        |   None -> raise (TagResolutionException <| sprintf "Unrecognized type for: %s" data.Data)
                        |   x -> x
                |   _ -> tagResolution [] [] JSON.providedScalarTags nst
            )
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }


let YamlCoreSchema =
    {
        GlobalTags = Failsafe.providedTags @ YamlCore.providedScalarTags
        TagResolution = tagResolution [] [] YamlCore.providedScalarTags
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }


let YamlExtendedSchema =
    {
        GlobalTags = Failsafe.providedTags @ YamlExtended.providedScalarTags @ YamlExtended.providedSeqTags @ YamlExtended.providedMappingTags
        TagResolution = tagResolution YamlExtended.providedMappingTags YamlExtended.providedSeqTags YamlExtended.providedScalarTags
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }
