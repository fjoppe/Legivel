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


module internal SchemaUtils =
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


    let toFloatComponent (m:string) (p:string) =
        let mt2z s = if s = "" then "0" else s

        let cleanMant = m.TrimStart('0') 
        let cleanPrec = p.TrimEnd('0')

        if cleanMant = "" then
            let unshifted = cleanPrec
            let shifted = unshifted.TrimStart('0')
            let exp = shifted.Length - unshifted.Length
            (shifted |> mt2z, exp)
        else
            let exp = cleanMant.Length
            ((cleanMant + cleanPrec).TrimEnd('0') |> mt2z, exp)

    let private tagFormatCheckError (n:Node) data =
        function
        |   (true, _)   -> Value n
        |   (false, _)  -> ErrorResult [MessageAtLine.CreateTerminate (n.ParseInfo.Start) ErrTagBadFormat (sprintf "Incorrect format: %s, for tag: %s" data (n.NodeTag.ToPrettyString()))]

    let isFloatValid n = 
        let data = getScalarNode n
        System.Double.TryParse(n.NodeTag.CanonFn data) |> tagFormatCheckError n data

    let isIntValid n = 
        let data = getScalarNode n
        System.Int32.TryParse(n.NodeTag.CanonFn data) |> tagFormatCheckError n data

    let isBooleanValid n = 
        let data = getScalarNode n
        System.Boolean.TryParse(n.NodeTag.CanonFn data) |> tagFormatCheckError n data


module internal Failsafe =
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
        (lazy(SchemaUtils.getMapNode n
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
            SchemaUtils.getMapNode n |> List.map(fst) |> validateDuplicateKeys n


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
        (lazy(SchemaUtils.getSeqNode n
                |> List.map(fun e -> e.Hash)
                |> List.sort
                |> NodeHash.Merge)
        )

    let validateUnorderedSequence (n: Node) = Value(n)

    let areScalarsEqual (n1:Node) (n2:Node) = 
        n1.Hash = n2.Hash &&
        n1.Kind = n2.Kind &&
        n1.NodeTag = n2.NodeTag &&
        match (n1,n2) with
        |   (ScalarNode mn1, ScalarNode mn2)  -> 
            mn1.Tag = mn2.Tag &&
            (mn1.Tag.CanonFn (mn1.Data)) = (mn2.Tag.CanonFn (mn2.Data))
        |   _ -> false

    let getScalarHash (n:Node) = (lazy(NodeHash.Create (SchemaUtils.getScalarNode n)))

    let isScalarValid (n: Node) = Value(n)

    let isScalarMatch n t = 
        match n with
        |   ScalarNode nd ->  IsMatch (nd.Data, t.Regex)
        |   _    -> false

    let neverMatches _ _ = false

    let fsMappingTag = TagFunctions.Create areUnorderedMappingsEqual getUnorderedMappingHash validateMappingForDuplicateKeys neverMatches
    let fsSequenceTag = TagFunctions.Create areUnorderedSequencesEqual getUnorderedSeqenceHash validateUnorderedSequence neverMatches
    let fsScalarTag = TagFunctions.Create areScalarsEqual getScalarHash isScalarValid isScalarMatch


    let getUnresolvedTag nodeKind tagstr = 
        let tf =
            match nodeKind with
            |   NodeKind.Mapping    -> fsMappingTag
            |   NodeKind.Sequence   -> fsSequenceTag
            |   NodeKind.Scalar     -> fsScalarTag
        GlobalTag.Create (DecodeEncodedUriHexCharacters(tagstr), nodeKind, tf)


    let localTagsAreEqual _ _ = true

    let localTagsGetHash (n:Node) =
        match n with
        |   MapNode _  -> getUnorderedMappingHash n
        |   SeqNode _  -> getUnorderedSeqenceHash n
        |   ScalarNode _ -> getScalarHash n
    
    let localtagFunctions = { LocalTagsFuncs.AreEqual = localTagsAreEqual; GetHash = localTagsGetHash}

    let MappingGlobalTag =  GlobalTag.Create("tag:yaml.org,2002:map", Mapping, fsMappingTag)
    let SequenceGlobalTag = GlobalTag.Create("tag:yaml.org,2002:seq", Sequence, fsSequenceTag)
    let StringGlobalTag =   GlobalTag.Create("tag:yaml.org,2002:str", Scalar, fsScalarTag )


    let defaultFailSafeResolution nst =
        match nst.NodeKind with
        |   Mapping -> Some MappingGlobalTag
        |   Sequence-> Some SequenceGlobalTag
        |   Scalar  -> Some StringGlobalTag

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
                | _ -> "ILLEGAL VALUE"
            ), { Failsafe.fsScalarTag with PostProcessAndValidateNode = SchemaUtils.isBooleanValid}
        )

    let IntegerGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:int", Scalar, "[-]?(0|[1-9][0-9]*)",
            (fun s ->
                match s with
                | Regex "^([-])?(0|[1-9][0-9_]*)$" [sign; is] -> sprintf "%+d" (Int32.Parse(String.Concat(sign, is)))
                | _ -> "ILLEGAL VALUE"
            ), { Failsafe.fsScalarTag with PostProcessAndValidateNode = SchemaUtils.isIntValid}
        )

    let FloatGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:float", Scalar, "[-]?(0|[1-9][0-9]*)?(\.[0-9]*)?([eE][-+][0-9]+)?",
            (fun s -> 
                let canonicalSign sign = if sign = "-" then "-" else "+"
                match s with
                | Regex "^([-])?(0|[1-9][0-9]*)?(?:\.([0-9]*))?(?:[eE]([-+])([0-9]+))?$" [sign; mantissa; prec; esign; exp] ->
                    let (fullMantissa, canExp) = SchemaUtils.toFloatComponent mantissa prec
                    let givenExp = int(esign + "0" + exp)
                    let canSign = if fullMantissa = "0" then "+" else canonicalSign sign
                    sprintf "%s0.%se%+04d" canSign fullMantissa (canExp+givenExp)
                | _ -> "ILLEGAL VALUE"
            ), { Failsafe.fsScalarTag with PostProcessAndValidateNode = SchemaUtils.isFloatValid}
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
                | _ -> "ILLEGAL VALUE"
            ), { Failsafe.fsScalarTag with PostProcessAndValidateNode = SchemaUtils.isBooleanValid}
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
                | _ -> "ILLEGAL VALUE"
            ), { Failsafe.fsScalarTag with PostProcessAndValidateNode = SchemaUtils.isIntValid}
        )

    let FloatGlobalTag = 
        let toCanonical s =
            let canonicalSign sign = if sign = "-" then "-" else "+"
            match s with
            | Regex "^([-+])?(0|[1-9][0-9]*)?(?:\\.([0-9]*))?(?:[eE]([-+])([0-9]+))?$" [sign; mantissa; prec; esign; exp] ->
                let (fullMantissa, canExp) = SchemaUtils.toFloatComponent mantissa prec
                let givenExp = int(esign + "0" + exp)
                let canSign = if fullMantissa = "0" then "+" else canonicalSign sign
                sprintf "%s0.%se%+04d" canSign fullMantissa (canExp+givenExp)
            | Regex "^([-+]?)\\.(?:inf|Inf|INF)$" [sign] ->
                let canSign = canonicalSign sign
                sprintf "%s.inf" canSign
            | Regex "^(\\.(nan|NaN|NAN))$" _ -> ".nan"
            | _ -> "ILLEGAL VALUE"

        let isFloatValid n =
            let data = SchemaUtils.getScalarNode n
            match (toCanonical data) with
            |   "+.inf"
            |   "-.inf"
            |   ".nan"  -> Value n
            |   _ -> SchemaUtils.isFloatValid n

        GlobalTag.Create("tag:yaml.org,2002:float", Scalar, "[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?|[-+]?\\.(inf|Inf|INF)|\\.(nan|NaN|NAN)",
            toCanonical, { Failsafe.fsScalarTag with PostProcessAndValidateNode = isFloatValid}
        )

    let providedScalarTags = [BooleanGlobalTag;IntegerGlobalTag;FloatGlobalTag;NullGlobalTag]

module internal YamlExtended =

    let getPairs nl =
        nl
        |> List.map(SchemaUtils.getMapNode)
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
        (lazy(SchemaUtils.getSeqNode n
            |> getKeysFromPairs
            |> List.map(fun k -> k.Hash)
            |> NodeHash.Merge)
        )


    let isMatchSequenceOfPairs (n:Node) _ = 
        let isSinglularMapping (ns:Node) =
            match ns with
            |   MapNode nd ->  nd.Data.Length = 1
            |   _ -> false
        match n with
        |   SeqNode nd -> nd.Data |> List.forall(isSinglularMapping)
        |   _    -> false

    let isMatchSequenceOfMappings (n:Node) _ = 
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
            (SchemaUtils.getSeqNode n)
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
                    | Regex "(^(~|null|Null|NULL)$)|^$" _ -> "~"
                    | _ -> failwith (sprintf "Cannot convert to null: %s" s)
            ), Failsafe.fsScalarTag
        )

    let BooleanGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:bool", Scalar, "y|Y|yes|Yes|YES|n|N|no|No|NO|true|True|TRUE|false|False|FALSE|on|On|ON|off|Off|OFF",
            (fun s -> 
                match s with
                | Regex "^(y|Y|yes|Yes|YES|true|True|TRUE|on|On|ON)$" _ -> "true"
                | Regex "^(n|N|no|No|NO|false|False|FALSE|off|Off|OFF)$" _ -> "false"
                | _ -> "ILLEGAL VALUE" // failwith (sprintf "Cannot convert to boolean: %s" s)
            ), { Failsafe.fsScalarTag with PostProcessAndValidateNode = SchemaUtils.isBooleanValid}
        )

    let IntegerGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:int", Scalar, "[-+]?0b[0-1_]+|[-+]?0[0-7_]+|[-+]?(0|[1-9][0-9_]*)|[-+]?0x[0-9a-fA-F_]+|[-+]?[1-9][0-9_]*(:[0-5]?[0-9])+",
            (fun s ->
                // used for both digit and hex conversion
                let digitToValue c = if c >= 'A' then 10+(int c)-(int 'A') else (int c)-(int '0')
                let convertToCanonical sign number = sprintf "%+d" (Int32.Parse(String.Concat(sign, number.ToString())))
                match s with
                | Regex "^(?:([-+])?0b([0-1_]+))$" [sign; bs] -> 
                    let ps = bs.Replace("_","").ToCharArray() |> List.ofArray
                    let ic = ps |> List.fold(fun s c -> (s <<< 1) + (digitToValue  c)) 0
                    convertToCanonical sign ic
                | Regex "^(?:([-+])?0([0-7_]+))$"  [sign; os] -> 
                    let ps = os.Replace("_","").ToCharArray() |> List.ofArray
                    let ic = ps |> List.fold(fun s c -> (s <<< 3) + (digitToValue  c)) 0
                    convertToCanonical sign ic
                | Regex "^(?:([-+])?(0|[1-9][0-9_]*))$" [sign; is] -> sprintf "%+d" (Int32.Parse(String.Concat(sign, is.Replace("_",""))))
                | Regex "^(?:([-+])?(0x[0-9a-fA-F_]+))$"  [sign; hs] -> 
                    let ps = hs.Substring(2).ToUpper().Replace("_","").ToCharArray() |> List.ofArray
                    let ic = ps |> List.fold(fun s c -> (s <<< 4) + (digitToValue  c)) 0
                    convertToCanonical sign ic
                | Regex "^(?:([-+])?([1-9][0-9_]*(:[0-5]?[0-9])+))$" ssl ->
                    let sign = List.item 0 ssl
                    let ss   = List.item 1 ssl
                    let ps = ss.Replace("_","").Split([|":"|], StringSplitOptions.RemoveEmptyEntries)
                    let ic = ps |> List.ofArray  |> List.fold(fun s t -> (s * 60) + (Int32.Parse(t))) 0
                    convertToCanonical sign ic
                | _ -> "ILLEGAL VALUE" // failwith (sprintf "Cannot convert to integer: %s" s)
            ), { Failsafe.fsScalarTag with PostProcessAndValidateNode = SchemaUtils.isIntValid}
        )

    let FloatGlobalTag = 
        let toCanonical s =
            let canonicalSign sign = if sign = "-" then "-" else "+"
            match s with
            | Regex "^(?:([-+])?([0-9][0-9_]*)?\.([0-9.]*)(?:[eE]([-+])([0-9]+))?)$" [sign; mantissa; prec; esign; exp] ->
                let (fullMantissa, canExp) = SchemaUtils.toFloatComponent (mantissa.Replace("_","")) prec
                let givenExp = int(esign + "0" + exp)
                let canSign = if fullMantissa = "0" then "+" else canonicalSign sign
                sprintf "%s0.%se%+04d" canSign fullMantissa (canExp+givenExp)
            | Regex "^(?:([-+]?)((?:[0-9][0-9_]*)(?::[0-5]?[0-9])+)\.([0-9_]*))$"  [sign; mantissa; prec] -> 
                let ps = mantissa.Replace("_","").Split([|":"|], StringSplitOptions.RemoveEmptyEntries)
                let ic = ps |> List.ofArray  |> List.fold(fun s t -> (s * 60) + (Int32.Parse(t))) 0
                let canSign = canonicalSign sign
                let canMantissa = ic.ToString()
                let canExp = canMantissa.Length
                sprintf "%s0.%s%se%+04d" canSign canMantissa (prec.Replace("_","")) canExp
            | Regex "^(?:([-+]?)\.(?:inf|Inf|INF))$" [sign] ->
                let canSign = canonicalSign sign
                sprintf "%s.inf" canSign
            | Regex "^(?:(\.(nan|NaN|NAN)))$" _ -> ".nan"
            | _ -> "ILLEGAL VALUE" // failwith (sprintf "Cannot convert to float: %s" s)

        let isFloatValid n =
            let data = SchemaUtils.getScalarNode n
            match (toCanonical data) with
            |   "+.inf"
            |   "-.inf"
            |   ".nan"  -> Value n
            |   _ -> SchemaUtils.isFloatValid n

        GlobalTag.Create("tag:yaml.org,2002:float", Scalar, "^[-+]?([0-9][0-9_]*)?\.[0-9.]*([eE][-+][0-9]+)?|[-+]?[0-9][0-9_]*(:[0-5]?[0-9])+\.[0-9_]*|[-+]?\.(inf|Inf|INF)|\.(nan|NaN|NAN)$",
            toCanonical, { Failsafe.fsScalarTag with PostProcessAndValidateNode = isFloatValid}
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
                let ut = dt.ToUniversalTime()   // in two steps, or we get a warning
                ut.ToString("o")
            | Regex(RGSF rgISO8601)  [_; year; month; day; hour; min; sec; fraction; tz] -> 
                let tzc = if tz = "" then "Z" else tz
                let dt = DateTime.Parse(canon (ToInt year) (ToInt month) (ToInt day) (ToInt hour) (ToInt min) (ToInt sec) (ToInt fraction) tzc)
                let ut = dt.ToUniversalTime()   // in two steps, or we get a warning
                ut.ToString("o")
            | _ -> "ILLEGAL VALUE" // failwith (sprintf "Cannot convert to timestamp: %s" s)

        let validateTimestamp n =
            let (isValid,str) = 
                let nd = SchemaUtils.getScalarNode n
                let str = timestampToCanonical nd
                (DateTime.TryParse(str) |> fst), nd
            if isValid then Value n 
            else 
                ErrorResult [MessageAtLine.CreateContinue (n.ParseInfo.Start) ErrTagBadFormat (sprintf "Timestamp has incorrect format: %s" str)]

        GlobalTag.Create("tag:yaml.org,2002:timestamp", Scalar, RGSF(rgtimestamp),
            (timestampToCanonical), { Failsafe.fsScalarTag with PostProcessAndValidateNode = validateTimestamp}
        )

    //  http://yaml.org/type/value.html
    let ValueGlobalTag =
        //  this tag only marks !!value, no logic; a native constuctor could/should effect this
        GlobalTag.Create("tag:yaml.org,2002:value", Scalar, "=",
            (fun s -> 
                    match s with
                    | Regex "=" _ -> "="
                    | _ -> failwith (sprintf "Cannot convert to value: %s" s)
            ), Failsafe.fsScalarTag
        )

    //  http://yaml.org/type/merge.html
    let MergeGlobalTag =
        //  this tag only marks !!merge, the !!map tag should effect this
        GlobalTag.Create("tag:yaml.org,2002:merge", Scalar, "<<",
            (fun s -> 
                    match s with
                    | Regex "<<" _ -> "<<"
                    | _ -> failwith (sprintf "Cannot convert to merge: %s" s)
            ), Failsafe.fsScalarTag
        )

    //  http://yaml.org/type/binary.html
    let BinaryGlobalTag =
        //  This tag can only be assigned, and is never detected; bc too many collisions with plain text.
        let base64Alphabet = RGO("A-Z") + RGO("a-z") + RGO("+/")
        //  from YamlParser, rules 24-33
        let ``b-line-feed`` = RGP "\u000a"
        let ``b-carriage-return`` = RGP "\u000d" 
        let ``b-break`` = 
            (``b-carriage-return`` + ``b-line-feed``)   |||  //  DOS, Windows
            ``b-carriage-return``                       |||  //  MacOS upto 9.x
            ``b-line-feed``                                     //  UNIX, MacOS X
        let ``s-space`` = "\u0020"  // space
        let ``s-tab`` = "\u0009"    // tab
        let ``s-white`` = RGO(``s-space`` + ``s-tab``)
        let controlChar = ``b-break`` ||| ``s-white`` ||| RGP("=")
        let allowedChars = OOM(base64Alphabet ||| controlChar)

        let binaryToCanonical s =
            match s with
            | Regex (RGSF allowedChars) [full] -> "<<"
            | _ -> failwith (sprintf "Cannot convert to merge: %s" s)        
        GlobalTag.Create("tag:yaml.org,2002:binary", Scalar, RGSF(allowedChars), 
            binaryToCanonical, Failsafe.fsScalarTag)


    let isMatchUnorderedSet (n:Node) _ = 
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
                if (hasNoValues (SchemaUtils.getMapNode n)) then Value(n)
                else 
                    ErrorResult [MessageAtLine.CreateContinue (n.ParseInfo.Start) ErrTagSyntax (sprintf "Construct has incorrect syntax for tag %s until position: %s, 'set' is a mapping without values, but not all values are null." (n.NodeTag.ToPrettyString()) (n.ParseInfo.End.ToPrettyString()))]
            )

    let orderedMappingTagFuncs = TagFunctions.Create areOrderedMappingsEqual getOrderedMappingHash validateOrderedMappings isMatchSequenceOfMappings
    let orderedPairsTagFuncs = TagFunctions.Create areOrderedMappingsEqual getOrderedMappingHash validateOrderedPairs isMatchSequenceOfPairs
    let unorderedSetTagFuncs = TagFunctions.Create Failsafe.areUnorderedMappingsEqual Failsafe.getUnorderedMappingHash validateUnorderedSet isMatchUnorderedSet

    //  http://yaml.org/type/omap.html
    let OrderedMappingGlobalTag =  GlobalTag.Create("tag:yaml.org,2002:omap", Sequence, orderedMappingTagFuncs)

    //  http://yaml.org/type/pairs.html
    let OrderedPairsGlobalTag =  GlobalTag.Create("tag:yaml.org,2002:pairs", Sequence, orderedPairsTagFuncs)

    //  http://yaml.org/type/set.html
    let UnOrderedSetGlobalTag =  GlobalTag.Create("tag:yaml.org,2002:set", Mapping, unorderedSetTagFuncs)

    let  mergeAndValidateMapping n = 
        let (mn,rn) = (SchemaUtils.getMapNode n) |> List.partition(fun (k,_) -> k.NodeTag.Uri = MergeGlobalTag.Uri)

        let rec merge mlst reslst =
            let mergeMapNode nd rs =
                let areKeysEqual (km:Node) (kr:Node) = kr.NodeTag.AreEqual kr km
                let nodesToMerge = 
                    nd |>  List.filter(fun (km:Node,_) -> rs |> List.exists(fun (kr:Node,_) -> areKeysEqual km kr) |> not)
                nodesToMerge @ rs
            match mlst with
            |   []  -> reslst |> List.rev |> Value
            |   h :: tl -> 
                match h with
                |   MapNode nd ->   mergeMapNode nd.Data reslst |> merge tl
                |   SeqNode nd -> 
                    nd.Data |> List.filter(fun n -> n.Kind <> NodeKind.Mapping)
                    |>  function
                        |   []   -> 
                            nd.Data
                            |>  List.map(SchemaUtils.getMapNode)
                            |>  List.fold(fun s i -> mergeMapNode i s) reslst
                            |>  merge tl
                        |   frs    -> 
                            frs
                            |> List.map(fun fn -> MessageAtLine.CreateTerminate (fn.ParseInfo.Start) ErrTagConstraint (sprintf "Incorrect Node type at position: %s, << should map to a sequece of mapping nodes, other types are not allowed in the sequence." (h.ParseInfo.Start.ToPrettyString())))
                            |> ErrorResult
                |   ScalarNode _ -> ErrorResult [MessageAtLine.CreateTerminate (n.ParseInfo.Start) ErrTagConstraint (sprintf "Merge tag or << cannot map to a scalar, at position: %s, << should map to a mapping node, or a sequence of mappings." (h.ParseInfo.Start.ToPrettyString()))]

        merge (mn |> List.map(fun (_,v) -> v)) (rn |> List.rev)
        |>  FallibleOption.bind(fun ml -> 
            match n with
            |   MapNode nd -> Failsafe.validateMappingForDuplicateKeys (MapNode {nd with Data = ml})
            |   _   -> failwith "Expecting a mapping node"
        )
        

    let validateMapping n = 
        (SchemaUtils.getMapNode n) |> List.map(fun (k,v) -> v) |> List.filter(fun n -> n.NodeTag.Uri = MergeGlobalTag.Uri)
        |>  function
            |   []  -> mergeAndValidateMapping n
            |   ml  ->
                ml
                |>  List.map(fun mn -> MessageAtLine.CreateTerminate (mn.ParseInfo.Start) ErrTagConstraint (sprintf "Merge tag or << cannot be used in a mapping value, at position: %s, << can only be used as a maping key." (n.ParseInfo.Start.ToPrettyString())))
                |>  ErrorResult
            
        
    let validateSequence n = 
        (SchemaUtils.getSeqNode n |> List.filter(fun n -> n.NodeTag.Uri = MergeGlobalTag.Uri)) 
        |>  function
            |   []  -> Failsafe.validateUnorderedSequence n
            |   ml  ->
                ml
                |>  List.map(fun mn -> MessageAtLine.CreateTerminate (mn.ParseInfo.Start) ErrTagConstraint (sprintf "Merge tag or << cannot be used in the sequence at position: %s, << can only be used as a maping key." (n.ParseInfo.Start.ToPrettyString())))
                |>  ErrorResult

    let YEMappingGlobalTag = { Failsafe.MappingGlobalTag with TagFunctions = { Failsafe.MappingGlobalTag.TagFunctions with PostProcessAndValidateNode = validateMapping }}
    let YESequenceGlobalTag = { Failsafe.SequenceGlobalTag with TagFunctions = { Failsafe.SequenceGlobalTag.TagFunctions with PostProcessAndValidateNode = validateSequence }}

    let YEFailSafeResolution nst =
        match nst.NodeKind with
        |   Mapping -> Some YEMappingGlobalTag
        |   Sequence-> Some YESequenceGlobalTag
        |   Scalar  -> Some Failsafe.StringGlobalTag

   
    //  order is important, !!pairs is a superset of !!omap
    let providedSeqTags = [OrderedMappingGlobalTag;OrderedPairsGlobalTag; YESequenceGlobalTag]
    let providedScalarTags = [BooleanGlobalTag; IntegerGlobalTag; FloatGlobalTag; TimestampGlobalTag; NullGlobalTag; ValueGlobalTag; MergeGlobalTag; Failsafe.StringGlobalTag]
    let providedMappingTags = [UnOrderedSetGlobalTag;YEMappingGlobalTag]


let private tagResolution (failsafe:TagResolutionInfo->GlobalTag option) (fsMap, fsSeq, fsScal) (mappingTags:GlobalTag list) (seqTags:GlobalTag list) (scalarTags:GlobalTag list) : TagResolutionFunc = fun nst -> 
    match nst.NonSpecificTag with
    |   "!" -> failsafe nst
    |   "?" ->  
        match nst.Content with
        |   MapNode _ -> 
            mappingTags
            |> List.tryFind(fun t -> t.IsMatch (nst.Content))
            |> Option.ifnone(Some fsMap)
        |   SeqNode _ -> 
            seqTags
            |> List.tryFind(fun t -> t.IsMatch (nst.Content))
            |> Option.ifnone(Some fsSeq)
        |   ScalarNode _ -> 
            scalarTags
            |> List.tryFind(fun t -> t.IsMatch (nst.Content))
            |> Option.ifnone(Some fsScal)
    |   _ -> failwith (sprintf "Received illegal non-specific tag: %s" nst.NonSpecificTag)


let private tagResolutionWithDefaultFailsafe = 
    tagResolution Failsafe.defaultFailSafeResolution (Failsafe.MappingGlobalTag, Failsafe.SequenceGlobalTag, Failsafe.StringGlobalTag)


//    Failsafe schema:  http://www.yaml.org/spec/1.2/spec.html#id2802346
let FailsafeSchema =
    {
        GlobalTags = Failsafe.providedTags
        TagResolution = tagResolutionWithDefaultFailsafe [] [] []
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }


//    Json schema:  http://www.yaml.org/spec/1.2/spec.html#id2803231
let JSONSchema =
    {
        GlobalTags = Failsafe.providedTags @ JSON.providedScalarTags 
        TagResolution = 
        // TagResolution
            (fun nst ->
                match (nst.NonSpecificTag, nst.Content) with
                |   ("?", ScalarNode _) -> 
                    JSON.providedScalarTags  |> List.tryFind(fun t -> t.IsMatch (nst.Content))
                |   _ -> tagResolutionWithDefaultFailsafe [] [] JSON.providedScalarTags nst
            )
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }


//  Core Schema:    http://www.yaml.org/spec/1.2/spec.html#id2804923
let YamlCoreSchema =
    {
        GlobalTags = Failsafe.providedTags @ YamlCore.providedScalarTags
        TagResolution = tagResolutionWithDefaultFailsafe [] [] YamlCore.providedScalarTags
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }


//  'Extended' schema (not official name):  http://yaml.org/type/
let YamlExtendedSchema =
    let tagResolutionYamlExtended = tagResolution YamlExtended.YEFailSafeResolution (YamlExtended.YEMappingGlobalTag, YamlExtended.YESequenceGlobalTag, Failsafe.StringGlobalTag)
    {
        //  note that failsafe tags are overriden in this schema        
        GlobalTags = YamlExtended.providedScalarTags @ YamlExtended.providedSeqTags @ YamlExtended.providedMappingTags
        TagResolution = tagResolutionYamlExtended YamlExtended.providedMappingTags YamlExtended.providedSeqTags YamlExtended.providedScalarTags
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }
