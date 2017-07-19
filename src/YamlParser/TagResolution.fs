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
        match n with
        |   MapNode n ->  
            (lazy(n.Data 
                  |> List.map(fun (k,_) -> k.Hash)
                  |> List.sort
                  |> NodeHash.Merge)
            )
        |   _    -> failwith "Tag-kind mismatch between node and tag"


    let validateDuplicateKeys n (nlst: Node list) =
        let areEqual (nl:Node list) (n: Node) =
            if nl.Length = 0 then false
            else n.NodeTag.AreEqual (nl.Head) n
        nlst
        |> List.groupBy(fun k -> k.Hash, k.NodeTag)
        |> List.map(snd)
        |> List.map(fun nl -> if (nl |> List.forall(fun n -> areEqual nl n)) then nl else [])
        |> List.filter(fun kl -> kl.Length > 1)
        |> function
            |   []  -> Value n
            |   dupLst ->
                let errs = [
                    for kt in dupLst do
                        let rf = kt.Head
                        for n in (kt |> List.skip 1) do
                            yield MessageAtLine.Create (n.ParseInfo.Start) ErrMapDuplicateKey (sprintf "Duplicate key for node %s at position: %s" (n.ToPrettyString()) (rf.ParseInfo.Start.ToPrettyString()))
                    ]
                ErrorResult errs



    let validateMappingForDuplicateKeys (n: Node) =
        match n with
        |   MapNode nd ->  
            let lrv = nd.Data
            lrv |> List.map(fst) |> validateDuplicateKeys n
        |   _    -> failwith "Tag-kind mismatch between node and tag"

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
        match n with
        |   SeqNode n ->  
            (lazy(n.Data 
                  |> List.map(fun e -> e.Hash)
                  |> List.sort
                  |> NodeHash.Merge)
            )
        |   _    -> failwith "Tag-kind mismatch between node and tag"

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

    let getScalarHash (n:Node) = 
        match n with
        |   ScalarNode n ->  (lazy(NodeHash.Create (n.Data)))
        |   _    -> failwith "Tag-kind mismatch between node and tag"

    let isScalarValid (n: Node) = Value(n)

    let isScalarMatch n t = 
        match n with
        |   ScalarNode nd ->  IsMatch (nd.Data, t.Regex)
        |   _    -> false

    let isNoMatch _ _ = false

    let fsMappingTag = TagFunctions.Create areUnorderedMappingsEqual getUnorderedMappingHash validateMappingForDuplicateKeys isNoMatch
    let fsSequenceTag = TagFunctions.Create areUnorderedSequencesEqual getUnorderedSeqenceHash isUnorderedSequenceValid isNoMatch
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
                    | _ -> raise (TagResolutionException (sprintf "Cannot convert to null: %s" s))
            ), Failsafe.fsScalarTag
        )

    let BooleanGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:bool", Scalar, "true|false",
            (fun s -> 
                match s with
                | Regex "true" _ -> "true"
                | Regex "false" _ -> "false"
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to boolean: %s" s))
            ), Failsafe.fsScalarTag
        )

    let IntegerGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:int", Scalar, "[-]?(0|[1-9][0-9]*)",
            (fun s ->
                match s with
                | Regex "^([-])?(0|[1-9][0-9_]*)$" [sign; is] -> sprintf "%+d" (Int32.Parse(String.Concat(sign, is)))
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to integer: %s" s))
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
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to float: %s" s))
            ), Failsafe.fsScalarTag
        )

    let providedTags = [NullGlobalTag; BooleanGlobalTag;IntegerGlobalTag;FloatGlobalTag]

module internal YamlCore =
    let NullGlobalTag =
        GlobalTag.Create("tag:yaml.org,2002:null", Scalar, "~|null|Null|NULL|^$",
            (fun s -> 
                    match s with
                    | Regex "~|null|Null|NULL|^$" _ -> "null"
                    | _ -> raise (TagResolutionException (sprintf "Cannot convert to null: %s" s))
            ), Failsafe.fsScalarTag
        )

    let BooleanGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:bool", Scalar, "true|True|TRUE|false|False|FALSE",
            (fun s -> 
                match s with
                | Regex "true|True|TRUE"    _ -> "true"
                | Regex "false|False|FALSE" _ -> "false"
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to boolean: %s" s))
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
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to integer: %s" s))
            ), Failsafe.fsScalarTag
        )

    let FloatGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:float", Scalar, "[-+]?(0|[1-9][0-9]*)?(\.[0-9]*)?([eE][-+][0-9]+)?|[-+]?\.(inf|Inf|INF)|\.(nan|NaN|NAN)",
            (fun s -> 
                let canonicalSign sign = if sign = "-" then "-" else "+"
                match s with
                | Regex "^([-+])?(0|[1-9][0-9]*)?(?:\.([0-9]*))?(?:[eE]([-+])([0-9]+))?$" [sign; mantissa; prec; esign; exp] ->
                    let canExp = int(esign + "0" + exp) + (mantissa.Length)
                    let fullMantissa = clearTrailingZeros (mantissa + prec)
                    let canSign = if fullMantissa = "0" then "+" else canonicalSign sign
                    sprintf "%s0.%se%+04d" canSign fullMantissa canExp
                | Regex "^([-+]?)\.(?:inf|Inf|INF)$" [sign] ->
                    let canSign = canonicalSign sign
                    sprintf "%s.inf" canSign
                | Regex "^(\.(nan|NaN|NAN))$" _ -> ".nan"
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to float: %s" s))
            ), Failsafe.fsScalarTag
        )

    let providedTags = [NullGlobalTag; BooleanGlobalTag;IntegerGlobalTag;FloatGlobalTag]

module internal YamlExtended =
    let areOrderedMappingsEqual (n1:Node) (n2:Node) = 
        n1.Hash = n2.Hash &&
        n1.Kind = n2.Kind &&
        n1.NodeTag = n2.NodeTag &&
        match (n1,n2) with
        |   (MapNode mn1, MapNode mn2)  ->
            mn1.Data.Length = mn2.Data.Length &&
            //  to check equality of ordered content, we keep the order of entry
            mn1.Data
            |> List.zip mn2.Data
            |> List.forall(fun ((kl,vl),(kr,vr)) -> kl.NodeTag.AreEqual kl kr && vl.NodeTag.AreEqual vl vr)
        |   _ -> false

    let getOrderedMappingHash (n:Node) =
        match n with
        |   MapNode n ->  
            (lazy(n.Data 
                  |> List.map(fun (k,_) -> k.Hash)
                  |> NodeHash.Merge)
            )
        |   _    -> failwith "YamlParser defect: Tag-kind mismatch between node and tag"


    let isMatchOrderedMapping (n:Node) t = 
        let isSingleMapping (ns:Node) =
            match ns with
            |   MapNode nd ->  nd.Data.Length = 1
            |   _ -> false
        match n with
        |   SeqNode nd -> nd.Data |> List.forall(isSingleMapping)
        |   _    -> false
        

    let validateOrderedMapping (n:Node) =
        let getKeys nl =
            nl
            |> List.map(fun n ->
                match n with
                |   MapNode nd ->  nd.Data |> List.map(fst)
                |   _ -> failwith "YamlParser defect: Expecting MapNode."
            )
            |>  List.concat

        if (isMatchOrderedMapping n n.NodeTag) then
            match n with
            |   SeqNode nd ->
                nd.Data 
                |> getKeys 
                |> Failsafe.validateDuplicateKeys n
            |   _    -> failwith "YamlParser defect: Tag-kind mismatch between node and tag"
        else
            ErrorResult [MessageAtLine.Create (n.ParseInfo.Start) ErrTagSyntax (sprintf "Construct has incorrect syntax for tag %s until position: %s, an 'omap' is a sequence of singular mappings." (n.NodeTag.ToPrettyString()) (n.ParseInfo.End.ToPrettyString()))]
        

    let BooleanGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:bool", Scalar, "y|Y|yes|Yes|YES|n|N|no|No|NO|true|True|TRUE|false|False|FALSE|on|On|ON|off|Off|OFF",
            (fun s -> 
                match s with
                | Regex "y|Y|yes|Yes|YES|true|True|TRUE|on|On|ON" _ -> "true"
                | Regex "n|N|no|No|NO|false|False|FALSE|off|Off|OFF" _ -> "false"
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to boolean: %s" s))
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
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to integer: %s" s))
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
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to float: %s" s))
            ), Failsafe.fsScalarTag
        )

    let yeOMappingTag = TagFunctions.Create areOrderedMappingsEqual getOrderedMappingHash validateOrderedMapping isMatchOrderedMapping

    let OrderedMappingGlobalTag =  GlobalTag.Create("tag:yaml.org,2002:omap", Sequence, yeOMappingTag)

    let providedScalarTags = [BooleanGlobalTag; IntegerGlobalTag; FloatGlobalTag]
    let providedSeqTags = [OrderedMappingGlobalTag]


//    Failsafe schema:  http://www.yaml.org/spec/1.2/spec.html#id2802346
let FailsafeSchema =
    let TagResolution : TagResolutionFunc = fun nst -> 
        match nst.NonSpecificTag with
        |   "?" ->  None
        |   "!" ->
            match nst.NodeKind with
            |   Mapping  -> Some Failsafe.MappingGlobalTag
            |   Sequence -> Some Failsafe.SequenceGlobalTag
            |   Scalar   -> Some Failsafe.StringGlobalTag
        |   _ -> raise (TagResolutionException (sprintf "Received illegal non-specific tag: %s" nst.NonSpecificTag))
    {
        GlobalTags = Failsafe.providedTags
        TagResolution = TagResolution
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }


//    Json schema:  http://www.yaml.org/spec/1.2/spec.html#id2803231
let JSONSchema =
    let TagResolution : TagResolutionFunc = fun nst -> 
        match nst.NonSpecificTag with
        |   "!" ->
            match nst.NodeKind with
            |   Mapping  -> Some Failsafe.MappingGlobalTag
            |   Sequence -> Some Failsafe.SequenceGlobalTag
            |   Scalar   -> Some Failsafe.StringGlobalTag
        |   "?" ->  
            match nst.Content with
            |   MapNode _ -> Some Failsafe.MappingGlobalTag
            |   SeqNode _ -> Some Failsafe.SequenceGlobalTag
            |   ScalarNode data -> 
                JSON.providedTags  |> List.tryFind(fun t -> t.IsMatch (nst.Content))
                |>  function
                    |   None -> raise (TagResolutionException (sprintf "Unrecognized type for: %s" data.Data))
                    |   x -> x
        |   _ -> raise (TagResolutionException (sprintf "Received illegal non-specific tag: %s" nst.NonSpecificTag))
    {
        GlobalTags = Failsafe.providedTags @ JSON.providedTags 
        TagResolution = TagResolution
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }


let YamlCoreSchema =
    let TagResolution : TagResolutionFunc = fun nst -> 
        match nst.NonSpecificTag with
        |   "!" ->
            match nst.NodeKind with
            |   Mapping  -> Some Failsafe.MappingGlobalTag
            |   Sequence -> Some Failsafe.SequenceGlobalTag
            |   Scalar   -> Some Failsafe.StringGlobalTag
        |   "?" ->  
            match nst.Content with
            |   MapNode _ -> Some Failsafe.MappingGlobalTag
            |   SeqNode _ -> Some Failsafe.SequenceGlobalTag
            |   ScalarNode _ -> 
                YamlCore.providedTags 
                |> List.tryFind(fun t -> t.IsMatch (nst.Content))
                |> Option.ifnone(Some (Failsafe.StringGlobalTag))
        |   _ -> raise (TagResolutionException (sprintf "Received illegal non-specific tag: %s" nst.NonSpecificTag))
    {
        GlobalTags = Failsafe.providedTags @ YamlCore.providedTags
        TagResolution = TagResolution
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }


let YamlExtendedSchema =
    let TagResolution : TagResolutionFunc = fun nst -> 
        match nst.NonSpecificTag with
        |   "!" ->
            match nst.NodeKind with
            |   Mapping  -> Some Failsafe.MappingGlobalTag
            |   Sequence -> 
                YamlExtended.providedSeqTags
                |> List.tryFind(fun t -> t.IsMatch (nst.Content))
                |> Option.ifnone(Some Failsafe.SequenceGlobalTag)
            |   Scalar   -> Some Failsafe.StringGlobalTag
        |   "?" ->  
            match nst.Content with
            |   MapNode _ -> Some Failsafe.MappingGlobalTag
            |   SeqNode _ -> Some Failsafe.SequenceGlobalTag
            |   ScalarNode _ -> 
                YamlExtended.providedScalarTags
                |> List.tryFind(fun t -> t.IsMatch (nst.Content))
                |> Option.ifnone(Some (Failsafe.StringGlobalTag))
        |   _ -> raise (TagResolutionException (sprintf "Received illegal non-specific tag: %s" nst.NonSpecificTag))
    {
        GlobalTags = Failsafe.providedTags @ YamlExtended.providedScalarTags @ YamlExtended.providedSeqTags
        TagResolution = TagResolution
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }
