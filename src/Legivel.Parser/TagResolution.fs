module Legivel.TagResolution

open System
open Legivel.RepresentationGraph
open Legivel.Utilities.RegexDSL
open ErrorsAndWarnings
open Legivel.Common
open Legivel.Internals
open System.Text.RegularExpressions


[<NoEquality; NoComparison>]
type TagResolutionInfo = {
        NonSpecificTag  : string
        Path            : Node list
        Content         : Node
    }
    with
        static member Create nst p c =
            { NonSpecificTag = nst; Path = p; Content = c }


type TagResolutionFunc = (TagResolutionInfo -> GlobalTag option)
type UnresolvedTagResolutionFunc = (NodeKind -> string -> GlobalTag)


[<NoEquality; NoComparison>]
type GlobalTagSchema = {
    GlobalTags              : GlobalTag list
    TagResolution           : TagResolutionFunc
    UnresolvedResolution    : UnresolvedTagResolutionFunc
    LocalTags               : LocalTagsFuncs
}


module SchemaUtils =
    let getMapNode (n:Node) =
        match n with
        |   MapNode n ->  n.Data 
        |   _    -> failwith "YamlParser Defect: Tag-kind mismatch between node and tag, expecting a Map Node"

    let getSeqNode (n:Node) =
        match n with
        |   SeqNode n ->  n.Data 
        |   _    -> failwith "YamlParser Defect: Tag-kind mismatch between node and tag, expecting a Seq Node"

    let getScalarNode (n:Node) =
        match n with
        |   ScalarNode n ->  n.Data 
        |   _    -> failwith "YamlParser Defect: Tag-kind mismatch between node and tag, expecting a Scalar Node"

    let getMapNodeData (n:Node) =
        match n with
        |   MapNode n ->  n
        |   _    -> failwith "YamlParser Defect: Tag-kind mismatch between node and tag, expecting a Map Node"

    let getSeqNodeData (n:Node) =
        match n with
        |   SeqNode n ->  n
        |   _    -> failwith "YamlParser Defect: Tag-kind mismatch between node and tag, expecting a Seq Node"

    let getScalarNodeData (n:Node) =
        match n with
        |   ScalarNode n ->  n
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

    let tagFormatCheckError (pm:ParseMessage) (n:Node) data =
        function
        |   Some _ -> FallibleOption.Value n, pm
        |   None   -> 
            pm.AddError(MessageAtLine.CreateTerminate (n.ParseInfo.Start) MessageCode.ErrTagBadFormat (lazy sprintf "Incorrect format: '%s', for tag: %s" data (n.NodeTag.ToPrettyString())))
            FallibleOption.ErrorResult(), pm

    let isFormattedScalarValid  (pm:ParseMessage) (n: Node) = 
        let data = getScalarNode n
        n.NodeTag.CanonFn data |> tagFormatCheckError pm n data


    let tagResolution (failsafe:TagResolutionInfo->GlobalTag option) (fsMap, fsSeq, fsScal) (mappingTags:GlobalTag list) (seqTags:GlobalTag list) (scalarTags:GlobalTag list) : TagResolutionFunc = fun nst -> 
        match nst.NonSpecificTag with
        |   "!" -> failsafe nst
        |   "?" ->  
            match nst.Content with
            |   MapNode _ -> 
                mappingTags
                |> List.tryFind(fun t -> t.IsMatch (nst.Content))
                |> Option.ifnone(fun() -> Some fsMap)
            |   SeqNode _ -> 
                seqTags
                |> List.tryFind(fun t -> t.IsMatch (nst.Content))
                |> Option.ifnone(fun() -> Some fsSeq)
            |   ScalarNode _ -> 
                scalarTags
                |> List.tryFind(fun t -> t.IsMatch (nst.Content))
                |> Option.ifnone(fun() -> Some fsScal)
        |   _ -> failwith (sprintf "Received illegal non-specific tag: %s" nst.NonSpecificTag)


module Failsafe =
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

    let validateDuplicateKeys (pm:ParseMessage) n (nlst: Node list) =
        nlst
        |> findDuplicateKeys
        |> function
            |   []  -> FallibleOption.Value n, pm
            |   dupLst ->
                [
                    for kt in dupLst do
                        let rf = kt.Head
                        for n in (kt |> List.skip 1) do
                            yield MessageAtLine.CreateContinue (n.ParseInfo.Start) MessageCode.ErrMapDuplicateKey (lazy sprintf "Duplicate key for node %s at position: %s" (n.ToPrettyString()) (rf.ParseInfo.Start.ToPrettyString()))
                ]
                |>  List.iter(fun e -> pm.AddError e)
                FallibleOption.ErrorResult(), pm


    let validateMappingForDuplicateKeys (pm:ParseMessage) (n: Node) =
            SchemaUtils.getMapNode n |> List.map(fst) |> validateDuplicateKeys pm n


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

    let validateUnorderedSequence  (pm:ParseMessage) (n: Node) = FallibleOption.Value(n), pm

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

    let isScalarValid  (pm:ParseMessage) (n: Node) = FallibleOption.Value(n), pm

    let isScalarMatch n (t:GlobalTag) = 
        match n with
        |   ScalarNode nd ->  IsMatchStr (nd.Data, t.Regex)
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

    let MappingGlobalTag =  GlobalTag.Create("tag:yaml.org,2002:map", NodeKind.Mapping, fsMappingTag)
    let SequenceGlobalTag = GlobalTag.Create("tag:yaml.org,2002:seq", NodeKind.Sequence, fsSequenceTag)
    let StringGlobalTag =   GlobalTag.Create("tag:yaml.org,2002:str", NodeKind.Scalar, fsScalarTag )


    let defaultFailSafeResolution nst =
        match nst.Content.Kind with
        |   NodeKind.Mapping -> Some MappingGlobalTag
        |   NodeKind.Sequence-> Some SequenceGlobalTag
        |   NodeKind.Scalar  -> Some StringGlobalTag

    let providedTags = [MappingGlobalTag; SequenceGlobalTag; StringGlobalTag]

    let tagResolutionWithDefaultFailsafe = 
        SchemaUtils.tagResolution defaultFailSafeResolution (MappingGlobalTag, SequenceGlobalTag, StringGlobalTag)

    //    Failsafe schema:  http://www.yaml.org/spec/1.2/spec.html#id2802346
    let Schema = {
        GlobalTags = providedTags
        TagResolution = tagResolutionWithDefaultFailsafe [] [] []
        UnresolvedResolution = getUnresolvedTag
        LocalTags = localtagFunctions
    }

module NonSpecific =
    let NonSpecificTagQT = TagKind.NonSpecific (LocalTag.Create "!" Failsafe.localtagFunctions)
    let NonSpecificTagQM = TagKind.NonSpecific (LocalTag.Create "?" Failsafe.localtagFunctions)
    let UnresolvedTag = TagKind.NonSpecific (LocalTag.Create "?" Failsafe.localtagFunctions)


module JSON =
    let formattedScalarTag = { Failsafe.fsScalarTag with PostProcessAndValidateNode = SchemaUtils.isFormattedScalarValid }

    let NullGlobalTag =
        GlobalTag.Create("tag:yaml.org,2002:null", NodeKind.Scalar, "null",
            (fun s -> 
                    match s with
                    | Regex "null" _ -> Some "null"
                    | _ -> None
            ), formattedScalarTag
        )

    let BooleanGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:bool", NodeKind.Scalar, "true|false",
            (fun s -> 
                match s with
                | Regex "true" _ -> Some "true"
                | Regex "false" _ -> Some "false"
                | _ -> None
            ), formattedScalarTag
        )

    let IntegerGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:int", NodeKind.Scalar, "[-]?(0|[1-9][0-9]*)",
            (fun s ->
                match s with
                | Regex "^([-])?(0|[1-9][0-9_]*)$" [sign; is] -> sprintf "%+d" (Int64.Parse(String.Concat(sign, is))) |> Some
                | _ -> None
            ), formattedScalarTag
        )

    let FloatGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:float", NodeKind.Scalar, "[-]?(0|[1-9][0-9]*)?(\.[0-9]*)?([eE][-+][0-9]+)?",
            (fun s -> 
                let canonicalSign sign = if sign = "-" then "-" else "+"
                match s with
                | Regex "^([-])?(0|[1-9][0-9]*)?(?:\.([0-9]*))?(?:[eE]([-+])([0-9]+))?$" [sign; mantissa; prec; esign; exp] ->
                    let (fullMantissa, canExp) = SchemaUtils.toFloatComponent mantissa prec
                    let givenExp = int(esign + "0" + exp)
                    let canSign = if fullMantissa = "0" then "+" else canonicalSign sign
                    sprintf "%s0.%se%+04d" canSign fullMantissa (canExp+givenExp) |> Some
                | _ -> None
            ), formattedScalarTag
        )


    let MappingGlobalTag = Failsafe.MappingGlobalTag
    let SequenceGlobalTag = Failsafe.SequenceGlobalTag
    let StringGlobalTag = Failsafe.StringGlobalTag

    let providedScalarTags = [NullGlobalTag; BooleanGlobalTag;IntegerGlobalTag;FloatGlobalTag]

    //    Json schema:  http://www.yaml.org/spec/1.2/spec.html#id2803231
    let Schema = {
        GlobalTags = Failsafe.providedTags @ providedScalarTags 
        TagResolution = 
            (fun nst ->
                match (nst.NonSpecificTag, nst.Content) with
                |   ("?", ScalarNode _) -> 
                    providedScalarTags  |> List.tryFind(fun t -> t.IsMatch (nst.Content))
                |   _ -> Failsafe.tagResolutionWithDefaultFailsafe [] [] providedScalarTags nst
            )
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }


module YamlCore =
    let formattedScalarTag = { Failsafe.fsScalarTag with PostProcessAndValidateNode = SchemaUtils.isFormattedScalarValid }

    let NullGlobalTag =
        GlobalTag.Create("tag:yaml.org,2002:null", NodeKind.Scalar, "~|null|Null|NULL|^$",
            (fun s -> 
                    match s with
                    | Regex "~|null|Null|NULL|^$" _ -> Some "~"
                    | _ -> None
            ), formattedScalarTag
        )

    let BooleanGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:bool", NodeKind.Scalar, "true|True|TRUE|false|False|FALSE",
            (fun s -> 
                match s with
                | Regex "true|True|TRUE"    _ -> Some "true"
                | Regex "false|False|FALSE" _ -> Some "false"
                | _ -> None
            ), formattedScalarTag
        )

    let IntegerGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:int", NodeKind.Scalar, "0o[0-7]+|[-+]?([0-9]+)|0x[0-9a-fA-F]+",
            (fun s ->
                // used for both digit and hex conversion
                let digitToValue c = if c >= 'A' then 10+(int c)-(int 'A') else (int c)-(int '0')
                let convertToCanonical sign number = sprintf "%+d" (Int64.Parse(String.Concat(sign, number.ToString())))
                match s with
                | Regex "^0o([0-7]+)$"  [os] -> 
                    let ps = os.ToCharArray() |> List.ofArray
                    let ic = ps |> List.fold(fun s c -> (s <<< 3) + (digitToValue  c)) 0
                    convertToCanonical "" ic |> Some
                | Regex "^([-+])?([0-9]+)$" [sign; is] -> sprintf "%+d" (Int64.Parse(String.Concat(sign, is))) |> Some
                | Regex "^(0x[0-9a-fA-F]+)$"  [hs] -> 
                    let ps = hs.Substring(2).ToUpper().ToCharArray() |> List.ofArray
                    let ic = ps |> List.fold(fun s c -> (s <<< 4) + (digitToValue  c)) 0
                    convertToCanonical "" ic |> Some
                | _ -> None
            ), formattedScalarTag
        )

    let FloatGlobalTag = 
        let toCanonical s =
            let canonicalSign sign = if sign = "-" then "-" else "+"
            match s with
            | Regex "^([-+])?(0|[1-9][0-9]*)?(?:\\.([0-9]*))?(?:[eE]([-+])([0-9]+))?$" [sign; mantissa; prec; esign; exp] ->
                let (fullMantissa, canExp) = SchemaUtils.toFloatComponent mantissa prec
                let givenExp = int(esign + "0" + exp)
                let canSign = if fullMantissa = "0" then "+" else canonicalSign sign
                sprintf "%s0.%se%+04d" canSign fullMantissa (canExp+givenExp) |> Some
            | Regex "^([-+]?)\\.(?:inf|Inf|INF)$" [sign] ->
                let canSign = canonicalSign sign
                sprintf "%s.inf" canSign |> Some
            | Regex "^(\\.(nan|NaN|NAN))$" _ -> ".nan" |> Some
            | _ -> None

        GlobalTag.Create("tag:yaml.org,2002:float", NodeKind.Scalar, "[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?|[-+]?\\.(inf|Inf|INF)|\\.(nan|NaN|NAN)",
            toCanonical, formattedScalarTag
        )

    let MappingGlobalTag = Failsafe.MappingGlobalTag
    let SequenceGlobalTag = Failsafe.SequenceGlobalTag
    let StringGlobalTag = Failsafe.StringGlobalTag

    let providedScalarTags = [BooleanGlobalTag;IntegerGlobalTag;FloatGlobalTag;NullGlobalTag]

    //  Core Schema:    http://www.yaml.org/spec/1.2/spec.html#id2804923
    let Schema = {
        GlobalTags = Failsafe.providedTags @ providedScalarTags
        TagResolution = Failsafe.tagResolutionWithDefaultFailsafe [] [] providedScalarTags
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }

module YamlExtended =
    let formattedScalarTag = { Failsafe.fsScalarTag with PostProcessAndValidateNode = SchemaUtils.isFormattedScalarValid }

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


    let validateOrderedMappings (pm:ParseMessage) (n:Node) =
        if (isMatchSequenceOfPairs n n.NodeTag) then
            (SchemaUtils.getSeqNode n)
            |> getKeysFromPairs 
            |> Failsafe.validateDuplicateKeys pm n
        else
            pm.AddError <| MessageAtLine.CreateContinue (n.ParseInfo.Start) MessageCode.ErrTagSyntax (lazy sprintf "Construct has incorrect syntax for tag %s until position: %s, 'omap' is a sequence of singular mappings, without duplicates." (n.NodeTag.ToPrettyString()) (n.ParseInfo.End.ToPrettyString()))
            FallibleOption.ErrorResult(), pm

        
    let validateOrderedPairs (pm:ParseMessage) (n:Node) =
        if (isMatchSequenceOfPairs n n.NodeTag) then FallibleOption.Value n, pm
        else 
            pm.AddError <| MessageAtLine.CreateContinue (n.ParseInfo.Start) MessageCode.ErrTagSyntax (lazy sprintf "Construct has incorrect syntax for tag %s until position: %s, 'pairs' is a sequence of singular mappings." (n.NodeTag.ToPrettyString()) (n.ParseInfo.End.ToPrettyString()))
            FallibleOption.ErrorResult(), pm

    let NullGlobalTag =
        GlobalTag.Create("tag:yaml.org,2002:null", NodeKind.Scalar, "~|null|Null|NULL|^$",
            (fun s -> 
                    match s with
                    | Regex "(^(~|null|Null|NULL)$)|^$" _ -> "~" |> Some
                    | _ -> None
            ), formattedScalarTag
        )

    let BooleanGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:bool", NodeKind.Scalar, "y|Y|yes|Yes|YES|n|N|no|No|NO|true|True|TRUE|false|False|FALSE|on|On|ON|off|Off|OFF",
            (fun s -> 
                match s with
                | Regex "^(y|Y|yes|Yes|YES|true|True|TRUE|on|On|ON)$" _ -> Some "true"
                | Regex "^(n|N|no|No|NO|false|False|FALSE|off|Off|OFF)$" _ -> Some "false"
                | _ -> None
            ), formattedScalarTag
        )

    let IntegerGlobalTag = 
        GlobalTag.Create("tag:yaml.org,2002:int", NodeKind.Scalar, "[-+]?0b[0-1_]+|[-+]?0[0-7_]+|[-+]?(0|[1-9][0-9_]*)|[-+]?0x[0-9a-fA-F_]+|[-+]?[1-9][0-9_]*(:[0-5]?[0-9])+",
            (fun s ->
                // used for both digit and hex conversion
                let digitToValue c = if c >= 'A' then 10+(int c)-(int 'A') else (int c)-(int '0')
                let convertToCanonical sign number = sprintf "%+d" (Int64.Parse(String.Concat(sign, number.ToString())))
                match s with
                | Regex "^(?:([-+])?0b([0-1_]+))$" [sign; bs] -> 
                    let ps = bs.Replace("_","").ToCharArray() |> List.ofArray
                    let ic = ps |> List.fold(fun s c -> (s <<< 1) + (digitToValue  c)) 0
                    convertToCanonical sign ic |> Some 
                | Regex "^(?:([-+])?0([0-7_]+))$"  [sign; os] -> 
                    let ps = os.Replace("_","").ToCharArray() |> List.ofArray
                    let ic = ps |> List.fold(fun s c -> (s <<< 3) + (digitToValue  c)) 0
                    convertToCanonical sign ic |> Some 
                | Regex "^(?:([-+])?(0|[1-9][0-9_]*))$" [sign; is] -> sprintf "%+d" (Int64.Parse(String.Concat(sign, is.Replace("_","")))) |> Some 
                | Regex "^(?:([-+])?(0x[0-9a-fA-F_]+))$"  [sign; hs] -> 
                    let ps = hs.Substring(2).ToUpper().Replace("_","").ToCharArray() |> List.ofArray
                    let ic = ps |> List.fold(fun s c -> (s <<< 4) + (digitToValue  c)) 0
                    convertToCanonical sign ic |> Some 
                | Regex "^(?:([-+])?([1-9][0-9_]*(:[0-5]?[0-9])+))$" ssl ->
                    let sign = List.item 0 ssl
                    let ss   = List.item 1 ssl
                    let ps = ss.Replace("_","").Split([|":"|], StringSplitOptions.RemoveEmptyEntries)
                    let ic = ps |> List.ofArray  |> List.fold(fun s t -> (s * 60L) + (Int64.Parse(t))) 0L
                    convertToCanonical sign ic |> Some 
                | _ -> None
            ), formattedScalarTag
        )

    let FloatGlobalTag = 
        let toCanonical s =
            let canonicalSign sign = if sign = "-" then "-" else "+"
            match s with
            | Regex "^(?:([-+])?([0-9][0-9_]*)?\.([0-9.]*)(?:[eE]([-+])([0-9]+))?)$" [sign; mantissa; prec; esign; exp] ->
                let (fullMantissa, canExp) = SchemaUtils.toFloatComponent (mantissa.Replace("_","")) prec
                let givenExp = int(esign + "0" + exp)
                let canSign = if fullMantissa = "0" then "+" else canonicalSign sign
                sprintf "%s0.%se%+04d" canSign fullMantissa (canExp+givenExp) |> Some 
            | Regex "^(?:([-+]?)((?:[0-9][0-9_]*)(?::[0-5]?[0-9])+)\.([0-9_]*))$"  [sign; mantissa; prec] -> 
                let ps = mantissa.Replace("_","").Split([|":"|], StringSplitOptions.RemoveEmptyEntries)
                let ic = ps |> List.ofArray  |> List.fold(fun s t -> (s * 60L) + (Int64.Parse(t))) 0L
                let canSign = canonicalSign sign
                let canMantissa = ic.ToString()
                let canExp = canMantissa.Length
                sprintf "%s0.%s%se%+04d" canSign canMantissa (prec.Replace("_","")) canExp |> Some 
            | Regex "^(?:([-+]?)\.(?:inf|Inf|INF))$" [sign] ->
                let canSign = canonicalSign sign
                sprintf "%s.inf" canSign |> Some 
            | Regex "^(?:(\.(nan|NaN|NAN)))$" _ -> Some ".nan"
            | _ -> None

        GlobalTag.Create("tag:yaml.org,2002:float", NodeKind.Scalar, "^([-+]?([0-9][0-9_]*)?\.[0-9.]*([eE][-+][0-9]+)?|[-+]?[0-9][0-9_]*(:[0-5]?[0-9])+\.[0-9_]*|[-+]?\.(inf|Inf|INF)|\.(nan|NaN|NAN))$",
            toCanonical, formattedScalarTag
        )

    let TimestampGlobalTag = 
        let digit = RGO("0-9")
        let hyphen = RGP("-")
        let rgyear = Repeat(digit,4)
        let rgmonth = digit + OPT(digit)
        let rgmonthf = Repeat(digit,2)
        let rgday = digit + OPT(digit)
        let rgdayf = Repeat(digit,2)
        let rgdate = (GRP rgyear) + hyphen + (GRP rgmonth) + hyphen + (GRP rgday)
        let rgdatef = (GRP rgyear) + hyphen + (GRP rgmonthf) + hyphen + (GRP rgdayf)
        let rghour = digit + OPT(digit)
        let rgmin = Repeat(digit, 2)
        let rgsec = Repeat(digit, 2)
        let rgfrac= ZOM(digit)
        let rgtime = (GRP rghour) + RGP(":") + (GRP rgmin) + RGP(":") + (GRP rgsec) + OPT(RGP "\." + GRP(rgfrac))
        let rgztimez = RGP "Z"
        let rgdtimez = RGO "-+" + rghour + OPT(RGP ":" + rgmin)
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
                ut.ToString("o") |> Some 
            | Regex(RGSF rgISO8601)  [_; year; month; day; hour; min; sec; fraction; tz] -> 
                let tzc = if tz = "" then "Z" else tz
                let dt = DateTime.Parse(canon (ToInt year) (ToInt month) (ToInt day) (ToInt hour) (ToInt min) (ToInt sec) (ToInt fraction) tzc)
                let ut = dt.ToUniversalTime()   // in two steps, or we get a warning
                ut.ToString("o") |> Some 
            | _ -> None

        let validateTimestamp (pm:ParseMessage) n =
            let (isValid,str) = 
                let nd = SchemaUtils.getScalarNode n
                timestampToCanonical nd
                |>  function
                    |   Some strd   -> (DateTime.TryParse(strd) |> fst), nd
                    |   None        -> false, nd
            if isValid then FallibleOption.Value n, pm
            else 
                pm.AddError <| MessageAtLine.CreateContinue (n.ParseInfo.Start) MessageCode.ErrTagBadFormat (lazy sprintf "Timestamp has incorrect format: %s" str)
                FallibleOption.ErrorResult(), pm

        GlobalTag.Create("tag:yaml.org,2002:timestamp", NodeKind.Scalar, RGSF(rgtimestamp),
            (timestampToCanonical), { formattedScalarTag with PostProcessAndValidateNode = validateTimestamp}
        )

    //  http://yaml.org/type/value.html
    let ValueGlobalTag =
        //  this tag only marks !!value, no logic; a native constuctor could/should effect this
        GlobalTag.Create("tag:yaml.org,2002:value", NodeKind.Scalar, "=",
            (fun s -> 
                    match s with
                    | Regex "=" _ -> Some "=" 
                    | _ -> None
            ), formattedScalarTag
        )

    //  http://yaml.org/type/merge.html
    let MergeGlobalTag =
        //  this tag only marks !!merge, the !!map tag should effect this
        GlobalTag.Create("tag:yaml.org,2002:merge", NodeKind.Scalar, "<<",
            (fun s -> 
                    match s with
                    | Regex "<<" _ -> Some "<<"
                    | _ -> None
            ), formattedScalarTag
        )

    //  http://yaml.org/type/binary.html
    let BinaryGlobalTag =
        //  This tag can only be assigned, and is never detected; bc too many collisions with plain text.
        let base64Alphabet = RGO "A-Z" + RGO "a-z" + RGO "0-9" + RGO "+/" + RGO "="
        //  from YamlParser, rules 24-33
        let ``b-line-feed`` = RGP "\u000a"
        let ``b-carriage-return`` = RGP "\u000d"
        let ``b-break`` = 
            (``b-carriage-return`` + ``b-line-feed``)   |||  //  DOS, Windows
            ``b-carriage-return``                       |||  //  MacOS upto 9.x
            ``b-line-feed``                                     //  UNIX, MacOS X
        let ``s-space`` = "\u0020"  // space
        let ``s-tab`` = "\u0009"    // tab
        let ``s-white`` = RGO (``s-space`` + ``s-tab``)
        let controlChar = ``b-break`` ||| ``s-white``
        let allowedChars = OOM(base64Alphabet ||| controlChar)

        let binaryToCanonical s =
            match s with
            | Regex (RGSF allowedChars) [_] -> Regex.Replace(s, RGSFR(controlChar),"") |> Some
            | _ -> None
        GlobalTag.Create("tag:yaml.org,2002:binary", NodeKind.Scalar, RGSF(allowedChars), 
            binaryToCanonical, { formattedScalarTag with IsMatch = fun _ _ -> false })


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

    let validateUnorderedSet (pm:ParseMessage) (n:Node) = 
        let hasNoValues nd =
            nd
            |> List.map(snd)
            |> List.forall(fun (v:Node) -> v.NodeTag = Global NullGlobalTag)
        if isMatchUnorderedSet n (n.NodeTag) then FallibleOption.Value(n), pm
        else
            Failsafe.validateMappingForDuplicateKeys pm n
            |> FallibleOption.bind(fun _ ->
                if (hasNoValues (SchemaUtils.getMapNode n)) then FallibleOption.Value(n), pm
                else 
                    pm.AddError <| MessageAtLine.CreateContinue (n.ParseInfo.Start) MessageCode.ErrTagSyntax (lazy sprintf "Construct has incorrect syntax for tag %s until position: %s, 'set' is a mapping without values, but not all values are null." (n.NodeTag.ToPrettyString()) (n.ParseInfo.End.ToPrettyString()))
                    FallibleOption.ErrorResult(), pm
            )

    let orderedMappingTagFuncs = TagFunctions.Create areOrderedMappingsEqual getOrderedMappingHash validateOrderedMappings isMatchSequenceOfMappings
    let orderedPairsTagFuncs = TagFunctions.Create areOrderedMappingsEqual getOrderedMappingHash validateOrderedPairs isMatchSequenceOfPairs
    let unorderedSetTagFuncs = TagFunctions.Create Failsafe.areUnorderedMappingsEqual Failsafe.getUnorderedMappingHash validateUnorderedSet isMatchUnorderedSet

    //  http://yaml.org/type/omap.html
    let OrderedMappingGlobalTag =  GlobalTag.Create("tag:yaml.org,2002:omap", NodeKind.Sequence, orderedMappingTagFuncs)

    //  http://yaml.org/type/pairs.html
    let OrderedPairsGlobalTag =  GlobalTag.Create("tag:yaml.org,2002:pairs", NodeKind.Sequence, orderedPairsTagFuncs)

    //  http://yaml.org/type/set.html
    let UnOrderedSetGlobalTag =  GlobalTag.Create("tag:yaml.org,2002:set", NodeKind.Mapping, unorderedSetTagFuncs)

    let  mergeAndValidateMapping (pm:ParseMessage) n = 
        let (mn,rn) = (SchemaUtils.getMapNode n) |> List.partition(fun (k,_) -> k.NodeTag.Uri = MergeGlobalTag.Uri)

        let rec merge mlst reslst =
            let mergeMapNode nd rs =
                let areKeysEqual (km:Node) (kr:Node) = kr.NodeTag.AreEqual kr km
                let nodesToMerge = 
                    nd |>  List.filter(fun (km:Node,_) -> rs |> List.exists(fun (kr:Node,_) -> areKeysEqual km kr) |> not)
                nodesToMerge @ rs
            match mlst with
            |   []  -> reslst |> List.rev |> FallibleOption.Value, pm
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
                            |> List.map(fun fn -> MessageAtLine.CreateTerminate (fn.ParseInfo.Start) MessageCode.ErrTagConstraint (lazy sprintf "Incorrect Node type at position: %s, << should map to a sequece of mapping nodes, other types are not allowed in the sequence." (h.ParseInfo.Start.ToPrettyString())))
                            |>  List.iter(fun m -> pm.AddError m)
                            FallibleOption.ErrorResult(), pm
                |   ScalarNode _ -> 
                    pm.AddError <| MessageAtLine.CreateTerminate (n.ParseInfo.Start) MessageCode.ErrTagConstraint (lazy sprintf "Merge tag or << cannot map to a scalar, at position: %s, << should map to a mapping node, or a sequence of mappings." (h.ParseInfo.Start.ToPrettyString()))
                    FallibleOption.ErrorResult(), pm

        merge (mn |> List.map(fun (_,v) -> v)) (rn |> List.rev)
        |>  FallibleOption.bind(fun ml -> 
            match n with
            |   MapNode nd -> Failsafe.validateMappingForDuplicateKeys pm (MapNode {nd with Data = ml})
            |   _   -> failwith "Expecting a mapping node"
        )
        

    let validateMapping (pm:ParseMessage) n = 
        (SchemaUtils.getMapNode n) |> List.map(fun (_,v) -> v) |> List.filter(fun n -> n.NodeTag.Uri = MergeGlobalTag.Uri)
        |>  function
            |   []  -> mergeAndValidateMapping pm n
            |   ml  ->
                ml
                |>  List.iter(fun mn -> 
                    MessageAtLine.CreateTerminate (mn.ParseInfo.Start) MessageCode.ErrTagConstraint (lazy sprintf "Merge tag or << cannot be used in a mapping value, at position: %s, << can only be used as a maping key." (n.ParseInfo.Start.ToPrettyString()))
                    |>  pm.AddError
                )
                FallibleOption.ErrorResult(), pm
            
        
    let validateSequence (pm:ParseMessage) n = 
        (SchemaUtils.getSeqNode n |> List.filter(fun n -> n.NodeTag.Uri = MergeGlobalTag.Uri)) 
        |>  function
            |   []  -> Failsafe.validateUnorderedSequence pm n
            |   ml  ->
                ml
                |>  List.iter(fun mn -> 
                    MessageAtLine.CreateTerminate (mn.ParseInfo.Start) MessageCode.ErrTagConstraint (lazy sprintf "Merge tag or << cannot be used in the sequence at position: %s, << can only be used as a maping key." (n.ParseInfo.Start.ToPrettyString()))
                    |>  pm.AddError
                )
                FallibleOption.ErrorResult(), pm

    let MappingGlobalTag = Failsafe.MappingGlobalTag.SetTagFunctions { Failsafe.MappingGlobalTag.TagFunctions with PostProcessAndValidateNode = validateMapping }
    let SequenceGlobalTag = Failsafe.SequenceGlobalTag.SetTagFunctions { Failsafe.SequenceGlobalTag.TagFunctions with PostProcessAndValidateNode = validateSequence }
    let StringGlobalTag = Failsafe.StringGlobalTag

    let YEFailSafeResolution nst =
        match nst.Content.Kind with
        |   NodeKind.Mapping -> Some MappingGlobalTag
        |   NodeKind.Sequence-> Some SequenceGlobalTag
        |   NodeKind.Scalar  -> Some StringGlobalTag

   
    //  order is important, !!pairs is a superset of !!omap
    let providedSeqTags = [OrderedMappingGlobalTag;OrderedPairsGlobalTag; SequenceGlobalTag]
    let providedScalarTags = [BooleanGlobalTag; IntegerGlobalTag; FloatGlobalTag; TimestampGlobalTag; NullGlobalTag; ValueGlobalTag; MergeGlobalTag; BinaryGlobalTag; Failsafe.StringGlobalTag]
    let providedMappingTags = [UnOrderedSetGlobalTag;MappingGlobalTag]

    let tagResolutionYamlExtended = SchemaUtils.tagResolution YEFailSafeResolution (MappingGlobalTag, SequenceGlobalTag, Failsafe.StringGlobalTag)

    //  'Extended' schema (not official name):  http://yaml.org/type/
    let Schema = {
        //  note that failsafe tags are overriden in this schema        
        GlobalTags = providedScalarTags @ providedSeqTags @ providedMappingTags
        TagResolution = tagResolutionYamlExtended providedMappingTags providedSeqTags providedScalarTags
        UnresolvedResolution = Failsafe.getUnresolvedTag
        LocalTags = Failsafe.localtagFunctions
    }

