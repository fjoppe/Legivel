module TagResolution

open System
open RepresentationGraph
open RegexDSL


exception TagResolutionException of string


type TagResolutionInfo = {
        NonSpecificTag  : string
        Path            : Node list
        Content         : Node
        NodeKind        : NodeKind
    }
    with
        static member Create nst p c nk =
            { NonSpecificTag = nst; Path = p; Content = c; NodeKind = nk }


type TagResolutionFunc = (TagResolutionInfo -> Tag option)


type GlobalTagSchema = {
    GlobalTags      : Tag list
    TagResolution   : TagResolutionFunc
}


type TagShorthand = {
        ShortHand : string
        MappedTagUri : string
    }
    with
        static member Create (short, full) = { ShortHand = short; MappedTagUri = full}
        static member DefaultSecondaryTagHandler = { ShortHand = "!!" ; MappedTagUri = "tag:yaml.org,2002:"}


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

module internal NonSpecific =
    let NonSpecificTagQT = Tag.Create("!", Scalar, "")
    let NonSpecificTagQM = Tag.Create("?", Scalar, "")
    let UnresolvedTag = Tag.Create("?", Scalar, "unresolved")


module internal Failsafe =
    let MappingGlobalTag =  Tag.Create("tag:yaml.org,2002:map", Mapping)
    let SequenceGlobalTag = Tag.Create("tag:yaml.org,2002:seq", Sequence)
    let StringGlobalTag =   Tag.Create("tag:yaml.org,2002:str", Scalar)


module internal JSON =
    let NullGlobalTag =
        Tag.Create("tag:yaml.org,2002:null", Scalar, "null",
            (fun s -> 
                    match s with
                    | Regex "null" _ -> "null"
                    | _ -> raise (TagResolutionException (sprintf "Cannot convert to null: %s" s))
            )
        )

    let BooleanGlobalTag = 
        Tag.Create("tag:yaml.org,2002:bool", Scalar, "true|false",
            (fun s -> 
                match s with
                | Regex "true" _ -> "true"
                | Regex "false" _ -> "false"
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to boolean: %s" s))
            )
        )

    let IntegerGlobalTag = 
        Tag.Create("tag:yaml.org,2002:int", Scalar, "[-]?(0|[1-9][0-9]*)",
            (fun s ->
                match s with
                | Regex "^([-])?(0|[1-9][0-9_]*)$" [sign; is] -> sprintf "%+d" (Int32.Parse(String.Concat(sign, is)))
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to integer: %s" s))
            )
        )

    let FloatGlobalTag = 
        Tag.Create("tag:yaml.org,2002:float", Scalar, "[-]?(0|[1-9][0-9]*)?(\.[0-9]*)?([eE][-+][0-9]+)?",
            (fun s -> 
                let canonicalSign sign = if sign = "-" then "-" else "+"
                match s with
                | Regex "^([-])?(0|[1-9][0-9]*)?(?:\.([0-9]*))?(?:[eE]([-+])([0-9]+))?$" [sign; mantissa; prec; esign; exp] ->
                    let canExp = int(esign + "0" + exp) + (mantissa.Length)
                    let fullMantissa = clearTrailingZeros (mantissa + prec)
                    let canSign = if fullMantissa = "0" then "+" else canonicalSign sign
                    sprintf "%s0.%se%+04d" canSign fullMantissa canExp
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to float: %s" s))
            )
        )

module internal YamlCore =
    let NullGlobalTag =
        Tag.Create("tag:yaml.org,2002:null", Scalar, "~|null|Null|NULL|^$",
            (fun s -> 
                    match s with
                    | Regex "~|null|Null|NULL|^$" _ -> "null"
                    | _ -> raise (TagResolutionException (sprintf "Cannot convert to null: %s" s))
            )
        )

    let BooleanGlobalTag = 
        Tag.Create("tag:yaml.org,2002:bool", Scalar, "true|True|TRUE|false|False|FALSE",
            (fun s -> 
                match s with
                | Regex "true|True|TRUE"    _ -> "true"
                | Regex "false|False|FALSE" _ -> "false"
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to boolean: %s" s))
            )
        )

    let IntegerGlobalTag = 
        Tag.Create("tag:yaml.org,2002:int", Scalar, "0o[0-7]+|[-+]?([0-9]+)|0x[0-9a-fA-F]+",
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
            )
        )

    let FloatGlobalTag = 
        Tag.Create("tag:yaml.org,2002:float", Scalar, "[-+]?(0|[1-9][0-9]*)?(\.[0-9]*)?([eE][-+][0-9]+)?|[-+]?\.(inf|Inf|INF)|\.(nan|NaN|NAN)",
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
            )
        )


module internal YamlExtended =
    let BooleanGlobalTag = 
        Tag.Create("tag:yaml.org,2002:bool", Scalar, "y|Y|yes|Yes|YES|n|N|no|No|NO|true|True|TRUE|false|False|FALSE|on|On|ON|off|Off|OFF",
            (fun s -> 
                match s with
                | Regex "y|Y|yes|Yes|YES|true|True|TRUE|on|On|ON" _ -> "true"
                | Regex "n|N|no|No|NO|false|False|FALSE|off|Off|OFF" _ -> "false"
                | _ -> raise (TagResolutionException (sprintf "Cannot convert to boolean: %s" s))
            )
        )

    let IntegerGlobalTag = 
        Tag.Create("tag:yaml.org,2002:int", Scalar, "[-+]?0b[0-1_]+|[-+]?0[0-7_]+|[-+]?(0|[1-9][0-9_]*)|[-+]?0x[0-9a-fA-F_]+|[-+]?[1-9][0-9_]*(:[0-5]?[0-9])+",
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
            )
        )

    let FloatGlobalTag = 
        Tag.Create("tag:yaml.org,2002:float", Scalar, "[-+]?([0-9][0-9_]*)?\.[0-9.]*([eE][-+][0-9]+)?|[-+]?[0-9][0-9_]*(:[0-5]?[0-9])+\.[0-9_]*|[-+]?\.(inf|Inf|INF)|\.(nan|NaN|NAN)",
            (fun s -> 
                let canonicalSign sign = if sign = "-" then "-" else "+"
                match s with
                | Regex "^([-+])?(0*)([1-9][0-9_]*)?\.(0*)([1-9][0-9.]*)(?:[eE]([-+])([0-9]+))?$" [sign; zmantissa; mantissa; zprec; prec; esign; exp] ->
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
            )
        )


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
        GlobalTags = [Failsafe.MappingGlobalTag; Failsafe.SequenceGlobalTag; Failsafe.StringGlobalTag]
        TagResolution = TagResolution
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
                [JSON.NullGlobalTag; JSON.BooleanGlobalTag;JSON.IntegerGlobalTag;JSON.FloatGlobalTag]
                |> List.tryFind(fun t -> IsMatch(data.Data, t.Regex))
                |> function
                |   Some v  -> Some v
                |   None    -> raise (TagResolutionException (sprintf "Unrecognized type for: %s" data.Data))
        |   _ -> raise (TagResolutionException (sprintf "Received illegal non-specific tag: %s" nst.NonSpecificTag))
    {
        GlobalTags = 
            [
                Failsafe.MappingGlobalTag; Failsafe.SequenceGlobalTag; Failsafe.StringGlobalTag
                JSON.NullGlobalTag; JSON.BooleanGlobalTag; JSON.IntegerGlobalTag; JSON.FloatGlobalTag
            ]
        TagResolution = TagResolution
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
            |   ScalarNode data -> 
                [YamlCore.NullGlobalTag; YamlCore.BooleanGlobalTag;YamlCore.IntegerGlobalTag;YamlCore.FloatGlobalTag]
                |> List.tryFind(fun t -> IsMatch(data.Data, t.Regex))
                |> function
                |   Some v -> Some v
                |   None -> Some (Failsafe.StringGlobalTag)
        |   _ -> raise (TagResolutionException (sprintf "Received illegal non-specific tag: %s" nst.NonSpecificTag))
    {
        GlobalTags = 
            [
                Failsafe.MappingGlobalTag; Failsafe.SequenceGlobalTag; Failsafe.StringGlobalTag
                YamlCore.NullGlobalTag; YamlCore.BooleanGlobalTag; YamlCore.IntegerGlobalTag; YamlCore.FloatGlobalTag
            ]
        TagResolution = TagResolution
    }
