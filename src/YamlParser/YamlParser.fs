module YamlParse

open System
open System.Text.RegularExpressions
open YamlParser.Internals
open YamlParser.Internals.ParserMonads
open TagResolution
open RegexDSL
open RepresentationGraph
open System.Diagnostics
open System.IO

exception ParseException of string

type Context = ``Block-out`` | ``Block-in`` | ``Flow-out`` | ``Flow-in`` | ``Block-key`` | ``Flow-key``
type Chomping = ``Strip`` | ``Clip`` | ``Keep``

type TagKind = Verbatim of string | ShortHandNamed of string * string | ShortHandSecondary of string | ShortHandPrimary of string | NonSpecificQT | NonSpecificQM | Empty

let ``start-of-line`` = (* RGP "\n" ||| *) RGP "^"
let ``end-of-file`` = RGP "\\z"


let ``Read stream until stop condition`` (strm:TextReader) (stopCondition:string->bool) =
    let rec reader acc =
        let accRev() = acc |> List.rev
        let str = strm.ReadLine()
        if str = null then (accRev(), None)
        else
            if not(stopCondition str) then reader (str :: acc)
            else (accRev(), Some str)
    reader []


let CreateScalarNode tag d = 
    ScalarNode(NodeData<Node>.Create tag d (lazy(NodeHash.Create d)))


let PlainEmptyNode = CreateScalarNode (NonSpecific.NonSpecificTagQM) ""


let CreateMapNode tag d = 
    MapNode(NodeData<(Node*Node) list>.Create tag d 
        (lazy(d 
              |> List.map(fun (k,_) -> k.Hash.Value)
              |> List.sort
              |> NodeHash.Merge)
        )
    )


let CreateSeqNode tag d = 
    SeqNode(NodeData<Node list>.Create tag d
        (lazy(d 
              |> List.map(fun e -> e.Hash.Value)
              |> List.sort
              |> NodeHash.Merge)
        )
    )


type Directive = YAML of string | TAG of string*string | RESERVED of string list

type ParseMessage = {
        Warn  : string list 
        Error : string list
    }
    with
        static member Create() = {Warn = [];Error=[]}
        member this.AddError s = {this with Error = s :: this.Error}
        member this.AddWarning s = {this with Warn = s :: this.Warn}


type ErrorMessage = string list

[<NoEquality; NoComparison>]
type ParseState = {
        /// Current document line number
        LineNumber  : int
        
        /// String to parse (or what's left of it)
        InputString : string
        
        /// Current Indentation
        n           : int
        
        /// Current Sub-Indentation, nested under ParseState.n
        m           : int
        
        /// Documented context
        c           : Context
        
        /// Chomping type 
        t           : Chomping
        
        /// Document anchors
        Anchors     : Map<string, Node>
        
        /// Parsing messages (warns/errors)
        Messages    : ParseMessage
        
        /// Document directives
        Directives  : Directive list
        
        /// Tag Shorthands
        TagShorthands : TagShorthand list
        
        /// Global Tag Schema
        GlobalTagSchema : GlobalTagSchema
        
        /// Local Tag Schema
        LocalTagSchema : GlobalTagSchema option

        /// Path from root to current
        NodePath : Node list

        /// (dev only) - Trace of success parses
        TraceSuccess: (string*ParseState) list
    }
    with
        static member AddErrorMessageDel (ps:ParseState) sl = (sl |> List.fold(fun (p:ParseState) s -> p.AddErrorMessage s) ps)
        member this.CountLines s =
            let patt = "\u000d\u000a|\u000d|\u000a" // see rule [28] ``b-break``
            let lc = Regex.Matches(s, patt).Count    //  counts \n in a string
            { this with LineNumber = this.LineNumber + lc}

        member this.SetRestString s = { this with InputString = s }
        member this.AddAnchor s n =  {this with Anchors = this.Anchors.Add(s, n)}

        member this.AddAnchors al =
            let newAchors = al |> Map.fold(fun (s:Map<string,Node>) k v -> s.Add(k, v)) this.Anchors
            {this with Anchors = newAchors}

        member this.GetAnchor s =
            if this.Anchors.ContainsKey s then this.Anchors.[s] |> Some
            else None
//            raise (ParseException (sprintf "Referenced anchor '%s' is unknown in line %d" s this.LineNumber))

        [<DebuggerStepThrough>]
        member this.SkipIfMatch p = 
            match (HasMatches(this.InputString, p)) with
            |   (true, mt,frs)  -> 
                let res = this.SetRestString(frs)
                res.CountLines mt
            |   (false, _,_)    -> this

        member  this.SetStyleContext cn = { this with c = cn}

        member this.SetIndent nn = { this with n = nn}

        member this.SetSubIndent mn = { this with m = mn}

        member this.FullIndented = { this with n = this.n + this.m; m=0 }

        member this.SetChomping tn = { this with t = tn }

        member inline this.OneOf with get() = EitherBuilder(this, ParseState.AddErrorMessageDel)

        member this.AddErrorMessage s = {this with Messages = this.Messages.AddError s}
        member this.AddWarningMessage s = {this with Messages = this.Messages.AddWarning s}

        member this.Errors   with get() = this.Messages.Error |> List.length
        member this.Warnings with get() = this.Messages.Error |> List.length

        member this.AddDirective d = {this with Directives = d:: this.Directives}

        member this.AddTagShortHand ts = { this with TagShorthands = (ts :: (this.TagShorthands |> List.filter(fun ol -> ts.ShortHand <> ol.ShortHand))) }

        static member Create inputStr schema = {
                LineNumber = 0; InputString = inputStr ; n=0; m=0; c=``Block-out``; t=``Clip``; 
                Anchors = Map.empty; TraceSuccess = []; Messages=ParseMessage.Create(); Directives=[]; 
                TagShorthands = [TagShorthand.DefaultSecondaryTagHandler];
                GlobalTagSchema = schema; LocalTagSchema = None; NodePath = []
            }

[<NoEquality; NoComparison>]
exception DocumentException of ParseState


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ParseState = 
    let CountLines s (ps:ParseState) = ps.CountLines s
    let SetStyleContext cn (ps:ParseState) = ps.SetStyleContext cn
    let SetIndent nn (ps:ParseState) = ps.SetIndent nn
    let SetSubIndent mn (ps:ParseState) = ps.SetSubIndent mn

    let asc (prs, str, ps) =  { prs with TraceSuccess = (str, ps) :: prs.TraceSuccess }
    let AddSuccessSR str ps pso = pso |> FallibleOption.map(fun (any,prs) -> any, asc (prs, str, ps))
    let AddSuccessMR str ps pso = pso |> FallibleOption.map(fun (any1,any2,prs) -> any1,any2, asc (prs, str, ps))
    let SetRestString s (ps:ParseState) = ps.SetRestString s

    let renv (prt, ps) =
        prt 
        |> SetStyleContext (ps.c)
        |> SetIndent (ps.n)
        |> SetSubIndent (ps.m)
    let ResetEnvSR ps pso = pso |> FallibleOption.map(fun (any, prt) -> (any, (renv(prt,ps))))
    let ResetEnvMR ps pso = pso |> FallibleOption.map(fun (any1, any2, prt) -> (any1, any2, (renv(prt,ps))))
    let ResetLineCount ps = { ps with LineNumber = 0}
    let PreserveErrorsSR ps (ct,pso) = 
        let inErrors = ps.Messages.Error  |> List.sort
        let outErrors = ct.Messages.Error |> List.sort
        let newErrors() = outErrors |> List.filter(fun e1 -> not(inErrors |> List.exists(fun e2 -> e1 = e2)))
        pso |> 
        function 
        |   Value (n,p) -> Value (n,p)  //  errors already preserved in p
        |   NoResult    -> 
            if inErrors = outErrors then NoResult
            else 
                if inErrors.Length > outErrors.Length then 
                    // Assumption: error decreasement only occurs in a "pso = Value v" situation, thus not this case.
                    // Error decrease only happens when an error is cancelled. 
                    //  When something cannot be parsed in one context it may register an erroro (ie single line mode),
                    //  but when it can be parsed in another context (ie multiline mode), the previous registered error is cancelled.
                    raise (ParseException "Unexpected error, losing errors,this needs reassement")
                else 
                    ErrorResult (newErrors())
        |   ErrorResult e -> ErrorResult(newErrors() @ e)

    let PreserveErrorsMR ps (ct,pso) = 
        let inErrors = ps.Messages.Error  |> List.sort
        let outErrors = ct.Messages.Error |> List.sort
        let newErrors() = outErrors |> List.filter(fun e1 -> not(inErrors |> List.exists(fun e2 -> e1 = e2)))
        pso |> 
        function 
        |   Value (n1,n2,p) -> Value (n1,n2,p)  //  errors already preserved in p
        |   NoResult    -> 
            if inErrors = outErrors then NoResult
            else 
                if inErrors.Length > outErrors.Length then 
                    // Assumption: error decreasement only occurs in a "pso = Value v" situation, thus not this case.
                    // Error decrease only happens when an error is cancelled. 
                    //  When something cannot be parsed in one context it may register an erroro (ie single line mode),
                    //  but when it can be parsed in another context (ie multiline mode), the previous registered error is cancelled.
                    raise (ParseException "Unexpected error, losing errors,this needs reassement")
                else 
                    ErrorResult (newErrors())
        |   ErrorResult e -> ErrorResult(newErrors() @ e)

    let AddErrorMessage m (ps:ParseState) = ps.AddErrorMessage m
    let AddWarningMessage m (ps:ParseState) = ps.AddWarningMessage m

    let AddErrorMessageDel m (ps:ParseState) = ParseState.AddErrorMessageDel ps m 
    let AddDirective d (ps:ParseState) = ps.AddDirective d
    let AddAnchorsFrom (prs:ParseState) (ps:ParseState) = ps.AddAnchors prs.Anchors 
    let AddTagShortHand ts (ps:ParseState)  = ps.AddTagShortHand ts

    let inline OneOf (ps:ParseState) = EitherBuilder(ps, ParseState.AddErrorMessageDel)
    let inline ``Match and Advance`` (patt:RGXType) postAdvanceFunc (ps:ParseState) =
        match (HasMatches(ps.InputString, patt)) with
        |   (true, mt, rest) -> ps |> SetRestString rest |> CountLines mt |> postAdvanceFunc
        |   (false, _, _)    -> NoResult

    let inline ``Match and Parse`` (patt:RGXType) parseFunc (ps:ParseState) =
        match (HasMatches(ps.InputString, patt)) with
        |   (true, mt, rest) -> ps |> SetRestString rest |> CountLines mt |> parseFunc mt
        |   (false, _, _)    -> NoResult

type ParseFuncSingleResult = FallibleOption<Node * ParseState, ErrorMessage>        //  returns parsed node, if possible
type ParseFuncMappedResult = FallibleOption<Node * Node * ParseState, ErrorMessage> //  returns parsed key-value pair, if possible
type ParseFuncSignature = (ParseState -> ParseFuncSingleResult)


type FlowFoldPrevType = Empty | TextLine


[<DebuggerStepThrough>]
let (|Regex3|_|) (pattern:RGXType) (ps:ParseState) =
    let m = Regex.Match(ps.InputString, RGS(pattern), RegexOptions.Multiline)
    if m.Success then 
        let lst = [ for g in m.Groups -> g.Value ]
        let fullMatch = lst |> List.head
        let rest = Advance(fullMatch, ps.InputString)
        let groups = lst |> List.tail
        Some(MatchResult.Create fullMatch rest groups, ps |> ParseState.CountLines fullMatch |> ParseState.SetRestString rest)
    else None

type Yaml12Parser(loggingFunction:string->unit) =

    let logger s ps =
        let str = if ps.InputString.Length > 10 then ps.InputString.Substring(0, 10) else ps.InputString
        sprintf "%s\t\"%s\" l:%d i:%d c:%A &a:%d" s (str.Replace("\n","\\n")) (ps.LineNumber) (ps.n) (ps.c) (ps.Anchors.Count) |> loggingFunction

    new() = Yaml12Parser(fun _ -> ())

    //  Utility functions
    member private this.debugbreak sr =
        sr |> FallibleOption.map (fun (n,ps) ->
            logger "break" ps
            (n,ps)
        )

    member this.``split by linefeed`` s = 
        Regex.Split(s, this.``b-as-line-feed``.ToString()) |> List.ofArray

    member this.``filter plain multiline`` (strList: string list) =
        let includedLines = 
            strList.Tail 
            |> Seq.ofList
            |> Seq.takeWhile(fun s -> s = "" || s.StartsWith(" ") || s.StartsWith("\t"))
            |> List.ofSeq
        let rest = String.Join("\n",  strList |> List.skip (1 + includedLines.Length))
        (strList.Head :: includedLines, rest)

    member this.``auto detect indent in block`` n (slst: string list) = 
        let ``match indented content`` s = 
            let icp = GRP(OOM(RGP this.``s-space``)) + OOM(this.``ns-char``)
            match s with
            | Regex2(icp) mt -> Some(mt.ge1.Length - n)
            | _-> None
        slst
        |> List.tryPick(``match indented content``)
        |>  function
            | Some v -> v 
            | None   -> 0

    member this.``auto detect indent in line`` ps =
        let icp = GRP(ZOM(RGP this.``s-space``)) + OOM(this.``ns-char``)
        match ps.InputString with
        | Regex2(icp) mt -> (mt.ge1.Length - ps.n)
        | _-> -1 // raise (ParseException(sprintf "Cannot detect indentation at '%s'" (stringPosition ps.InputString)))


    member this.``content with properties`` (``follow up func``: ParseFuncSignature) ps =
        let addAnchor anchor c (ps:ParseState) = if anchor<> "" then (ps.AddAnchor anchor c) else ps
        match (this.``c-ns-properties`` ps) with
        |   Value(prs, tag, anchor) -> 
            prs 
            |> ``follow up func``
            |>  FallibleOption.bind(fun (c, prs) ->
                let prs = addAnchor anchor c prs
                this.ResolveTag prs tag c
            )
        |   NoResult        -> NoResult
        |   ErrorResult e   -> ErrorResult e


    member this.``join lines`` (strlst:string list) = String.Join("\n", strlst)


    member this.``chomp lines`` ps strlst =
        let stripAll lst = lst |> List.rev |> List.skipWhile(fun s -> String.IsNullOrWhiteSpace(s)) |> List.rev
        match ps.t with
        | ``Strip`` -> strlst |> stripAll
        | ``Clip``  -> 
            if (String.IsNullOrWhiteSpace(List.last strlst)) then 
                List.append (strlst |> stripAll) [""] 
            else 
                strlst 
        | ``Keep``  -> strlst 


    member this.``block fold lines`` ps (strlst: string list) =
        let IsTrimmable s = IsMatch(s, RGSF((this.``s-line-prefix`` ps) ||| (this.``s-indent(<n)`` ps)))
        let IsSpacedText s = IsMatch(s, RGSF(this.``s-nb-spaced-text`` ps))

        let skipIndent s = 
            if IsMatch(s, RGS(this.``s-indent(n)`` ps)) then s.Substring(ps.n)
            else raise (ParseException "Problem with indentation")
        let unIndent s = if s <> "" then skipIndent s else s

        let rec trimLines inLines outLines noFold =
            let result nf = 
                match nf with
                |   true    -> inLines, outLines
                |   false   -> inLines, outLines |> List.tail
            
            match inLines with
            |   []          -> result noFold
            |   h :: rest   ->
                if IsTrimmable h then
                    trimLines rest (h.Trim() :: outLines) noFold
                else    // non empty
                    result (noFold || IsSpacedText h)

        let rec foldEverything inLines outLines folded =
            match inLines with
            |   []          -> outLines |> List.rev
            |   h :: rest   ->
                if IsTrimmable h then
                    let (inl, outl) = trimLines inLines outLines (outLines.Length = 0 || IsSpacedText outLines.Head || rest.Length = 0)
                    foldEverything inl outl true
                else    // non empty
                    if outLines.Length = 0 then
                        foldEverything rest (h :: outLines) false
                    else
                        let prev = List.head outLines
                        if not(IsSpacedText h) && not(folded) && prev <> "" then
                            let foldedout = prev + " " + (skipIndent h) :: List.tail outLines
                            foldEverything rest foldedout false
                        else
                           foldEverything rest (h :: outLines) false
        foldEverything strlst [] false |> List.map(fun s -> unIndent s)


    member this.``flow fold lines`` (convert:string -> string) ps str =
        let rec doFold fst prev (res : string) (lst: string list) =
            match lst with
            |   []  -> 
                let res = if res.EndsWith("\n") then res.Remove(res.Length-1) else res
                match (prev) with
                |   FlowFoldPrevType.Empty  -> res + " "
                |   FlowFoldPrevType.TextLine -> res.ToString()
            |   curr :: tail ->
                let currStr = 
                    (if fst then curr.TrimEnd() 
                    else if tail.Length = 0 then curr.TrimStart()
                    else curr.Trim()) + "\n"

                let currStr = (convert currStr)
                let currType = 
                    if Regex.IsMatch(currStr, RGS(this.``l-empty`` ps)) then 
                        if fst then FlowFoldPrevType.TextLine else FlowFoldPrevType.Empty
                    else FlowFoldPrevType.TextLine

                logger (sprintf "%s\t\t<-%s" (res.ToString().Replace("\n","\\n")) (currStr.Replace("\n","\\n"))) ps

                match (prev) with
                |   FlowFoldPrevType.Empty  -> doFold false currType (res+currStr) tail
                |   FlowFoldPrevType.TextLine -> 
                    let res = 
                        if res.EndsWith("\n") then
                            match currType with
                            |   FlowFoldPrevType.Empty -> res.Remove(res.Length-1)
                            |   FlowFoldPrevType.TextLine -> res.Remove(res.Length-1) + " "
                        else res
                    doFold false currType (res+currStr) tail
        doFold true FlowFoldPrevType.Empty "" str

    member this.``double quote flowfold lines`` ps str =
        let convertDQuoteEscapedMultiLine (str:string) = 
            let SandR =
                // prevent any collision with other escaped chars
                str.Split([|"\\\\"|], StringSplitOptions.RemoveEmptyEntries) 
                |>  List.ofArray
                |>  List.map(DecodeEncodedUnicodeCharacters)
                |>  List.map(DecodeEncodedHexCharacters)
                |>  List.map(DecodeEncodedEscapedCharacters)
                |>  List.map(fun s -> s.Replace("\\\n", ""))
                |>  List.map(fun s -> s.Replace("\x0d\x0a", "\n"))
            String.Join("\\", SandR)    // "\\" in dquote is escaped, and here it is directly converted to "\"
        this.``flow fold lines`` convertDQuoteEscapedMultiLine ps str

    member this.``single quote flowfold lines`` ps str =
        let convertSQuoteEscapedMultiLine (str:string) = 
            str.Replace("''", "'").Replace("\x0d\x0a", "\n")
        this.``flow fold lines`` convertSQuoteEscapedMultiLine ps str

    member this.``plain flow fold lines`` ps str =
        let convertPrainEscapedMultiLine (str:string) = str.Replace("\x0d\x0a", "\n")
        this.``flow fold lines`` convertPrainEscapedMultiLine ps str

    member this.ResolveTag (ps:ParseState) tag (node:Node) : FallibleOption<Node * ParseState, ErrorMessage> =
        let ResolveNonSpecificTag nst = 
            TagResolutionInfo.Create nst (ps.NodePath) (node) (node.Kind)
            |> ps.GlobalTagSchema.TagResolution
            |> function
            |   Some t -> 
                if t.Kind = node.Kind then  Value(node.SetTag (Global t), ps)
                else NoResult
            |   None -> Value(node, ps |> ParseState.AddErrorMessage (sprintf "Cannot resolve secondary tag: %s" nst))

        let ResolveLocalTag tag =
            Value(node.SetTag (Local tag), ps)

        let ResolveShorthand tsh sub =
            match tsh.MappedTagBase with
            |   Regex(RGSF(this.``ns-global-tag-prefix``)) _ -> 
                ps.GlobalTagSchema.GlobalTags 
                |> List.tryFind(fun t -> t.Uri = DecodeEncodedUriHexCharacters(tsh.MappedTagBase+sub))
                |>  function
                    |   Some t  -> if (t.Kind = node.Kind) then Value(Global t) else NoResult
                    |   None    -> 
                        match node.Kind with
                        |   Scalar  -> Unrecognized (GlobalTag.Create (DecodeEncodedUriHexCharacters(tsh.MappedTagBase+sub), node.Kind))
                        |   _       -> Global (GlobalTag.Create (DecodeEncodedUriHexCharacters(tsh.MappedTagBase+sub), node.Kind))
                        |> Value
            |   Regex(RGSF(this.``c-ns-local-tag-prefix``)) _ -> Value(Local (tsh.MappedTagBase+sub))
            |   _ -> NoResult
            |>  FallibleOption.map(fun e -> (node.SetTag e),ps)

        let TryResolveTagShortHand name (sub:string) : FallibleOption<Node * ParseState, ErrorMessage> =
            ps.TagShorthands 
            |> List.tryFind(fun tsh -> tsh.ShortHand = name) 
            |>  function
                | Some tsh -> ResolveShorthand tsh sub
                | None -> NoResult

        let ResolveTagShortHand name (sub:string) : FallibleOption<Node * ParseState, ErrorMessage> =
            ps.TagShorthands 
            |> List.tryFind(fun tsh -> tsh.ShortHand = name) 
            |>  function
                | Some tsh -> ResolveShorthand tsh sub
                | None ->
                    let n = node.SetTag (NonSpecific.UnresolvedTag)
                    let p = ps.AddErrorMessage (sprintf "Cannot resolve shorthand '%s%s'" name sub)
                    Value(n, p)

        match tag with
        |   NonSpecificQM   -> ResolveNonSpecificTag "?"
        |   NonSpecificQT   -> ResolveNonSpecificTag "!"
        |   ShortHandPrimary name -> 
            TryResolveTagShortHand "!" name
            |> FallibleOption.ifnoresult(ResolveLocalTag ("!"+name) )
        |   ShortHandSecondary sub  -> ResolveTagShortHand "!!" sub
        |   ShortHandNamed (name, sub)  -> ResolveTagShortHand name sub
        |   Verbatim name   -> ResolveLocalTag name
        |   TagKind.Empty   -> Value(node,ps)

    member this.ResolvedNullNode (ps:ParseState) =  
        (this.ResolveTag ps NonSpecificQM PlainEmptyNode).Data |> fst // should never be None
          
    //  [1] http://www.yaml.org/spec/1.2/spec.html#c-printable
    member this.``c-printable`` = 
        RGO ("\u0009\u000a\u000d\u0020-\u007e" +   // 8 - bit, #x9 | #xA | #xD | [#x20-#x7E]
             "\u0085\u00a0-\ud7ff\ue000-\ufffd")   // 16- bit, #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD]
                                                   //  32-bit -> currently not supported because .Net does not encode naturally. Yaml: [#x10000-#x10FFFF]

    //  [2] http://www.yaml.org/spec/1.2/spec.html#nb-json
    member ths.``nb-json`` = RGO "\u0009\u0020-\uffff"

    //  [3] http://www.yaml.org/spec/1.2/spec.html#c-byte-order-mark
    member this.``c-byte-order-mark`` = RGP "\ufeff"

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
        logger "s-line-prefix" ps
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
        logger "s-separate" ps
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
    member this.``l-directive`` (ps:ParseState) : FallibleOption<ParseState,ErrorMessage> = 
        ps 
        |> ParseState.``Match and Advance`` (RGP "%") (fun prs ->
            match prs.InputString with
            |   Regex2(this.``ns-yaml-directive``)  mt    -> 
                let ps = 
                    match (mt.ge1.Split('.') |> List.ofArray) with
                    | [a;b] when a="1" && b<"2" -> ps |> ParseState.AddWarningMessage (sprintf "YAML %s document will be parsed as YAML 1.2" mt.ge1)
                    | [a;b] when a="1" && b="2" -> ps
                    | [a;b] when a="1" && b>"2" -> ps |> ParseState.AddWarningMessage (sprintf "YAML %s document will be parsed as YAML 1.2" mt.ge1)
                    | [a;_] when a>"1"          -> ps |> ParseState.AddErrorMessage (sprintf "YAML %s document cannot be parsed, only YAML 1.2 is supported" mt.ge1)
                    | _                         -> ps |> ParseState.AddErrorMessage (sprintf "Illegal directive: %%YAML %s, document cannot be parsed" mt.ge1)
                let ymlcnt = ps.Directives |> List.filter(function | YAML _ -> true | _ -> false) |> List.length
                let ps = 
                    if ymlcnt>0 then (ps |> ParseState.AddErrorMessage "The YAML directive must only be given at most once per document.")
                    else ps
                if ps.Errors >0 then
                    raise (DocumentException ps)
                else
                    Value(YAML(mt.ge1), ps.SetRestString mt.Rest)
            |   Regex2(this.``ns-tag-directive``)   mt    -> 
                let tg = mt.ge2 |> fst
                let ymlcnt = ps.Directives |> List.filter(function | TAG (t,_) ->  (t=tg) | _ -> false) |> List.length
                let ps = 
                    if ymlcnt>0 then (ps |> ParseState.AddErrorMessage (sprintf "You can only add a tag shorthand once: %s" tg))
                    else ps
                if ps.Errors >0 then
                    raise (DocumentException ps)
                else
                    Value(TAG(mt.ge2),  ps.SetRestString (mt.Rest) |> ParseState.AddTagShortHand (TagShorthand.Create (mt.ge2)))
            |   Regex2(this.``ns-reserved-directive``) mt -> Value(RESERVED(mt.Groups), ps |> ParseState.SetRestString mt.Rest |> ParseState.AddWarningMessage (sprintf "Unsupported directive: %%%s" mt.ge1))
            |   _   -> NoResult
        )
        |> FallibleOption.bind(fun (t,prs) ->
            prs |> ParseState.``Match and Advance`` (this.``s-l-comments``) (fun prs2 -> prs2.AddDirective t |> Value)
        )

    //  [83]    http://www.yaml.org/spec/1.2/spec.html#ns-reserved-directive
    member this.``ns-reserved-directive`` = 
        this.``ns-directive-name`` + ZOM(this.``s-separate-in-line`` + this.``ns-directive-parameter``)

    //  [84]    http://www.yaml.org/spec/1.2/spec.html#ns-directive-name
    member this.``ns-directive-name`` = OOM(this.``ns-char``)

    //  [85]    http://www.yaml.org/spec/1.2/spec.html#ns-directive-parameter
    member this.``ns-directive-parameter`` = OOM(this.``ns-char``)

    //  [86]    http://www.yaml.org/spec/1.2/spec.html#ns-yaml-directive
    member this.``ns-yaml-directive`` = RGP("YAML") + this.``s-separate-in-line`` + GRP(this.``ns-yaml-version``)

    //  [87]    http://www.yaml.org/spec/1.2/spec.html#ns-yaml-version
    member this.``ns-yaml-version`` = OOM(this.``ns-dec-digit``) + RGP("\\.") + OOM(this.``ns-dec-digit``)

    //  [88]    http://www.yaml.org/spec/1.2/spec.html#ns-tag-directive
    member this.``ns-tag-directive`` = 
        (RGP "TAG") + this.``s-separate-in-line`` + GRP(this.``c-tag-handle``) + this.``s-separate-in-line`` + GRP(this.``ns-tag-prefix``)

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
    member this.``c-ns-properties`` ps : FallibleOption<ParseState * TagKind * string, ErrorMessage> =
        logger "c-ns-properties" ps
        let ``tag anchor`` = GRP(this.``c-ns-tag-property``) + OPT((this.``s-separate`` ps) + GRP(this.``c-ns-anchor-property``))
        let ``anchor tag`` = GRP(this.``c-ns-anchor-property``) + OPT((this.``s-separate`` ps) + GRP(this.``c-ns-tag-property``))
        let classifyTag t =
            if t = "" then TagKind.Empty
            else
                let verbatim = (RGP "!<") + GRP(OOM(this.``ns-uri-char``)) + (RGP ">")
                let shorthandNamed = GRP(this.``c-named-tag-handle``) + GRP(OOM(this.``ns-tag-char``))
                let shorthandSecondary = this.``c-secondary-tag-handle`` + GRP(OOM(this.``ns-tag-char``))
                let shorthandPrimary = this.``c-primary-tag-handle``+ GRP(OOM(this.``ns-tag-char``))
                match t with
                |   Regex2(verbatim) mr -> Verbatim mr.ge1
                |   Regex2(shorthandNamed) mr -> ShortHandNamed  mr.ge2
                |   Regex2(shorthandSecondary) mr -> ShortHandSecondary  mr.ge1
                |   Regex2(shorthandPrimary) mr -> ShortHandPrimary mr.ge1
                |   Regex2(this.``c-non-specific-tag``) _ -> NonSpecificQT
                |   _ -> raise (ParseException "Unexpected Tag format")

        match ps with
        |   Regex3(``tag anchor``) (mt, prs) -> 
            let (tag, anchor) = mt.ge2
            let classifiedTag = classifyTag tag
            let anchor = if anchor<>"" then anchor.Substring(1) else anchor
            Value(prs, classifiedTag, anchor)
        |   Regex3(``anchor tag``) (mt, prs) -> 
            let (anchor, tag) = mt.ge2
            let classifiedTag = classifyTag tag
            let anchor = if anchor<>"" then anchor.Substring(1) else anchor
            Value(prs, classifiedTag, anchor)
        |   _ -> NoResult

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
        logger "c-ns-alias-node" ps
        ps |> ParseState.``Match and Advance`` (RGP "\\*") (fun prs ->
            prs |> ParseState.``Match and Parse`` (this.``ns-anchor-name``) (fun mt prs2 ->
                let retrievedAnchor = ps.GetAnchor mt
                match retrievedAnchor with
                |   Some ra -> Value(ra, prs2)
                |   None    -> NoResult // //raise (ParseException (sprintf "Referenced anchor '%s' is unknown in line %d" s this.LineNumber))
        ))
        |> ParseState.AddSuccessSR "c-ns-alias-node" ps

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
        logger "c-double-quoted" ps
        //  TODO: maybe SandR the full content, before processing it (see this.``double quote flowfold lines``)
        let convertDQuoteEscapedSingleLine (str:string) = 
            let SandR =
                // prevent any collision with other escaped chars
                str.Split([|"\\\\"|], StringSplitOptions.RemoveEmptyEntries) 
                |>  List.ofArray
                |>  List.map(DecodeEncodedUnicodeCharacters)
                |>  List.map(DecodeEncodedHexCharacters)
                |>  List.map(DecodeEncodedEscapedCharacters)
                |>  List.map(fun s -> s.Replace("\x0d\x0a", "\n"))
                |>  List.map(fun s -> s.Replace("\\\"", "\""))
            String.Join("\\", SandR)    // "\\" in dquote is escaped, and here it is directly converted to "\"

        let processSingleLine prs content =
            content 
            |> convertDQuoteEscapedSingleLine
            |> CreateScalarNode (NonSpecific.NonSpecificTagQT)
            |> this.ResolveTag prs NonSpecificQT

        let processMultiLine prs content =
            content 
            |> this.``split by linefeed``
            |> this.``double quote flowfold lines`` ps
            |> CreateScalarNode (NonSpecific.NonSpecificTagQT)
            |> this.ResolveTag prs NonSpecificQT

        let patt = (RGP "\"") + GRP(this.``nb-double-text`` ps) + (RGP "\"")

        match ps with
        |   Regex3(patt)    (mt,prs) ->
            let content = mt.ge1
            match ps.c with
            |  ``Flow-out`` |  ``Flow-in`` ->   //  multiline
                let lines = content |> this.``split by linefeed`` |> List.length
                if lines = 1 then
                    processSingleLine prs content
                else
                    processMultiLine prs content
            |   ``Block-key`` | ``Flow-key`` -> //  single line
                processSingleLine prs content
            | _  ->  raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")
        |   _ -> NoResult
        |> ParseState.AddSuccessSR "c-double-quoted" ps        

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
        logger "c-single-quoted" ps
        let convertSQuoteEscapedSingleLine (str:string) = str.Replace("''", "'")

        let processSingleLine prs content =
            content 
            |> convertSQuoteEscapedSingleLine
            |> CreateScalarNode (NonSpecific.NonSpecificTagQT)
            |> this.ResolveTag prs NonSpecificQT 
        let processMultiLine prs content =
            content 
            |> this.``split by linefeed``
            |> this.``single quote flowfold lines`` ps
            |> CreateScalarNode (NonSpecific.NonSpecificTagQT)
            |> this.ResolveTag prs NonSpecificQT 

        let patt = (RGP "'") + GRP(this.``nb-single-text`` ps) + (RGP "'")
        match ps with
        |   Regex3(patt)    (mt,prs) ->
            let content = mt.ge1
            match ps.c with
            |  ``Flow-out`` |  ``Flow-in`` ->   //  multiline
                let lines = content |> this.``split by linefeed`` |> List.length
                if lines = 1 then
                    processSingleLine prs content
                else
                    processMultiLine prs content
            |   ``Block-key`` | ``Flow-key`` -> //  single line
                processSingleLine prs content
            | _             ->  raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")
        |   _ -> NoResult
        |> ParseState.AddSuccessSR "c-single-quoted" ps        

    //  [121]   http://www.yaml.org/spec/1.2/spec.html#nb-single-text(n,c)
    member this.``nb-single-text`` ps =
        logger "nb-single-text" ps
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
    member this.``s-single-next-line`` ps = //  note, spec is recursive, below is an attempt to rewrite recursive regex 
        ZOM((this.``s-flow-folded`` ps) + this.``ns-single-char`` + this.``nb-ns-single-in-line``) + (this.``s-flow-folded`` ps) |||
        OOM((this.``s-flow-folded`` ps) + this.``ns-single-char`` + this.``nb-ns-single-in-line``) + ZOM(this.``s-white``)        

    //  [125]   http://www.yaml.org/spec/1.2/spec.html#nb-single-multi-line(n)
    member this.``nb-single-multi-line`` ps = this.``nb-ns-single-in-line`` + ((this.``s-single-next-line`` ps) ||| ZOM(this.``s-white``))

    //  [126]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-first(c)
    member this.``ns-plain-first`` ps = (this.``ns-char`` - this.``c-indicator``) ||| ((RGP "\\?") ||| (RGP ":") ||| (RGP "-")) + (this.``ns-plain-safe`` ps)

    //  [127]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-safe(c)
    member this.``ns-plain-safe`` ps =
        logger "ns-plain-safe" ps
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
    member this.``ns-plain-char`` ps = (this.``ns-char`` + (RGP "#")) ||| ((this.``ns-plain-safe`` ps) - (RGO ":#")) ||| ((RGP ":") + (this.``ns-plain-safe`` ps))

    //  [131]   http://www.yaml.org/spec/1.2/spec.html#ns-plain(n,c)
    member this.``ns-plain`` (ps:ParseState) =
        logger "ns-plain" ps
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
    member this.``in-flow`` (ps:ParseState) =
        match ps.c with
        |   ``Flow-out`` -> ``Flow-in``
        |   ``Flow-in``  -> ``Flow-in``
        |   ``Block-key``-> ``Flow-key``
        |   ``Flow-key`` -> ``Flow-key``
        | _              -> raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")

    //  [137]   http://www.yaml.org/spec/1.2/spec.html#c-flow-sequence(n,c)
    member this.``c-flow-sequence`` (ps:ParseState) : ParseFuncSingleResult=
        logger "c-flow-sequence" ps
        ps |> ParseState.``Match and Advance`` ((RGP "\\[") + OPT(this.``s-separate`` ps)) (fun prs ->
            let prs = prs.SetStyleContext(this.``in-flow`` prs)
            match (this.``ns-s-flow-seq-entries`` prs) with
            |   Value (c, prs2) ->  
                prs2 |> ParseState.``Match and Advance`` (RGP "\\]") (fun psx -> Value (c, psx) ) 
            |   _ ->
                prs |> ParseState.``Match and Advance`` (RGP "\\]") (fun psx -> 
                    CreateSeqNode (NonSpecific.NonSpecificTagQT) [] |> this.ResolveTag psx NonSpecificQT)
        )
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "c-flow-sequence" ps        
                
    //  [138]   http://www.yaml.org/spec/1.2/spec.html#ns-s-flow-seq-entries(n,c)
    member this.``ns-s-flow-seq-entries`` (ps:ParseState) : ParseFuncSingleResult =
        logger "ns-s-flow-seq-entries" ps
        let rec ``ns-s-flow-seq-entries`` (ps:ParseState) (lst:Node list) : ParseFuncSingleResult =
            match (this.``ns-flow-seq-entry`` ps) with
            |   Value (entry, prs) ->
                let lst = entry :: lst
                let prs = prs.SkipIfMatch (OPT(this.``s-separate`` prs))
                let commaPattern = (RGP ",") + OPT(this.``s-separate`` prs)
                match prs with 
                |   Regex3(commaPattern) (_, prs2) -> ``ns-s-flow-seq-entries`` prs2 lst
                |   _ ->  CreateSeqNode (NonSpecific.NonSpecificTagQT) (lst |> List.rev) |> this.ResolveTag prs NonSpecificQT
            |   _ -> 
                if lst.Length = 0 then NoResult   // empty sequence
                else CreateSeqNode (NonSpecific.NonSpecificTagQT) (lst |> List.rev) |> this.ResolveTag ps NonSpecificQT 
        ``ns-s-flow-seq-entries`` ps []
        |> ParseState.AddSuccessSR "ns-s-flow-seq-entries" ps        

    //  [139]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-seq-entry(n,c)
    member this.``ns-flow-seq-entry`` (ps:ParseState) : ParseFuncSingleResult =
        logger "ns-flow-seq-entry" ps

        (ps |> ParseState.OneOf) {
            either(this.``ns-flow-pair`` >> FallibleOption.bind(fun (ck, cv, prs) -> CreateMapNode (NonSpecific.NonSpecificTagQT) [(ck,cv)] |> this.ResolveTag prs NonSpecificQT))
            either(this.``ns-flow-node``)
            ifneither(NoResult)
        }
        |> ParseState.PreserveErrorsSR ps
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "ns-flow-seq-entry"  ps

    //  [140]   http://www.yaml.org/spec/1.2/spec.html#c-flow-mapping(n,c)
    member this.``c-flow-mapping`` (ps:ParseState) : ParseFuncSingleResult =
        logger "c-flow-mapping" ps
        ps |> ParseState.``Match and Advance`` ((RGP "\\{") + OPT(this.``s-separate`` ps)) (fun prs ->
            let prs = prs.SetStyleContext(this.``in-flow`` prs)
            match (this.``ns-s-flow-map-entries`` prs) with
            |   Value (c, prs2) -> 
                prs2 |> ParseState.``Match and Advance`` (RGP "\\}") (fun prs2 -> Value(c, prs2))
            |   _ -> 
                prs |> ParseState.``Match and Advance`` (RGP "\\}") (fun prs2 -> CreateMapNode (NonSpecific.NonSpecificTagQT) [] |> this.ResolveTag prs2 NonSpecificQT)
        )
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "c-flow-mapping" ps       
            
    //  [141]   http://www.yaml.org/spec/1.2/spec.html#ns-s-flow-map-entries(n,c)
    member this.``ns-s-flow-map-entries`` (ps:ParseState) : ParseFuncSingleResult =
        logger "ns-s-flow-map-entries" ps
        let rec ``ns-s-flow-map-entries`` (ps:ParseState) (lst:(Node*Node) list) : ParseFuncSingleResult =
            match (this.``ns-flow-map-entry`` ps) with
            |   Value (ck, cv, prs) ->
                let lst = (ck, cv) :: lst
                let prs = prs.SkipIfMatch (OPT(this.``s-separate`` prs))
                let commaPattern = (RGP ",") + OPT(this.``s-separate`` prs)
                match prs with
                |   Regex3(commaPattern) (_, prs2) -> ``ns-s-flow-map-entries`` prs2 lst
                |   _ -> CreateMapNode (NonSpecific.NonSpecificTagQT) (lst |> List.rev) |> this.ResolveTag prs NonSpecificQT
            |   _ -> 
                if lst.Length = 0 then NoResult   // empty sequence
                else CreateMapNode (NonSpecific.NonSpecificTagQT) (lst |> List.rev) |> this.ResolveTag ps NonSpecificQT
        ``ns-s-flow-map-entries`` ps []
        |> ParseState.AddSuccessSR "ns-s-flow-map-entries" ps        

    //  [142]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-map-entry(n,c)
    member this.``ns-flow-map-entry`` (ps:ParseState) : ParseFuncMappedResult =
        logger "ns-flow-map-entry" ps
        let ``ns-flow-map-explicit-entry`` ps = 
            ps |> ParseState.``Match and Advance`` (RGP "\\?" + (this.``s-separate`` ps)) (fun prs -> this.``ns-flow-map-explicit-entry`` prs)
        (ps |> ParseState.OneOf) {
            either (``ns-flow-map-explicit-entry``)
            either (this.``ns-flow-map-implicit-entry``)
            ifneither (NoResult)        
        }
        |> ParseState.PreserveErrorsMR ps
        |> ParseState.AddSuccessMR "ns-flow-map-entry" ps        

    //  [143]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-map-explicit-entry(n,c)
    member this.``ns-flow-map-explicit-entry`` (ps:ParseState) : ParseFuncMappedResult =
        logger "ns-flow-map-explicit-entry" ps
        match (this.``ns-flow-map-implicit-entry`` ps) with
        |   Value (ck, cv, prs) -> Value(ck, cv, prs)
        |   _ -> Value(this.ResolvedNullNode ps, this.ResolvedNullNode ps, ps)       // ( ``e-node`` + ``e-node``)
        |> ParseState.AddSuccessMR "ns-flow-map-explicit-entry" ps       

    //  [144]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-map-implicit-entry(n,c)
    member this.``ns-flow-map-implicit-entry`` (ps:ParseState) : ParseFuncMappedResult =
        logger "ns-flow-map-implicit-entry" ps
        (ps |> ParseState.OneOf) {
            either (this.``ns-flow-map-yaml-key-entry``)
            either (this.``c-ns-flow-map-empty-key-entry``)
            either (this.``c-ns-flow-map-json-key-entry``)
            ifneither (NoResult)        
        }
        |> ParseState.PreserveErrorsMR ps
        |> ParseState.AddSuccessMR "ns-flow-map-implicit-entry" ps        

    //  [145]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-map-yaml-key-entry(n,c)
    member this.``ns-flow-map-yaml-key-entry`` (ps:ParseState) =
        logger "ns-flow-map-yaml-key-entry" ps
        match (this.``ns-flow-yaml-node`` ps) with
        |   Value (ck, prs) -> 
            let prs = prs.SkipIfMatch (OPT(this.``s-separate`` prs))
            match (this.``c-ns-flow-map-separate-value`` prs) with
            |   Value (cv, prs2) -> Value(ck,cv, prs2)
            |   _ -> Value(ck, this.ResolvedNullNode prs, prs)  //  ``e-node``
        |   _   -> NoResult
        |> ParseState.AddSuccessMR "ns-flow-map-yaml-key-entry" ps     

    //  [146]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-map-empty-key-entry(n,c)
    member this.``c-ns-flow-map-empty-key-entry`` (ps:ParseState) : ParseFuncMappedResult =
        logger "c-ns-flow-map-empty-key-entry" ps
        match (this.``c-ns-flow-map-separate-value`` ps) with
        |   Value (c, prs) -> Value(this.ResolvedNullNode prs, c, prs)   //  ``e-node``
        |   _ -> NoResult
        |> ParseState.AddSuccessMR "c-ns-flow-map-empty-key-entry" ps     

    //  [147]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-map-separate-value(n,c)
    member this.``c-ns-flow-map-separate-value`` (ps:ParseState) : ParseFuncSingleResult =
        logger "c-ns-flow-map-separate-value" ps
        ps |> ParseState.``Match and Advance`` (RGP ":") (fun prs ->
            if IsMatch(prs.InputString, (this.``ns-plain-safe`` prs)) then NoResult
            else
                prs |> ParseState.``Match and Advance`` (this.``s-separate`` prs) (this.``ns-flow-node``)
                |>  function
                    |   NoResult    -> PlainEmptyNode |> this.ResolveTag prs NonSpecificQM  //  ``e-node``
                    |   x   -> x
        )
        |> ParseState.AddSuccessSR "c-ns-flow-map-separate-value" ps     

    //  [148]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-map-json-key-entry(n,c)
    member this.``c-ns-flow-map-json-key-entry`` (ps:ParseState) =
        logger "c-ns-flow-map-json-key-entry" ps
        match (this.``c-flow-json-node`` ps) with
        |   Value (ck, prs) -> 
            let prs = prs.SkipIfMatch (OPT(this.``s-separate`` prs))
            match (this.``c-ns-flow-map-adjacent-value`` prs) with
            |   Value (cv, prs2) -> Value(ck,cv, prs2)
            |   _ -> Value(ck, this.ResolvedNullNode prs, prs)  //  ``e-node``
        |   _    -> NoResult
        |> ParseState.AddSuccessMR "c-ns-flow-map-json-key-entry" ps

    //  [149]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-map-adjacent-value(n,c)
    member this.``c-ns-flow-map-adjacent-value`` (ps:ParseState) =
        logger "c-ns-flow-map-adjacent-value" ps
        ps |> ParseState.``Match and Advance`` (RGP ":") (fun prs ->
            let prs = prs.SkipIfMatch (OPT(this.``s-separate`` ps))
            match (this.``ns-flow-node`` prs) with
            |   Value(c, prs2) -> Value(c, prs2)
            |   _ -> Value(this.ResolvedNullNode prs, prs)  //  ``e-node``        
        )
        |> ParseState.AddSuccessSR "c-ns-flow-map-adjacent-value" ps

    //  [150]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-pair(n,c)
    member this.``ns-flow-pair`` (ps:ParseState) : ParseFuncMappedResult =
        logger "ns-flow-pair" ps
        let ``ns-flow-map-explicit-entry`` (ps:ParseState) = 
            ps |> ParseState.``Match and Advance`` (RGP "\\?" + (this.``s-separate`` ps)) (this.``ns-flow-map-explicit-entry``)
        (ps |> ParseState.OneOf) {
            either (``ns-flow-map-explicit-entry``)
            either (this.``ns-flow-pair-entry``)
            ifneither (NoResult)        
        }
        |> ParseState.PreserveErrorsMR ps
        |> ParseState.AddSuccessMR "ns-flow-pair" ps

    //  [151]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-pair-entry(n,c)
    member this.``ns-flow-pair-entry`` (ps:ParseState) : ParseFuncMappedResult =
        logger "ns-flow-pair-entry" ps
        (ps |> ParseState.OneOf) {
            either (this.``ns-flow-pair-yaml-key-entry``)
            either (this.``c-ns-flow-map-empty-key-entry``)
            either (this.``c-ns-flow-pair-json-key-entry``)
            ifneither (NoResult)        
        }
        |> ParseState.PreserveErrorsMR ps
        |> ParseState.AddSuccessMR "ns-flow-pair-entry" ps

    //  [152]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-pair-yaml-key-entry(n,c)
    member this.``ns-flow-pair-yaml-key-entry`` (ps:ParseState) : ParseFuncMappedResult =
        logger "ns-flow-pair-yaml-key-entry" ps
        let ``ns-s-implicit-yaml-key`` (ps:ParseState) = (this.``ns-s-implicit-yaml-key`` (ps.SetStyleContext ``Flow-key``))
        match (``ns-s-implicit-yaml-key`` ps) with
        |   Value (ck, prs) -> 
            match (this.``c-ns-flow-map-separate-value`` prs) with
            |   Value(cv, prs2) -> Value(ck, cv, prs2)
            |   _ ->    NoResult
        |   _ -> NoResult
        |> ParseState.ResetEnvMR ps
        |> ParseState.AddSuccessMR "ns-flow-pair-yaml-key-entry" ps

    //  [153]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-pair-json-key-entry(n,c)
    member this.``c-ns-flow-pair-json-key-entry`` (ps:ParseState) : ParseFuncMappedResult =
        logger "c-ns-flow-pair-json-key-entry" ps
        match (this.``c-s-implicit-json-key`` (ps.SetStyleContext ``Flow-key``)) with
        |   Value(ck, prs) ->
            match (this.``c-ns-flow-map-adjacent-value`` prs) with
            |   Value(cv, prs2) -> Value(ck, cv, prs2)
            |   NoResult -> NoResult
            |   ErrorResult e -> ErrorResult e
        |   NoResult -> NoResult
        |   ErrorResult e -> ErrorResult e
        |> ParseState.ResetEnvMR ps
        |> ParseState.AddSuccessMR "c-ns-flow-pair-json-key-entry" ps

    //  [154]   http://www.yaml.org/spec/1.2/spec.html#ns-s-implicit-yaml-key(c)
    member this.``ns-s-implicit-yaml-key`` (ps:ParseState) : ParseFuncSingleResult =
        logger "ns-s-implicit-yaml-key" ps
//        let ``n/a`` = 0
//        let ps = ps.SetIndent ``n/a``
        match (this.``ns-flow-yaml-node`` ps) with
        |   Value (ck, prs) -> 
            let prs = prs.SkipIfMatch (OPT(this.``s-separate-in-line``))
            Value(ck, prs)
        |   _ -> NoResult
        |> ParseState.AddSuccessSR "ns-s-implicit-yaml-key" ps

    //  [155]   http://www.yaml.org/spec/1.2/spec.html#c-s-implicit-json-key(c)
    member this.``c-s-implicit-json-key`` (ps:ParseState) : ParseFuncSingleResult = (* At most 1024 characters altogether *)
        logger "c-s-implicit-json-key" ps
//        let ``n/a`` = 0
//        let ps = ps.SetIndent ``n/a``
        match (this.``c-flow-json-node`` ps) with
        |   Value (c, prs) -> 
            let prs = prs.SkipIfMatch (OPT(this.``s-separate-in-line``))
            Value(c, prs)
        |   _   -> NoResult
        |> ParseState.AddSuccessSR  "c-s-implicit-json-key" ps

    //  [156]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-yaml-content(n,c)
    member this.``ns-flow-yaml-content`` (ps:ParseState) : ParseFuncSingleResult = 
        logger "ns-flow-yaml-content" ps
        match ps with
        |   Regex3(this.``ns-plain`` ps) (mt, prs) -> 
            match prs.c with
            | ``Flow-out``  | ``Flow-in``   ->
                let (includedLines, rest) =
                    mt.FullMatch
                    |> this.``split by linefeed``
                    |> this.``filter plain multiline``
                let prs = prs.SetRestString (sprintf "%s%s" rest (prs.InputString))
                includedLines
                |> this.``plain flow fold lines`` prs
                |> CreateScalarNode (NonSpecific.NonSpecificTagQM) 
                |> this.ResolveTag prs NonSpecificQM
            | ``Block-key`` | ``Flow-key``  -> 
                mt.FullMatch
                |> CreateScalarNode (NonSpecific.NonSpecificTagQM) 
                |> this.ResolveTag prs NonSpecificQM
            | _  -> raise(ParseException "The context 'block-out' and 'block-in' are not supported at this point")
        |   _ -> NoResult
        |> ParseState.ResetEnvSR ps 
        |> ParseState.AddSuccessSR  "ns-flow-yaml-content" ps

    //  [157]   http://www.yaml.org/spec/1.2/spec.html#c-flow-json-content(n,c)
    member this.``c-flow-json-content`` (ps:ParseState) : ParseFuncSingleResult =
        logger "c-flow-json-content" ps
        ps.OneOf {
            either (this.``c-single-quoted``)
            either (this.``c-double-quoted``)
            either (this.``c-flow-mapping``)
            either (this.``c-flow-sequence``)
            ifneither (NoResult)
        }
        |> ParseState.PreserveErrorsSR ps
        |> ParseState.AddSuccessSR  "c-flow-json-content" ps
        
    //  [158]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-content(n,c)
    member this.``ns-flow-content`` (ps:ParseState) : ParseFuncSingleResult =
        logger "ns-flow-content" ps
        ps.OneOf {
            either (this.``ns-flow-yaml-content``)
            either (this.``c-flow-json-content``)
            ifneither (NoResult)
        }
        |> ParseState.PreserveErrorsSR ps
        |> ParseState.AddSuccessSR  "ns-flow-content" ps

    //  [159]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-yaml-node(n,c)
    member this.``ns-flow-yaml-node`` (ps:ParseState) : ParseFuncSingleResult =
        logger "ns-flow-yaml-node" ps
        let ``ns-flow-yaml-content`` ps =
            ps 
            |> ParseState.``Match and Advance`` (this.``s-separate`` ps) (this.``ns-flow-yaml-content``)
            |> FallibleOption.ifnoresult (Value (PlainEmptyNode, ps))    //  ``e-scalar`` None

        ps.OneOf {
            either (this.``c-ns-alias-node``)
            either (this.``ns-flow-yaml-content``)
            either (this.``content with properties`` ``ns-flow-yaml-content``)
            ifneither (NoResult)
        }
        |> ParseState.PreserveErrorsSR ps
        |> ParseState.AddSuccessSR  "ns-flow-yaml-node" ps

    //  [160]   http://www.yaml.org/spec/1.2/spec.html#c-flow-json-node(n,c)
    member this.``c-flow-json-node`` (ps:ParseState) : ParseFuncSingleResult =
        logger "c-flow-json-node" ps
        let ``c-flow-json-content`` ps =
            ps |> ParseState.``Match and Advance`` (this.``s-separate`` ps) (this.``c-flow-json-content``)
        ps.OneOf {
            either (this.``c-flow-json-content``)
            either (this.``content with properties`` ``c-flow-json-content``)
            ifneither (NoResult)
        }
        |> ParseState.PreserveErrorsSR ps
        |> ParseState.AddSuccessSR "c-flow-json-node" ps
    
    //  [161]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-node(n,c)
    member this.``ns-flow-node`` (ps:ParseState) : ParseFuncSingleResult =
        logger "ns-flow-node" ps
        let ``ns-flow-content`` ps =
            ps |> ParseState.``Match and Advance`` (this.``s-separate`` ps) (this.``ns-flow-content``)
            |>  FallibleOption.ifnoresult (Value (PlainEmptyNode, ps))    //  ``e-scalar`` None

        ps.OneOf {
            either (this.``c-ns-alias-node``)
            either (this.``ns-flow-content``)
            either (this.``content with properties`` ``ns-flow-content``)
            ifneither (NoResult)
        }
        |> ParseState.PreserveErrorsSR ps
        |> ParseState.AddSuccessSR "ns-flow-node" ps

    //  [162]   http://www.yaml.org/spec/1.2/spec.html#c-b-block-header(m,t)
    member this.``c-b-block-header`` (ps:ParseState) = 
        logger "c-b-block-header" ps
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
        logger "b-chomped-last" ps
        match ps.t with
        |   ``Strip``   -> this.``b-non-content``    ||| RGP("\\z")
        |   ``Clip``    -> this.``b-as-line-feed``   ||| RGP("\\z")
        |   ``Keep``    -> this.``b-as-line-feed``   ||| RGP("\\z")

    //  [166]   http://www.yaml.org/spec/1.2/spec.html#l-chomped-empty(n,t)
    member this.``l-chomped-empty`` (ps:ParseState) =
        logger "l-chomped-empty" ps
        match ps.t with
        |   ``Strip``   -> this.``l-strip-empty`` ps
        |   ``Clip``    -> this.``l-strip-empty`` ps
        |   ``Keep``    -> this.``l-keep-empty`` ps

    //  [167]   http://www.yaml.org/spec/1.2/spec.html#l-strip-empty(n)
    member this.``l-strip-empty`` ps = ZOM((this.``s-indent(<=n)`` ps) + this.``b-non-content``) + OPT(this.``l-trail-comments`` ps)

    //  [168]   http://www.yaml.org/spec/1.2/spec.html#l-keep-empty(n)
    member this.``l-keep-empty`` ps = ZOM(this.``l-empty`` (ps.SetStyleContext ``Block-in``)) + OPT(this.``l-trail-comments`` ps)

    //  [169]   http://www.yaml.org/spec/1.2/spec.html#l-trail-comments(n)
    member this.``l-trail-comments`` ps = (this.``s-indent(<n)`` ps) + this.``c-nb-comment-text`` + this.``b-comment`` + ZOM(this.``l-comment``)

    //  [170]   http://www.yaml.org/spec/1.2/spec.html#c-l+literal(n)
    member this.``c-l+literal`` ps = 
        logger "c-l+literal" ps
        let trimIndent pst (slist: string list) =
            let skipIndent s = 
                match s with
                |   Regex(RGS(this.``s-indent(n)`` pst))    _ -> s.Substring(pst.n)
                |   Regex(RGS(this.``s-indent(<n)`` pst))   _ -> ""
                |   _ -> failwith (sprintf "Problem with indentation: %s" s)
            let unIndent s = if s <> "" then skipIndent s else s

            let ``l-empty`` = RGSF((this.``s-line-prefix`` (pst.SetStyleContext ``Block-in``)) ||| (this.``s-indent(<n)`` pst))
            let ``l-literaltext`` = RGSF((this.``s-indent(n)`` pst) + OOM(this.``nb-char``))

            let trimTail sin sout =
                match sin with
                |   []  -> sout |> List.rev
                |   h :: _ ->
                    let patt = this.``l-chomped-empty`` pst + RGP("\\z")
                    if (h="") || IsMatch(h, patt) then sout |> List.rev
                    else raise (ParseException (sprintf "Unexpected characters"))

            let rec trimMain sin sout =
                match sin with
                |   []  -> sout |> List.rev
                |   h :: rest ->
                    if (h="") then 
                        trimMain rest (unIndent h :: sout)
                    else
                        match h with
                        |   Regex(``l-empty``)       _ -> trimMain rest (unIndent h :: sout)
                        |   Regex(``l-literaltext``) _ -> trimMain rest (unIndent h :: sout)
                        |   _ -> trimTail sin sout

            let rec trimHead sin sout =
                match sin with
                |   []  -> sout |> List.rev
                |   h :: rest ->
                    if (h="") then
                        trimHead rest (unIndent h :: sout)
                    else
                        match h with
                        |   Regex(``l-empty``)  _ -> trimHead rest (unIndent h :: sout)
                        //  todo: more whitespace than indent -> raise
                        |   _ -> trimMain sin sout
            trimHead slist []

        ps |> ParseState.``Match and Advance`` (RGP "\\|") (fun prs ->
            let ``literal-content`` (ps:ParseState) =
                let ps = if ps.n < 1 then (ps.SetIndent 1) else ps
                let p = this.``l-literal-content`` ps
                match ps.InputString  with
                |   Regex2(p)  m -> Value(m.ge1, ps.SetRestString m.Rest)
                |   _ -> NoResult
            let (pm, prs2) = (this.``c-b-block-header`` prs)
            let m = match pm with
                    |   Some(m) -> m
                    |   None    ->
                    match (``literal-content`` prs2) with
                    |   Value(ms, _) ->  
                        let split = ms |> this.``split by linefeed`` 
                        let aut = split |> this.``auto detect indent in block`` prs2.n
                        if aut < 0 then raise (ParseException "Lesser indented than expected")
                        aut
                    |   _  -> raise (ParseException "Could not detect indentation of literal block scalar after '|'")

            match (``literal-content`` (prs2 |> ParseState.SetIndent (prs2.n+m) |> ParseState.SetSubIndent 0)) with
            |   Value(ms, ps2) ->  
                let split = ms |> this.``split by linefeed`` 
                let s = 
                    split 
                    |> trimIndent ps2
                    |> this.``chomp lines`` ps2 
                    |> this.``join lines``
                Value(s, ps2)
            |   x  -> x
        )
        |> ParseState.AddSuccessSR "c-l+literal" ps

    //  [171]   http://www.yaml.org/spec/1.2/spec.html#l-nb-literal-text(n)
    member this.``l-nb-literal-text`` (ps:ParseState) = ZOM(this.``l-empty`` (ps.SetStyleContext ``Block-in``)) + (this.``s-indent(n)`` ps) + OOM(this.``nb-char``)

    //  [172]   http://www.yaml.org/spec/1.2/spec.html#b-nb-literal-next(n)
    member this.``b-nb-literal-next`` ps = this.``b-as-line-feed`` + (this.``l-nb-literal-text`` ps)
    
    //  [173]   http://www.yaml.org/spec/1.2/spec.html#l-literal-content(n,t)
    member this.``l-literal-content`` (ps:ParseState) = 
        GRP(OPT((this.``l-nb-literal-text`` ps) + ZOM(this.``b-nb-literal-next`` ps) + (this.``b-chomped-last`` ps)) + (this.``l-chomped-empty`` ps))

    //  [174]   http://www.yaml.org/spec/1.2/spec.html#c-l+folded(n)
    member this.``c-l+folded`` ps =
        logger "c-l+folded" ps
        ps |> ParseState.``Match and Advance`` (RGP ">") (fun prs ->
            let ``folded-content`` (ps:ParseState) =
                let ps = if ps.n < 1 then ps.SetIndent 1 else ps
                let patt = this.``l-folded-content`` (ps.FullIndented)
                match ps with
                |   Regex3(patt)  (m,p) -> Value(m.ge1, p)
                |   _ -> NoResult
            let (pm, prs2) = (this.``c-b-block-header`` prs)
            let m = match pm with
                    |   Some(m) -> m
                    |   None    ->
                    match (``folded-content`` prs2) with
                    |   Value(ms, _) ->  
                        let split = ms |> this.``split by linefeed`` 
                        let aut = split |> this.``auto detect indent in block`` prs2.n
                        if aut < 0 then raise (ParseException "Lesser indented than expected")
                        aut
                    |   _  -> raise (ParseException "Could not detect indentation of literal block scalar after '>'")

            (``folded-content`` (prs2 |> ParseState.SetIndent (prs2.n+m) |> ParseState.SetSubIndent 0))
            |> FallibleOption.map(fun (ms, ps2) -> 
                let s = 
                    ms 
                    |> this.``split by linefeed`` 
                    |> this.``block fold lines`` ps2
                    |> this.``chomp lines`` ps2 
                    |> this.``join lines``
                (s, ps2)
            )
        )
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "c-l+folded" ps 

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
        logger "l+block-sequence" ps
        let m = this.``auto detect indent in line`` ps
        if m < 1 then NoResult // raise (ParseException "Incorrect indentation")
        else
            let ps = ps.SetSubIndent m
            let rec ``l+block-sequence`` ps (acc: Node list) =
                let contentOrNone = 
                    if (acc.Length = 0) then NoResult
                    else CreateSeqNode (NonSpecific.NonSpecificTagQT) (List.rev acc) |> this.ResolveTag ps NonSpecificQT

                (ps.FullIndented) |> ParseState.``Match and Advance`` (this.``s-indent(n)`` ps.FullIndented) (this.``c-l-block-seq-entry``)
                |>  function
                    |   Value(c, prs2)   -> ``l+block-sequence`` prs2 (c :: acc)
                    |   NoResult            -> contentOrNone
                    |   ErrorResult e -> ErrorResult e
            ``l+block-sequence`` ps []
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "l+block-sequence" ps

    //  [184]   http://www.yaml.org/spec/1.2/spec.html#c-l-block-seq-entry(n)
    member this.``c-l-block-seq-entry`` ps =
        logger "c-l-block-seq-entry" ps

        ps |> ParseState.``Match and Advance`` (RGP("-")) (fun prs ->
            if IsMatch(prs.InputString, (this.``ns-char``)) then // Not followed by an ns-char
                NoResult
            else
                let prs = prs.SetStyleContext ``Block-in``
                this.``s-l+block-indented`` prs
        )
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "c-l-block-seq-entry" ps

    //  [185]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-indented(n,c)
    member this.``s-l+block-indented`` ps =
        logger "s-l+block-indented" ps
        let m = (ps.n) + (this.``auto detect indent in line`` ps)
        let m = if m<0 then 0 else m
        let ps = ps.SetSubIndent m

        let ``indented compact`` ps =
            ps |> ParseState.``Match and Advance`` (this.``s-indent(n)`` (ps.SetIndent ps.m)) (fun prs ->
                let prs = prs |> ParseState.SetIndent (ps.n+1+ps.m) |> ParseState.SetSubIndent 0
                (prs |> ParseState.OneOf) 
                    {
                        either (this.``ns-l-compact-sequence``)
                        either (this.``ns-l-compact-mapping``)
                        ifneither NoResult
                    }
                    |> ParseState.PreserveErrorsSR ps
            )

        (ps |> ParseState.OneOf) {
            either (``indented compact``)
            either (this.``s-l+block-node``)
            ifneither (
                let prs2 = ps.SkipIfMatch (this.``e-node`` + this.``s-l-comments``)
                Value(this.ResolvedNullNode prs2, prs2))
        }
        |> ParseState.PreserveErrorsSR ps
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "s-l+block-indented" ps

    //  [186]   http://www.yaml.org/spec/1.2/spec.html#ns-l-compact-sequence(n)
    member this.``ns-l-compact-sequence`` ps = 
        logger "ns-l-compact-sequence" ps
        let rec ``ns-l-compact-sequence`` ps (acc: Node list) =
            let contentOrNone = 
                if (acc.Length = 0) then NoResult
                else CreateSeqNode (NonSpecific.NonSpecificTagQT) (List.rev acc) |> this.ResolveTag ps NonSpecificQT
            
            ps |> ParseState.``Match and Advance`` (this.``s-indent(n)`` ps) (this.``c-l-block-seq-entry``)
            |>  function
                |   Value(c, prs2) -> ``ns-l-compact-sequence`` prs2 (c :: acc)
                |   NoResult -> contentOrNone
                |   ErrorResult e -> ErrorResult e
        match (this.``c-l-block-seq-entry`` ps) with
        |   Value (c, prs) -> ``ns-l-compact-sequence`` prs [c]
        |   _ ->    NoResult
        |> ParseState.AddSuccessSR "ns-l-compact-sequence" ps

    //  [187]   http://www.yaml.org/spec/1.2/spec.html#l+block-mapping(n)
    member this.``l+block-mapping`` ps =
        logger "l+block-mapping" ps
        let m = this.``auto detect indent in line`` ps
        if m < 1 then NoResult // raise (ParseException "Incorrect indentation")
        else
            let ps = ps.SetSubIndent m
            let rec ``l+block-mapping`` ps (acc:(Node*Node) list) = 
                let contentOrNone = 
                    if (acc.Length = 0) then NoResult
                    else CreateMapNode (NonSpecific.NonSpecificTagQT) (List.rev acc) |> this.ResolveTag ps NonSpecificQT 

                (ps.FullIndented) |> ParseState.``Match and Advance`` (this.``s-indent(n)`` ps.FullIndented) (this.``ns-l-block-map-entry``)
                |>  function
                    |   Value(ck, cv, prs)   ->  ``l+block-mapping`` prs ((ck,cv) :: acc)
                    |   NoResult             ->  contentOrNone
                    |   ErrorResult e -> ErrorResult e
            ``l+block-mapping`` ps []
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "l+block-mapping" ps

    //  [188]   http://www.yaml.org/spec/1.2/spec.html#ns-l-block-map-entry(n)
    member this.``ns-l-block-map-entry`` (ps:ParseState) : ParseFuncMappedResult = 
        logger "ns-l-block-map-entry" ps
        (ps |> ParseState.OneOf) {
            either (this.``c-l-block-map-explicit-entry``)
            either (this.``ns-l-block-map-implicit-entry``)
            ifneither (NoResult)
        }
        |> ParseState.PreserveErrorsMR ps
        |> ParseState.AddSuccessMR "ns-l-block-map-entry" ps

    //  [189]   http://www.yaml.org/spec/1.2/spec.html#c-l-block-map-explicit-entry(n)
    member this.``c-l-block-map-explicit-entry`` (ps:ParseState) : ParseFuncMappedResult=
        logger "c-l-block-map-explicit-entry" ps
        match (this.``c-l-block-map-explicit-key`` ps) with
        |   Value(ck, prs1) ->
            match (this.``l-block-map-explicit-value`` prs1) with
            |   Value (cv, prs1) -> Value(ck, cv, prs1)
            |   _   ->
                prs1 |> ParseState.``Match and Advance`` (this.``e-node``) (fun prs2 -> Value(ck, this.ResolvedNullNode prs2, prs2))
                |>  function
                    |   NoResult    ->  raise (ParseException "Cannot identify mapping value")
                    |   v       ->  v
        |   NoResult -> NoResult
        |   ErrorResult e -> ErrorResult e
        |> ParseState.AddSuccessMR "c-l-block-map-explicit-entry" ps

    //  [190]   http://www.yaml.org/spec/1.2/spec.html#c-l-block-map-explicit-key(n)
    member this.``c-l-block-map-explicit-key`` ps : ParseFuncSingleResult =
        logger "c-l-block-map-explicit-key" ps
        ps |> ParseState.``Match and Advance`` (RGP "\\?") (fun prs ->
            this.``s-l+block-indented`` (prs |> ParseState.SetStyleContext ``Block-out``)
        )
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "c-l-block-map-explicit-key" ps

    //  [191]   http://www.yaml.org/spec/1.2/spec.html#l-block-map-explicit-value(n)
    member this.``l-block-map-explicit-value`` ps = 
        logger "l-block-map-explicit-value" ps
        ps |> ParseState.``Match and Advance`` ((this.``s-indent(n)`` ps) + RGP(":")) (fun prs ->
            this.``s-l+block-indented`` (prs |> ParseState.SetStyleContext ``Block-out``)
        )
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "l-block-map-explicit-value" ps

    //  [192]   http://www.yaml.org/spec/1.2/spec.html#ns-l-block-map-implicit-entry(n)
    member this.``ns-l-block-map-implicit-entry`` ps : ParseFuncMappedResult =
        logger "ns-l-block-map-implicit-entry" ps
        let matchValue (ck, prs) =
            match (this.``c-l-block-map-implicit-value`` prs) with
            |   Value (cv, prs2) -> Value(ck, cv, prs2)
            |   _ ->  NoResult //  raise (ParseException "Expecting an implicit mapping value")
        match (this.``ns-s-block-map-implicit-key`` ps) with
        |   Value (ck, prs1) -> matchValue(ck, prs1)
        |   _   ->
            ps |> ParseState.``Match and Advance`` (this.``e-node``) (fun prs ->
                (this.ResolveTag prs NonSpecificQM PlainEmptyNode) 
                |> FallibleOption.bind(matchValue)
            )
        |> ParseState.AddSuccessMR "ns-l-block-map-implicit-entry" ps

    //  [193]   http://www.yaml.org/spec/1.2/spec.html#ns-s-block-map-implicit-key
    member this.``ns-s-block-map-implicit-key`` ps = 
        logger "ns-s-block-map-implicit-key" ps
        (ps |> ParseState.SetStyleContext ``Block-key`` |> ParseState.OneOf) {
            either (this.``c-s-implicit-json-key``)
            either (this.``ns-s-implicit-yaml-key``)
            ifneither (NoResult)
        }
        |> ParseState.PreserveErrorsSR ps
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "ns-s-block-map-implicit-key" ps

    //  [194]   http://www.yaml.org/spec/1.2/spec.html#c-l-block-map-implicit-value(n)
    member this.``c-l-block-map-implicit-value`` (ps:ParseState) : ParseFuncSingleResult=
        logger "c-l-block-map-implicit-value" ps
        ps |> ParseState.``Match and Advance`` (RGP ":" ) (fun prs ->
            let prs = prs.SetStyleContext ``Block-out``
            match (this.``s-l+block-node`` prs) with
            |   Value (c, prs2) -> Value(c, prs2)
            |   _ ->
                prs |> ParseState.``Match and Advance`` (this.``e-node`` +  this.``s-l-comments``) (fun prs ->
                    this.ResolveTag prs NonSpecificQM PlainEmptyNode
                )
        )
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "c-l-block-map-implicit-value" ps

    //  [195]   http://www.yaml.org/spec/1.2/spec.html#ns-l-compact-mapping(n)
    member this.``ns-l-compact-mapping`` ps =
        logger "ns-l-compact-mapping" ps
        let rec ``ns-l-compact-mapping`` ps (acc: (Node * Node) list) =
            let contentOrNone = 
                if (acc.Length = 0) then NoResult
                else CreateMapNode (NonSpecific.NonSpecificTagQT) (List.rev acc) |> this.ResolveTag  ps NonSpecificQT 

            ps |> ParseState.``Match and Advance`` (this.``s-indent(n)`` ps) (this.``ns-l-block-map-entry``)
            |>  function
                |   Value(ck, cv, prs)  ->  ``ns-l-compact-mapping`` prs ((ck,cv) :: acc)
                |   NoResult            ->  contentOrNone
                |   ErrorResult e       ->  ErrorResult e
        match (this.``ns-l-block-map-entry`` ps) with
        |   Value(ck, cv, prs) -> ``ns-l-compact-mapping`` prs [(ck,cv)]
        |   _ ->    NoResult
        |> ParseState.AddSuccessSR  "ns-l-compact-mapping" ps

    //  [196]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-node(n,c)
    member this.``s-l+block-node`` (ps:ParseState) : ParseFuncSingleResult =
        logger "s-l+block-node" ps
        (ps |> ParseState.OneOf) {
            either (this.``s-l+block-in-block``)
            either (this.``s-l+flow-in-block``)
            ifneither (NoResult)
        }
        |> ParseState.PreserveErrorsSR ps
        |> ParseState.AddSuccessSR  "s-l+block-node" ps

    //  [197]   http://www.yaml.org/spec/1.2/spec.html#s-l+flow-in-block(n)
    member this.``s-l+flow-in-block`` (ps:ParseState) : ParseFuncSingleResult =
        logger "s-l+flow-in-block" ps
        let prs = ps |> ParseState.SetIndent (ps.n + 1) |> ParseState.SetStyleContext ``Flow-out``
        prs |> ParseState.``Match and Advance`` (this.``s-separate`` prs) (fun prs ->
            match (this.``ns-flow-node`` prs) with
            |   Value(c, prs2) -> Value(c, prs2.SkipIfMatch (this.``s-l-comments``)) 
            |   _ -> NoResult
        )
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "s-l+flow-in-block" ps

    //  [198]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-in-block(n,c)
    member this.``s-l+block-in-block`` (ps:ParseState) : ParseFuncSingleResult =
        logger "s-l+block-in-block" ps
        ps.OneOf {
            either (this.``s-l+block-scalar``)
            either (this.``s-l+block-collection``)
            ifneither (NoResult)
        }
        |> ParseState.PreserveErrorsSR ps
        |> ParseState.AddSuccessSR "s-l+block-in-block" ps

    //  [199]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-scalar(n,c)
    member this.``s-l+block-scalar`` (ps:ParseState) : ParseFuncSingleResult =
        logger "s-l+block-scalar" ps
        let psp1 = ps.SetIndent (ps.n + 1)
        let ``literal or folded`` (ps:ParseState) =
            let psp1 = ps.SetIndent (ps.n + 1)
            psp1 |> ParseState.``Match and Advance`` (this.``s-separate`` psp1) (fun prs ->
                let mapScalar (s, prs) = CreateScalarNode (NonSpecific.NonSpecificTagQT) (s), prs // |> this.ResolveTag prs NonSpecificQT
                (prs |> ParseState.SetIndent (prs.n-1) |> ParseState.OneOf)
                    {
                        either   (this.``c-l+literal`` >> FallibleOption.map mapScalar)
                        either   (this.``c-l+folded``  >> FallibleOption.map mapScalar)
                        ifneither NoResult
                    }
                    |> ParseState.PreserveErrorsSR ps
            )
        psp1 |> ParseState.``Match and Advance`` (this.``s-separate`` psp1) (fun prs ->
            let mapScalar (s, prs) = CreateScalarNode (NonSpecific.NonSpecificTagQT) (s) |> this.ResolveTag prs NonSpecificQT
            (prs |> ParseState.SetIndent (prs.n-1) |> ParseState.OneOf)
                {
                    either(this.``content with properties`` ``literal or folded``)
                    either   (this.``c-l+literal`` >> FallibleOption.bind mapScalar)
                    either   (this.``c-l+folded``  >> FallibleOption.bind mapScalar)
                    ifneither NoResult
                }
                |> ParseState.PreserveErrorsSR ps

        )
        |> ParseState.ResetEnvSR ps
        |> ParseState.AddSuccessSR "s-l+block-scalar" ps

    //  [200]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-collection(n,c)
    member this.``s-l+block-collection`` (ps:ParseState) : ParseFuncSingleResult =
        logger "s-l+block-collection" ps
        let omit f d1 _ = f d1
        let psp1 = ps.SetIndent (ps.n + 1)
        let ``seq or map`` (pls:ParseState) =
            let pls = pls.SkipIfMatch(this.``s-l-comments``) 
            let pls = pls.SetIndent (ps.n)
            pls.OneOf {
                either (omit this.``l+block-sequence`` (this.``seq-spaces`` pls))
                either (this.``l+block-mapping``)
                ifneither(NoResult)
            }
            |> ParseState.PreserveErrorsSR ps
            
        let ``optional spaced content with properties`` ps =
            ps |> ParseState.``Match and Advance`` (this.``s-separate`` ps) (this.``content with properties`` ``seq or map``)

        psp1.OneOf {
            either (``optional spaced content with properties``)
            either (``seq or map``)
            ifneither NoResult
        }
        |> ParseState.PreserveErrorsSR ps
        |> ParseState.AddSuccessSR "s-l+block-collection" ps

    //  [201]   http://www.yaml.org/spec/1.2/spec.html#seq-spaces(n,c)
    member this.``seq-spaces`` ps = 
        match ps.c with
        |   ``Block-out``   ->  ps.SetIndent (ps.n-1)
        |   ``Block-in``    ->  ps
        |   _ ->    raise (ParseException "Case is not supported")

    //  [202]   http://www.yaml.org/spec/1.2/spec.html#l-document-prefix
    member this.``l-document-prefix`` = OPT(this.``c-byte-order-mark``) + ZOM(this.``l-comment``)

    //  [203]   http://www.yaml.org/spec/1.2/spec.html#c-directives-end
    member this.``c-directives-end`` = RGP "---"

    //  [204]   http://www.yaml.org/spec/1.2/spec.html#c-document-end
    member this.``c-document-end`` = RGP "\\.\\.\\."

    //  [205]   http://www.yaml.org/spec/1.2/spec.html#l-document-suffix
    member this.``l-document-suffix`` = this.``c-document-end`` + this.``s-l-comments``

    //  [206]   http://www.yaml.org/spec/1.2/spec.html#c-forbidden
    member this.``c-forbidden`` =
        ``start-of-line`` + ( this.``c-directives-end`` ||| this.``c-document-end``) +
        (this.``b-char`` ||| this.``s-white`` ||| ``end-of-file``)

    //  [207]   http://www.yaml.org/spec/1.2/spec.html#l-bare-document
    member this.``l-bare-document`` (ps:ParseState) : ParseFuncSingleResult = 
        logger "l-bare-document" ps
        ps 
        |> ParseState.SetIndent -1
        |> ParseState.SetStyleContext ``Block-in``
        |> this.``s-l+block-node`` (* Excluding c-forbidden content *)
        |> ParseState.AddSuccessSR "l-bare-document" ps

    //  [208]   http://www.yaml.org/spec/1.2/spec.html#l-explicit-document
    member this.``l-explicit-document`` (ps:ParseState) : ParseFuncSingleResult =
        logger "l-explicit-document" ps
        ps 
        |> ParseState.``Match and Advance`` (this.``c-directives-end``) (fun prs ->
            prs.OneOf {
                either(this.``l-bare-document``)
                ifneither (
                    let prs2 = prs.SkipIfMatch (this.``e-node`` + this.``s-l-comments``)
                    this.ResolveTag prs2 NonSpecificQM PlainEmptyNode
                )
            }
            |> ParseState.PreserveErrorsSR ps
        )
        |> ParseState.AddSuccessSR "l-explicit-document" ps

    //  [209]   http://www.yaml.org/spec/1.2/spec.html#l-directive-document
    member this.``l-directive-document`` (ps:ParseState) : ParseFuncSingleResult = 
        logger "l-directive-document" ps
        let rec readDirectives ps =
            match (this.``l-directive`` ps) with
            |   Value psr   -> readDirectives psr
            |   NoResult    -> ps
            |   ErrorResult el -> (el |> List.fold(fun ps e -> ps |> ParseState.AddErrorMessage e) ps)
        let psr = readDirectives ps
        this.``l-explicit-document`` psr
        |> FallibleOption.map(fun (n,psr) -> n, (ps.SetRestString (psr.InputString)))
        |> ParseState.AddSuccessSR "l-directive-document" ps

    //  [210]   http://www.yaml.org/spec/1.2/spec.html#l-any-document
    member this.``l-any-document`` (ps:ParseState) : ParseFuncSingleResult =
        logger "l-any-document" ps
        (ps |> ParseState.ResetLineCount |> ParseState.OneOf) {
            either(this.``l-directive-document``)
            either(this.``l-explicit-document``)
            either(this.``l-bare-document``)
            ifneither NoResult
        }
        |> ParseState.PreserveErrorsSR ps
        |> ParseState.AddSuccessSR "l-any-document" ps

    //  [211]   http://www.yaml.org/spec/1.2/spec.html#l-yaml-stream
    member this.``l-yaml-stream`` (globalTagScema:GlobalTagSchema) (input:string) = 
        let ps = ParseState.Create input globalTagScema
        logger "l-yaml-stream" ps
        ps |> 
        ParseState.``Match and Advance`` (ZOM(this.``l-document-prefix``)) (fun psr ->
            let toEmptyOrSingletonList ps = 
                function
                |   NoResult -> (ps, [])
                |   Value (n, ps2) -> (ps2, [n])
                |   ErrorResult el -> ((el |> List.fold(fun ps e -> ps |> ParseState.AddErrorMessage e) ps), [])

            let rec successorDoc (ps:ParseState, nodes) =
                if ps.InputString.Length > 0 then
                    (ps |> ParseState.OneOf) {
                        either (ParseState.``Match and Advance`` (OOM(this.``l-document-suffix``) + ZOM(this.``l-document-prefix``)) (this.``l-any-document``))
                        either (ParseState.``Match and Advance`` (ZOM(this.``l-document-prefix``)) (this.``l-explicit-document``))
                        ifneither NoResult
                    }
                    |> ParseState.PreserveErrorsSR ps
                    |>  function
                        |   Value (n, ps2) -> successorDoc (ps2, (n :: nodes))
                        |   _   -> (ps |> ParseState.AddErrorMessage "Cannot parse document.", nodes)
                else
                    (ps, nodes)

            psr 
            |>  this.``l-any-document`` 
            |>  toEmptyOrSingletonList psr
            |>  successorDoc
            |>  Value
        )
        |> FallibleOption.map (fun (ps, nodes) -> (nodes |> List.rev, ps))



