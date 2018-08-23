module Legivel.Parser

open System
open System.Text.RegularExpressions
open Legivel.Common
open Legivel.Tokenizer
open Legivel.Internals.ParserMonads
open Legivel.TagResolution
open Legivel.Utilities.RegexDSL
open Legivel.RepresentationGraph
open Legivel.Internals
open ErrorsAndWarnings
open System.Diagnostics
open System.IO
open System.Collections.Generic


exception ParseException of string


type Context = 
    | ``Block-out`` = 0
    | ``Block-in``  = 1
    | ``Flow-out``  = 2
    | ``Flow-in``   = 3
    | ``Block-key`` = 4
    | ``Flow-key``  = 5

type Chomping = 
    | ``Strip`` = 0
    | ``Clip``  = 1
    | ``Keep``  = 2

type TagKind = Verbatim of string | ShortHandNamed of string * string | ShortHandSecondary of string | ShortHandPrimary of string | NonSpecificQT | NonSpecificQM | Empty


let ``start-of-line`` = RGP ("^", [Token.NoToken])
let ``end-of-file`` = RGP ("\\z", [Token.NoToken])


let ``Read stream until stop condition`` (strm:TextReader) (stopCondition:string->bool) =
    let rec reader acc =
        let accRev() = acc |> List.rev
        let str = strm.ReadLine()
        if str = null then (accRev(), None)
        else
            if not(stopCondition str) then reader (str :: acc)
            else (accRev(), Some str)
    reader []


let CreateScalarNode tag pi d = 
    ScalarNode(NodeData<Node>.Create tag d pi)


let PlainEmptyNode pi = CreateScalarNode (NonSpecific.NonSpecificTagQM) pi ""


let CreateMapNode tag pi d  = 
    MapNode(NodeData<(Node*Node) list>.Create tag (d|>List.rev) pi)


let CreateSeqNode tag pi d  = 
    SeqNode(NodeData<Node list>.Create tag (d|>List.rev) pi)

type Directive = YAML of string | TAG of string*string | RESERVED of string list


type ParseRestrictions = {
        AllowedMultiLine : bool
    }
    with
        static member Create() = { AllowedMultiLine = true }


type ParseMessage = {
        Warn  : MessageAtLineList
        Error : MessageAtLineList
        Cancel: DocumentLocation // to cancel errors
        Terminates : MessageAtLineList
    }
    with
        static member Create() = {Warn = MessageAtLineList();Error=MessageAtLineList(); Cancel=DocumentLocation.Empty; Terminates=MessageAtLineList()}
        member this.AddError (mal:MessageAtLine)   = 
            this.Error.Add mal
            if mal.Action = MessageAction.Terminate then this.Terminates.Add mal
        member this.AddWarning (mal:MessageAtLine) = 
            this.Warn.Add mal
        member this.AddCancel mal   = {this with Cancel = mal }


[<NoEquality; NoComparison>]
type RollingTokenizer = private {
        Data        : RollingStream<TokenData> 
        ContextPosition    : int
    }
    with
        member this.Position 
            with get() = this.Data.Position
            and  set v = this.Data.Position <- v
        member this.Reset() = this.Data.Position <- this.ContextPosition
        member this.Advance() = { this with ContextPosition = this.Data.Position}
        member this.EOF = this.Data.EOF
        member this.Peek(n) = this.Data.Peek(n)
        member this.Peek()  = this.Data.Peek()
        static member Create i = { Data = RollingStream<_>.Create (tokenProcessor i) (TokenData.Create (Token.EOF) ""); ContextPosition = 0}


type ScalarMemoizeKey = {
    SreamPositionStart : int
    FunctionNumber     : int
}
with
    static member Create p f = { SreamPositionStart = p; FunctionNumber = f }


type ScalarMemoizeValue = {
    SreamPositionEnd : int
    Value            : FallibleOption<Node>
    PositionDelta    : DocumentLocation
    Length           : int
}
with
    static member Create p s d l = { SreamPositionEnd = p; Value = s; PositionDelta=d; Length=l }


[<NoEquality; NoComparison>]
type ParseState = {
        //  Document Location
        Location : DocumentLocation

        //  track string length
        TrackLength : int

        /// String to parse (or what's left of it)
        Input       : RollingTokenizer
        
        /// Current Indentation
        n           : int
        
        /// Indentation levels
        IndentLevels: int list

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
        
        ///// Global Tag Schema
        //GlobalTagSchema : GlobalTagSchema
        
        /// Local Tag Schema
        LocalTagSchema : GlobalTagSchema option

        /// Path from root to current
        NodePath : Node list

        /// Report on tag processing
        TagReport   : TagReport

        /// Context sensitive restrictions 
        Restrictions : ParseRestrictions

        ///// Memoize scalar values by position and regex.
        //Caching : Dictionary<ScalarMemoizeKey, ScalarMemoizeValue>
    }
    with
        static member AddErrorMessageDel (ps:ParseState) sl = 
            sl 
            |> List.iter(fun s -> ps.AddErrorMessage s |> ignore)
            ps

        static member PositionDelta s =
            let patt = "\u000d\u000a|\u000d|\u000a" // see rule [28] ``b-break``
            let lines = Regex.Split(s, patt) |> List.ofArray
            let lc = (lines.Length-1)  //  counts \n in a string
            let lcc = (lines |> List.last).Length // local column count
            lc, lcc

        member this.SetPositionDelta sl lc lcc =
            let cc = if lc > 0 then 1 + lcc else this.Location.Column + lcc
            { this with Location = this.Location.AddAndSet lc cc; TrackLength = this.TrackLength + sl }

        member this.TrackPosition s =
            ParseState.PositionDelta s ||> this.SetPositionDelta (s.Length)

        static member HasNoTerminatingError (ps:ParseState) = ps.Messages.Terminates.Count = 0

        member this.Advance() = { this with Input = this.Input.Advance() }

        member this.AddAnchor s n =  {this with Anchors = this.Anchors.Add(s, n)}

        member this.AddAnchors al =
            let newAchors = al |> Map.fold(fun (s:Map<string,Node>) k v -> s.Add(k, v)) this.Anchors
            {this with Anchors = newAchors}

        member this.GetAnchor s =
            if this.Anchors.ContainsKey s then this.Anchors.[s] |> Some
            else None

        member this.MarkParseRange pss =
            pss
            //let filterRange msl =
            //    msl
            //    |> List.filter(fun (m:MessageAtLine) -> m.Location >= pss.Location && m.Location <= this.Location)
            
            //(this.Messages.Error |> Map.toList |> List.map snd |> List.collect id) @ this.Messages.Warn
            //|> filterRange
            //|> List.fold(fun (s:ParseState) i -> s.AddCancelMessage (i.Location)) this


        [<DebuggerStepThrough>]
        member this.SkipIfMatch p = 
            match (HasMatches(this.Input.Data, p)) with
            |   (true, mt)  -> 
                let nxt = this.Advance()
                (nxt.TrackPosition mt).MarkParseRange this
            |   (false, _)    -> this
        
        member this.SetStyleContext cn = { this with c = cn}

        member this.SetIndent nn = { this with n = nn; IndentLevels = this.n :: this.IndentLevels}

        member this.SetSubIndent mn = { this with m = mn}

        member this.FullIndented = { this with n = this.n + this.m; m=0 }

        member this.SetChomping tn = { this with t = tn }

        member inline this.OneOf with get() = EitherBuilder(this, (fun ps -> ps.Input.Reset()), (fun ps -> ps.Advance()), ParseState.HasNoTerminatingError)

        member this.AddErrorMessage (s:MessageAtLine) = this.Messages.AddError s; FallibleOption<_>.ErrorResult()
        member this.AddWarningMessage (s:MessageAtLine) = this.Messages.AddWarning s; FallibleOption<_>.NoResult()
        member this.AddCancelMessage s = {this with Messages = this.Messages.AddCancel s}

        member this.Errors   with get() = this.Messages.Error.Count
        member this.Warnings with get() = this.Messages.Warn.Count

        member this.AddDirective d = {this with Directives = d:: this.Directives}
        member this.SetDirectives d = { this with Directives = d }

        member this.AddTagShortHand ts = { this with TagShorthands = (ts :: (this.TagShorthands |> List.filter(fun ol -> ts.ShortHand <> ol.ShortHand))) }

        member this.UpdateTagReport tr = {this with TagReport = tr }

        member this.ResetRestrictions() = {this with Restrictions = ParseRestrictions.Create()}

        member this.ResetTrackLength() = { this with TrackLength = 0 }

        static member Create inputStr = {
                Location = DocumentLocation.Create 1 1; Input = RollingTokenizer.Create inputStr ; n=0; m=0; c=Context.``Block-out``; t=Chomping.``Clip``; 
                Anchors = Map.empty; Messages=ParseMessage.Create(); Directives=[]; 
                TagShorthands = [TagShorthand.DefaultSecondaryTagHandler];
                LocalTagSchema = None; NodePath = [];
                TagReport = TagReport.Create (Unrecognized.Create 0 0) 0 0
                Restrictions = ParseRestrictions.Create(); TrackLength = 0
                IndentLevels = []
                ; (*Caching = Dictionary()*)
            }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ParseState = 
    let TrackPosition s (ps:ParseState) = ps.TrackPosition s
    let SetStyleContext cn (ps:ParseState) = ps.SetStyleContext cn
    let SetIndent nn (ps:ParseState) = ps.SetIndent nn
    let SetSubIndent mn (ps:ParseState) = ps.SetSubIndent mn
    let RevokeIndentation idl (ps:ParseState) =
        {ps with IndentLevels = idl }

    let Advance (ps:ParseState) = ps.Advance()

    let renv (prt, ps) =
        prt 
        |> SetStyleContext (ps.c)
        |> SetIndent (ps.n)
        |> SetSubIndent (ps.m)
        |> RevokeIndentation (ps.IndentLevels)

    let ResetEnv ps pso = pso |> FallibleOption.map(fun (any, prt) -> any, (renv(prt,ps)))
    let ResetDocumentParseState ps = 
        { ps with 
            ParseState.Location = (DocumentLocation.Create 1 1)
            n=0; m=0; c=Context.``Block-out``; t=Chomping.``Clip``; 
            Anchors = Map.empty; Messages=ParseMessage.Create(); 
            Directives=[]; 
            TagShorthands = [TagShorthand.DefaultSecondaryTagHandler];
            LocalTagSchema = ps.LocalTagSchema; 
            NodePath = []
        }

    let AddErrorMessage m (ps:ParseState) = ps.AddErrorMessage m
    let AddErrorMessageList el (ps:ParseState) = 
        el |> List.iter(fun e -> ps |> AddErrorMessage e |> ignore)
        FallibleOption<_>.ErrorResult()
    let AddWarningMessage m (ps:ParseState) = ps.AddWarningMessage m
    let AddCancelMessage m (ps:ParseState) = ps.AddCancelMessage m

    let PreserveNoResult outErrors =
        if not(outErrors.HasErrorOccurred) then FallibleOption<_>.NoResult()
        else FallibleOption<_>.ErrorResult()

    let PreserveErrors _ ct = 
        ct.Result.Result |> 
        function 
        |   FallibleOption.Value  -> 
            let (n,p) = ct.Result.Data
            FallibleOption<_>.Value (n,p)  //  errors already preserved in p
        |   FallibleOption.NoResult    -> PreserveNoResult(ct)
        |   FallibleOption.ErrorResult -> PreserveNoResult(ct)
        |   _ -> failwith "Illegal value"

    let TrackParseLocation ps (pso:FallibleOption<_>) =
        match pso.Result with
        |   FallibleOption.Value -> 
            let (n, psr) = pso.Data
            FallibleOption<_>.Value(n,psr |> AddCancelMessage (ps.Location))
        |   _ -> pso

    let MarkParseRange (pss:ParseState) (pso:FallibleOption<_>) =
        match pso.Result with
        |   FallibleOption.Value -> 
            let (n, (psr:ParseState)) = pso.Data
            FallibleOption<_>.Value(n, psr.MarkParseRange pss)
        |   _ -> pso

    let AddErrorMessageDel m (ps:ParseState) = ParseState.AddErrorMessageDel ps m 
    let AddDirective d (ps:ParseState) = ps.AddDirective d
    let AddAnchorsFrom (prs:ParseState) (ps:ParseState) = ps.AddAnchors prs.Anchors 
    let AddTagShortHand ts (ps:ParseState)  = ps.AddTagShortHand ts

    let IncUnresolved (ps:ParseState) = ps.UpdateTagReport {ps.TagReport with Unresolved = ps.TagReport.Unresolved + 1}
    let IncUnavailable (ps:ParseState) = ps.UpdateTagReport {ps.TagReport with Unavailable = ps.TagReport.Unavailable + 1}
    let IncUnrecognizedScalar (ps:ParseState) = ps.UpdateTagReport {ps.TagReport with Unrecognized = {ps.TagReport.Unrecognized with Scalar = ps.TagReport.Unrecognized.Scalar + 1}}
    let IncUnrecognizedCollection (ps:ParseState) = ps.UpdateTagReport {ps.TagReport with Unrecognized = {ps.TagReport.Unrecognized with Collection = ps.TagReport.Unrecognized.Collection + 1}}


    let inline OneOf (ps:ParseState) = EitherBuilder(ps, (fun ps -> ps.Input.Reset()), (fun ps -> ps.Advance()), ParseState.HasNoTerminatingError)

    let ``Match and Advance`` (patt:RGXType) postAdvanceFunc (ps:ParseState) =
        match (HasMatches(ps.Input.Data, patt)) with
        |   (true, mt) -> ps |> AddCancelMessage (ps.Location) |> Advance |> TrackPosition mt |> postAdvanceFunc
        |   (false, _)    -> ps.Input.Reset(); FallibleOption<_>.NoResult()


    let inline ``Match and Parse`` (patt:RGXType) parseFunc (ps:ParseState) =
        match (HasMatches(ps.Input.Data, patt)) with
        |   (true, mt) -> ps |> Advance |> TrackPosition mt |> parseFunc mt
        |   (false, _)    -> ps.Input.Reset(); FallibleOption<_>.NoResult()


    let ProcessErrors (ps:ParseState) =
        ps.Messages.Error
        |>  Seq.iter(fun m -> if ps.Messages.Cancel < m.Location then ps.Messages.Error.Remove(m) |> ignore)
        let freeForm = 
            ps.Messages.Error
            |>  Seq.filter(fun m -> m.Code = MessageCode.Freeform) 
            |>  Seq.distinctBy(fun m -> m.Location.Line,m.Location.Column,m.Message)
        let filteredErrors = 
            ps.Messages.Error
            |>  Seq.filter(fun m -> m.Code <> MessageCode.Freeform) 
            |>  Seq.distinctBy(fun m -> m.Location)
        let newErrors = freeForm |>  Seq.append filteredErrors
        ps.Messages.Error.Clear()
        ps.Messages.Error.AddRange(newErrors)
        {ps with Messages = {ps.Messages with Cancel = DocumentLocation.Empty}}


    let PostProcessErrors fr =
        fr
        |> FallibleOption.map(fun (node, ps:ParseState) ->(node,ProcessErrors ps))
        |> FallibleOption.map(fun (node, ps:ParseState) ->
            let rg = RGP("---", [Token.``t-hyphen``]) ||| RGP("...", [Token.``t-dot``])
            let p = ps.Input.Position
            let rs =
                AssesInput ps.Input.Data rg 
                |>  function
                    |   (false,_) -> (node,ps)
                    |   (true, _) -> 
                        let errs = 
                            ps.Messages.Error 
                            |>  Seq.filter(fun m -> m.Action = MessageAction.Continue && m.Location <> ps.Location)
                        ps.Messages.Error.Clear()
                        ps.Messages.Error.AddRange(errs)
                        (node,ps)
            ps.Input.Position <- p
            rs
        )

    let ToRepresentation (pso:ParseState) (no:FallibleOption<_>)  =
        let mal2pm (ml:MessageAtLine seq) = (ml |> List.ofSeq |> List.map(fun w -> ParseMessageAtLine.Create (w.Location) (w.Message.Force())))
        let wr = pso.Messages.Warn  |> mal2pm
        match no.Result with
        |   FallibleOption.NoResult    -> EmptyRepresentation(EmptyDocumentResult.Create wr pso.Location), pso
        |   FallibleOption.ErrorResult ->
            let er = pso.Messages.Error |> mal2pm
            let erss = ErrorResult.Create wr er (pso.Location) 
            NoRepresentation(erss),pso
        |   FallibleOption.Value  ->
            let (n, (ps2:ParseState)) = no.Data
            let wr2 = wr @ (ps2.Messages.Warn |> mal2pm)
            if ps2.Errors > 0 then
                let er = ps2.Messages.Error |> mal2pm
                let erss = ErrorResult.Create wr2 er (ps2.Location) 
                NoRepresentation(erss),ps2
            else
                if (ps2.TagReport.Unrecognized.Scalar > 0 || ps2.TagReport.Unresolved > 0) then
                    let prr = ParsedDocumentResult.Create wr2 (ps2.TagReport) (ps2.Location) (ps2.TagShorthands) n
                    PartialRepresentaton(prr),ps2
                else
                    let crr = ParsedDocumentResult.Create wr2 (ps2.TagReport) (ps2.Location) (ps2.TagShorthands) n
                    CompleteRepresentaton(crr),ps2
        |   _   -> failwith "Illegal value no"

    let RestrictMultiLine ps = {ps with Restrictions = {ps.Restrictions with AllowedMultiLine = false}}
    let ResetRestrictions (ps:ParseState) = ps.ResetRestrictions()
    let ResetTrackLength (ps:ParseState) = ps.ResetTrackLength()

let getParseInfo pso psn = ParseInfo.Create (pso.Location) (psn.Location)

type ParseFuncResult<'a> = FallibleOption<'a * ParseState>        //  returns parsed node, if possible
type ParseFuncSignature<'a> = (ParseState -> ParseFuncResult<'a>)

type FlowFoldPrevType = Empty | TextLine


[<DebuggerStepThrough>]
let (|Regex3|_|) (pattern:RGXType) (ps:ParseState) =
    AssesInput (ps.Input.Data) pattern
    |>  TokenDataToString
    |>  Option.bind(fun inps -> 
        let m = Regex.Match(inps, RGSF(pattern), RegexOptions.Multiline)
        if m.Success then 
            let lst = [ for g in m.Groups -> g.Value ]
            let fullMatch = lst |> List.head
            let ps = ps.Advance()
            let groups = lst |> List.tail
            Some(MatchResult.Create fullMatch groups, ps |> ParseState.TrackPosition fullMatch)
        else None
    )
    |>  Option.ifnone(fun()->ps.Input.Reset();None)


[<DebuggerStepThrough>]
let (|Regex4|_|) (pattern:RGXType, condition:(RollingStream<TokenData> * TokenData-> bool)) (ps:ParseState) =
    AssesInputPostParseCondition condition (ps.Input.Data) pattern
    |>  TokenDataToString
    |>  Option.bind(fun inps -> 
        let m = Regex.Match(inps, RGSF(pattern), RegexOptions.Multiline)
        if m.Success then 
            let lst = [ for g in m.Groups -> g.Value ]
            let fullMatch = lst |> List.head
            let ps = ps.Advance()
            let groups = lst |> List.tail
            Some(MatchResult.Create fullMatch groups, ps |> ParseState.TrackPosition fullMatch)
        else None
    )
    |>  Option.ifnone(fun()->ps.Input.Reset();None)


type CreateErrorMessage() =
    static member TabIndentError ps = MessageAtLine.CreateTerminate (ps.Location) MessageCode.ErrTabCannotIndent (lazy "A tab cannot be used for indentation, use spaces instead.")
    static member IndentLevelError ps = MessageAtLine.CreateTerminate (ps.Location) MessageCode.ErrIndentationError (lazy sprintf "This line is indented incorrectly, expected %d spaces." (ps.n + ps.m))


let MemoizeCache = Dictionary<int*int*Context,RGXType>()


type Yaml12Parser(globalTagSchema : GlobalTagSchema, loggingFunction:string->unit) =
    let logger s ps = 
#if DEBUG
        sprintf "%s\t l:%d col:%d i:%d c:%A &a:%d e:%d w:%d sp:%d" s (ps.Location.Line) (ps.Location.Column) (ps.n) (ps.c) (ps.Anchors.Count) (ps.Messages.Error.Count) (ps.Messages.Warn.Count) (ps.Input.Position)
        //sprintf "%d" (ps.Location.Line)
        |> loggingFunction
#else
        ()
#endif

    member private this.GlobalTagSchema
        with get() = globalTagSchema 

        /// Memoize scalar values by position and regex.
    member private this.Caching = Dictionary<ScalarMemoizeKey, ScalarMemoizeValue>()


    new(globalTagSchema : GlobalTagSchema) = Yaml12Parser(globalTagSchema, fun _ -> ())

    member private this.LogReturn str ps (pso:FallibleOption<_>) = 
#if DEBUG
        match pso.Result with
        |   FallibleOption.Value -> 
            let  (any,prs) = pso.Data
            sprintf "/%s (Value) l:%d i:%d c:%A &a:%d e:%d w:%d sp:%d" str (prs.Location.Line) (prs.n) (prs.c) (prs.Anchors.Count) (ps.Messages.Error.Count) (prs.Messages.Warn.Count) (ps.Input.Position) |> loggingFunction
        |   FallibleOption.NoResult -> sprintf "/%s (NoResult) l:%d i:%d c:%A &a:%d e:%d w:%d sp:%d" str (ps.Location.Line) (ps.n) (ps.c) (ps.Anchors.Count) (ps.Messages.Error.Count) (ps.Messages.Warn.Count) (ps.Input.Position) |> loggingFunction 
        |   FallibleOption.ErrorResult -> sprintf "/%s (ErrorResult) l:%d i:%d c:%A &a:%d e:%d w:%d sp:%d" str (ps.Location.Line) (ps.n) (ps.c) (ps.Anchors.Count) (ps.Errors) (ps.Messages.Warn.Count) (ps.Input.Position) |> loggingFunction
        |   _   -> failwith "Illegal value for pso.Result"
        pso
#else
        pso
#endif


    //  Utility functions
    member private this.Debugbreak sr =
        sr 


    member this.Memoize fn =
        (fun k c ->
        match MemoizeCache.TryGetValue k with
        | true, v -> v
        | false, _ -> let v = fn (c)
                      MemoizeCache.Add(k,v)
                      v)


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
            let icp = GRP(OOM(RGP (this.``s-space``, [Token.``t-space``]))) + OOM(this.``ns-char``)

            let m = Regex.Match(s, RGS(icp), RegexOptions.Multiline)
            if m.Success then 
                let lst = [ for g in m.Groups -> g.Value ]
                let fullMatch = lst |> List.head
                let groups = lst |> List.tail
                let mt =MatchResult.Create fullMatch groups
                Some(mt.ge1.Length - n)
            else None

        slst
        |> List.tryPick(``match indented content``)
        |>  function
            | Some v -> v 
            | None   -> 0

    member this.``auto detect indent in line`` ps =
        let icp = GRP(ZOM(RGP (this.``s-space``, [Token.``t-space``]))) + OOM(this.``ns-char``)
        let p = ps.Input.Position
        let tkl = AssesInput (ps.Input.Data) icp
        ps.Input.Position <- p
        match tkl with
        |   (true, tl) -> 
            let th = tl |> List.takeWhile(fun e -> e.Token = Token.``t-space``)
            th.Length - ps.n
        |   (false, _) -> -1


    member this.``content with properties`` (``follow up func``: ParseFuncSignature<'a>) ps =
        let addAnchor (anchor:string) (n:Node) (ps:ParseState) = if anchor<> "" then (ps.AddAnchor anchor n) else ps
        let cnsproperties = this.``c-ns-properties`` ps
        match cnsproperties.Result with
        |   FallibleOption.Value -> 
            let (prs, (tag,tl), (anchor,_)) = cnsproperties.Data
            prs 
            |> ``follow up func``
            |> ParseState.TrackParseLocation ps
            |> FallibleOption.bind(fun (content:Node, prs) ->
                let prs = addAnchor anchor content prs
                content
                |> this.ResolveTag prs tag tl 
                |> this.PostProcessAndValidateNode
            )
        |   FallibleOption.NoResult -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
        |   _ -> failwith "Illegal value for cnsproperties"

    member this.``join lines`` (strlst:string list) = String.Join("\n", strlst)


    member this.``chomp lines`` ps strlst =
        let stripAll lst = lst |> List.rev |> List.skipWhile(fun s -> String.IsNullOrWhiteSpace(s)) |> List.rev
        match ps.t with
        | Chomping.``Strip`` -> strlst |> stripAll
        | Chomping.``Clip``  -> 
            if (String.IsNullOrWhiteSpace(List.last strlst)) then 
                List.append (strlst |> stripAll) [""] 
            else 
                strlst 
        | Chomping.``Keep``  -> strlst 


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
#if DEBUG
                logger (sprintf "%s\t\t<-%s" (res.ToString().Replace("\n","\\n")) (currStr.Replace("\n","\\n"))) ps
#endif

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

    member this.ResolveTag (ps:ParseState) tag tagLocation (node:Node) : FallibleOption<Node * ParseState> =
        let checkTagKindMatch (t:GlobalTag) rs =
            if t.Kind = node.Kind then FallibleOption<_>.Value(rs)
            else
                ps.AddErrorMessage <| MessageAtLine.CreateContinue (tagLocation) MessageCode.ErrTagKindMismatch (lazy sprintf "Tag-kind mismatch, tag: %A, node: %A" (t.Kind) (node.Kind))

        let ResolveNonSpecificTag nst = 
            TagResolutionInfo.Create nst (ps.NodePath) (node)
            |> this.GlobalTagSchema.TagResolution
            |> function
            |   Some t -> checkTagKindMatch t (node.SetTag (Global t), ps)
            |   None -> 
                let psunresvd = 
                    ps |> ParseState.AddWarningMessage(MessageAtLine.CreateContinue (ps.Location) MessageCode.Freeform (lazy sprintf "Cannot resolve tag: '%s'" nst)) |> ignore
                    ps |> ParseState.IncUnresolved
                FallibleOption<_>.Value(node, psunresvd)

        let ResolveLocalTag tag =
            FallibleOption<_>.Value(node.SetTag (Local (LocalTag.Create tag (this.GlobalTagSchema.LocalTags))), ps)

        let ResolveShorthand tsh sub =
            match tsh.MappedTagBase with
            |   Regex(RGSF(this.``ns-global-tag-prefix``)) _ -> 
                this.GlobalTagSchema.GlobalTags 
                |> List.tryFind(fun t -> t.Uri = DecodeEncodedUriHexCharacters(tsh.MappedTagBase+sub))
                |>  function
                    |   Some t  -> checkTagKindMatch t (node.SetTag (Global t),ps)
                    |   None    -> 
                        match node.Kind with
                        |   Scalar  -> FallibleOption<_>.Value(Unrecognized (this.GlobalTagSchema.UnresolvedResolution (node.Kind) (tsh.MappedTagBase+sub)) |> node.SetTag, ps |> ParseState.IncUnrecognizedScalar) 
                        |   _       -> FallibleOption<_>.Value(Global (this.GlobalTagSchema.UnresolvedResolution (node.Kind) (tsh.MappedTagBase+sub))       |> node.SetTag, ps |> ParseState.IncUnrecognizedCollection)
            |   Regex(RGSF(this.``c-ns-local-tag-prefix``)) _ -> FallibleOption<_>.Value(Local (LocalTag.Create (tsh.MappedTagBase+sub) (this.GlobalTagSchema.LocalTags))|> node.SetTag, ps)
            |   _ -> FallibleOption<_>.NoResult()


        let TryResolveTagShortHand name (sub:string) : FallibleOption<Node * ParseState> =
            ps.TagShorthands 
            |> List.tryFind(fun tsh -> tsh.ShortHand = name) 
            |>  function
                | Some tsh -> ResolveShorthand tsh sub
                | None -> FallibleOption<_>.NoResult()

        let ResolveTagShortHand name (sub:string) : FallibleOption<Node * ParseState> =
            ps.TagShorthands 
            |> List.tryFind(fun tsh -> tsh.ShortHand = name) 
            |>  function
                | Some tsh -> ResolveShorthand tsh sub
                | None ->
                    let n = node.SetTag (NonSpecific.UnresolvedTag)
                    ps.AddErrorMessage (MessageAtLine.CreateContinue (ps.Location) MessageCode.Freeform (lazy sprintf "The %s handle wasn't declared." name)) |> ignore
                    FallibleOption<_>.Value(n, ps)

        match tag with
        |   NonSpecificQM   -> ResolveNonSpecificTag "?"
        |   NonSpecificQT   -> ResolveNonSpecificTag "!"
        |   ShortHandPrimary name -> 
            TryResolveTagShortHand "!" name
            |> FallibleOption.ifnoresult(fun () -> ResolveLocalTag ("!"+name) )
        |   ShortHandSecondary sub  -> ResolveTagShortHand "!!" sub
        |   ShortHandNamed (name, sub)  -> ResolveTagShortHand name sub
        |   Verbatim name   -> ResolveLocalTag name
        |   TagKind.Empty   -> FallibleOption<_>.Value(node,ps)

#if DEBUG
    member this.KeyToString (key:ScalarMemoizeKey) =
        sprintf "<%d,%d>" (key.FunctionNumber) (key.SreamPositionStart)
#endif

    member this.CacheNode key (postParseState:ParseState) posDelta len (potetialNode:FallibleOption<Node * ParseState>) =
        let pos = postParseState.Input.Position
        let cv = 
            match potetialNode.Result with
            |   FallibleOption.Value -> 
                let (n,_) = potetialNode.Data

#if DEBUG
                logger (sprintf "> cached: %s end: %d >> %s" (this.KeyToString key) pos (n.ToPrettyString())) postParseState
#endif
                ScalarMemoizeValue.Create pos (FallibleOption<_>.Value n) posDelta len
            |   FallibleOption.NoResult   -> ScalarMemoizeValue.Create pos (FallibleOption<_>.NoResult()) posDelta len
            |   FallibleOption.ErrorResult -> ScalarMemoizeValue.Create pos (FallibleOption<_>.ErrorResult()) posDelta len
            |   _   -> failwith "Illegal value for potetialNode"
        this.Caching.Add(key, cv)
        potetialNode

    member this.UncacheNode key (preParseState:ParseState) =
        let cv = this.Caching.[key]
        preParseState.Input.Position <- cv.SreamPositionEnd
        match cv.Value.Result with
        |   FallibleOption.Value -> 
            let n = cv.Value.Data

#if DEBUG
            logger (sprintf "> uncached: %s end: %d >> %s" (this.KeyToString key) (cv.SreamPositionEnd) (n.ToPrettyString())) preParseState
#endif
            let postCacheState = preParseState.SetPositionDelta (cv.Length) (cv.PositionDelta.Line) (cv.PositionDelta.Column)
            FallibleOption<_>.Value(n,postCacheState)
        |   FallibleOption.NoResult -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
        |   _ -> failwith "Illegal value for cv"

    member this.PostProcessAndValidateNode (fn : FallibleOption<Node * ParseState>) : FallibleOption<Node * ParseState> = 
        match fn.Result with
        |   FallibleOption.Value -> 
            let (n,ps)  = fn.Data
            n.NodeTag.PostProcessAndValidateNode (ps.Messages.Error) n 
            |> FallibleOption.map(fun nd -> (nd,ps))
        |   FallibleOption.NoResult -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
        |   _ -> failwith "Illegal value for fn"

    member this.ResolvedNullNode (ps:ParseState) =  
        (this.ResolveTag ps NonSpecificQM (ps.Location) (PlainEmptyNode (getParseInfo ps ps))).Data |> fst // should never be None

          
    //  [1] http://www.yaml.org/spec/1.2/spec.html#c-printable
    member this.``c-printable`` = 
        RGO (
            "\u0009\u000a\u000d\u0020-\u007e" +   // 8 - bit, #x9 | #xA | #xD | [#x20-#x7E]
            "\u0085\u00a0-\ud7ff\ue000-\ufffd",   // 16- bit, #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD]
                                                   //  32-bit -> currently not supported because .Net does not encode naturally. Yaml: [#x10000-#x10FFFF]
            [
            Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``c-printable``; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark`` 
            Token.``t-colon`` ; Token.``t-comma``; Token.``t-dot`` ; Token.``t-square-bracket-start`` ; Token.``t-square-bracket-end`` ; Token.``t-curly-bracket-start``
            Token.``t-curly-bracket-end`` ; Token.``t-hash`` ; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``
            Token.``t-gt``; Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``;Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``
            Token.``ns-dec-digit``; Token.``c-escape``
            ])

    //  [2] http://www.yaml.org/spec/1.2/spec.html#nb-json
    member ths.``nb-json`` = 
        RGO ("\u0009\u0020-\uffff",
            [
            Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``c-printable``; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark`` 
            Token.``t-colon`` ; Token.``t-comma``; Token.``t-dot`` ; Token.``t-square-bracket-start`` ; Token.``t-square-bracket-end`` ; Token.``t-curly-bracket-start``
            Token.``t-curly-bracket-end`` ; Token.``t-hash`` ; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``
            Token.``t-gt``; Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``;Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``
            Token.``ns-dec-digit``; Token.``c-escape``; Token.``nb-json``
            ])

    //  [3] http://www.yaml.org/spec/1.2/spec.html#c-byte-order-mark
    member this.``c-byte-order-mark`` = RGP ("\ufeff", [Token.``byte-order-mark``])

    //  [4] http://www.yaml.org/spec/1.2/spec.html#c-sequence-entry
    member this.``c-sequence-entry`` = RGP ("-", [Token.``t-hyphen``])

    //  [5] http://www.yaml.org/spec/1.2/spec.html#c-mapping-key
    member this.``c-mapping-key`` = RGP ("\\?", [Token.``t-questionmark``])

    //  [6] http://www.yaml.org/spec/1.2/spec.html#c-mapping-value
    member this.``c-mapping-value`` = RGP (":", [Token.``t-colon``])

    //  [7] http://www.yaml.org/spec/1.2/spec.html#c-collect-entry
    member this.``c-collect-entry`` = RGP(",", [Token.``t-comma``])

    //  [8] http://www.yaml.org/spec/1.2/spec.html#c-sequence-start
    member this.``c-sequence-start`` = RGP("\[", [Token.``t-square-bracket-start``])

    //  [9] http://www.yaml.org/spec/1.2/spec.html#c-sequence-end
    member this.``c-sequence-end`` = RGP("\]", [Token.``t-square-bracket-end``])

    //  [10]    http://www.yaml.org/spec/1.2/spec.html#c-mapping-start
    member this.``c-mapping-start`` = RGP ("\{",[Token.``t-curly-bracket-start``])

    //  [11]    http://www.yaml.org/spec/1.2/spec.html#c-mapping-end
    member this.``c-mapping-end`` = RGP("\}", [Token.``t-curly-bracket-end``])

    //  [12]    http://www.yaml.org/spec/1.2/spec.html#c-comment
    member this.``c-comment`` = RGP ("#", [Token.``t-hash``])

    //  [13]    http://www.yaml.org/spec/1.2/spec.html#c-anchor
    member this.``c-anchor`` = "&"

    //  [14]    http://www.yaml.org/spec/1.2/spec.html#c-alias
    member this.``c-alias`` = "*"

    //  [15]    http://www.yaml.org/spec/1.2/spec.html#c-tag
    member this.``c-tag`` = "!"

    //  [16]    http://www.yaml.org/spec/1.2/spec.html#c-literal
    member this.``c-literal`` = "|"

    //  [17]    http://www.yaml.org/spec/1.2/spec.html#c-folded
    member this.``c-folded`` = RGP(">", [Token.``t-gt``])

    //  [18]    http://www.yaml.org/spec/1.2/spec.html#c-single-quote
    member this.``c-single-quote`` = RGP ("\'", [Token.``t-single-quote``])

    //  [19]    http://www.yaml.org/spec/1.2/spec.html#c-double-quote
    member this.``c-double-quote`` = RGP ("\"", [Token.``t-double-quote``])

    //  [20]    http://www.yaml.org/spec/1.2/spec.html#c-directive
    member this.``c-directive`` = "%"

    //  [21]    http://www.yaml.org/spec/1.2/spec.html#c-reserved
    member this.``c-reserved`` = RGO ("\u0040\u0060", [Token.``t-commat``;Token.``t-tick``])

    //  [22]    http://www.yaml.org/spec/1.2/spec.html#c-indicator
    member this.``c-indicator`` = 
        RGO  (
            "\-\?:,\[\]\{\}#&\*!;>\'\"%@`", 
            [ 
            Token.``t-hyphen``; Token.``t-questionmark``; Token.``t-colon``
            Token.``t-comma``; Token.``t-square-bracket-start``; Token.``t-square-bracket-end``
            Token.``t-curly-bracket-start``; Token.``t-curly-bracket-end``; Token.``t-hash``
            Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``
            Token.``t-gt``; Token.``t-single-quote``; Token.``t-double-quote``
            Token.``t-percent``; Token.``t-commat``;Token.``t-tick``
            ])


    //  [23]    http://www.yaml.org/spec/1.2/spec.html#c-flow-indicator
    member this.``c-flow-indicator`` = 
        RGO  (@",\[\]\{\}", 
            [
                Token.``t-comma``
                Token.``t-square-bracket-start``; Token.``t-square-bracket-end``
                Token.``t-curly-bracket-start``; Token.``t-curly-bracket-end``
            ])

    //  [24]    http://www.yaml.org/spec/1.2/spec.html#b-line-feed
    member this.``b-line-feed`` = RGP ("\u000a", [Token.NewLine])

    //  [25]    http://www.yaml.org/spec/1.2/spec.html#b-carriage-return
    member this.``b-carriage-return`` = RGP ("\u000d", [Token.NewLine])

    //  [i26]   http://www.yaml.org/spec/1.2/spec.html#b-char
    member this.``b-char`` = this.``b-line-feed`` ||| this.``b-carriage-return``

    //  [27]    http://www.yaml.org/spec/1.2/spec.html#nb-char
    member this.``nb-char``  = this.``c-printable`` - RGO("\u000a\u000d", [Token.NewLine]) // this.``b-char``

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
    member this.``s-space`` : string = "\u0020"  // space

    //  [32]    http://www.yaml.org/spec/1.2/spec.html#s-tab
    member this.``s-tab`` = "\u0009"    // tab

    //  [33]    http://www.yaml.org/spec/1.2/spec.html#s-white
    member this.``s-white`` = RGO(this.``s-space`` + this.``s-tab``, [Token.``t-space``; Token.``t-tab``])

    //  [34]    http://www.yaml.org/spec/1.2/spec.html#ns-char
    member this.``ns-char`` = this.``nb-char`` - this.``s-white``

    //  [35]    http://www.yaml.org/spec/1.2/spec.html#ns-dec-digit
    member this.``ns-dec-digit`` = RGO ("\u0030-\u0039", [Token.``ns-dec-digit``])      //  0-9

    //  [36]    http://www.yaml.org/spec/1.2/spec.html#ns-hex-digit
    member this.``ns-hex-digit`` =
        this.``ns-dec-digit`` +
        RGO ("\u0041-\u0046", [Token.``c-printable``])  +  //  A-F
        RGO ("\u0061-\u0066", [Token.``c-printable``])     //  a-f

    //  [37]    http://www.yaml.org/spec/1.2/spec.html#ns-ascii-letter
    member this.``ns-ascii-letter`` = 
        RGO ("\u0041-\u005A", [Token.``c-printable``]) +   //  A-Z
        RGO ("\u0061-\u007A", [Token.``c-printable``])     //  a-z

    //  [38]    http://www.yaml.org/spec/1.2/spec.html#ns-word-char
    member this.``ns-word-char`` =
        this.``ns-dec-digit`` + (RGO (@"\-", [Token.``t-hyphen``])) + this.``ns-ascii-letter``

    //  [39]    http://www.yaml.org/spec/1.2/spec.html#ns-uri-char
    member this.``ns-uri-char`` = 
        RGP (@"%", [Token.``t-percent``]) + this.``ns-hex-digit`` + this.``ns-hex-digit``  |||
        RGO (
            @"#;/?:@&=+$,_.!~*\'\(\)\[\]", 
            [
            Token.``t-hash``; Token.``t-forward-slash``; Token.``t-questionmark``;Token.``t-colon``;Token.``t-ampersand``; 
            Token.``t-commat``; Token.``t-equals``;Token.``t-plus``;Token.``t-comma``; Token.``t-dot``
            Token.``t-quotationmark``;Token.``t-single-quote``;Token.``t-square-bracket-start``;Token.``t-square-bracket-end``
            Token.``c-printable``
        ]) + this.``ns-word-char``

    //  [40]    http://www.yaml.org/spec/1.2/spec.html#ns-tag-char
    member this.``ns-tag-char`` = 
        RGP (@"%", [Token.``t-percent``]) + this.``ns-hex-digit`` + this.``ns-hex-digit``  |||
        (RGO (
            @"#;/?:@&=+$_.~*\'\(\)", 
            [
            Token.``t-hash``; Token.``t-forward-slash``;Token.``t-questionmark``;Token.``t-colon``;Token.``t-ampersand``; 
            Token.``t-commat``; Token.``t-equals``;Token.``t-plus``;Token.``t-comma``; Token.``t-dot``
            Token.``t-single-quote``;Token.``t-square-bracket-start``;Token.``t-square-bracket-end``
            Token.``c-printable``;
        ]) - this.``c-flow-indicator``) + this.``ns-word-char``

    //  [41]    http://www.yaml.org/spec/1.2/spec.html#c-escape
    member this.``c-escape`` = RGP ("\\\\", [Token.``c-escape``])

    //  [42]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-null
    member this.``ns-esc-null`` = RGP ("0", [Token.``ns-dec-digit``])

    //  [43]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-bell
    member this.``ns-esc-bell`` = RGP ("a", [Token.``c-printable``])

    //  [44]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-backspace
    member this.``ns-esc-backspace`` = RGP( "b", [Token.``c-printable``])

    //  [45]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-horizontal-tab
    member this.``ns-esc-horizontal-tab`` = RGP ("t", [Token.``c-printable``])

    //  [46]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-line-feed
    member this.``ns-esc-line-feed`` = RGP ("n", [Token.``c-printable``])

    //  [47]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-vertical-tab
    member this.``ns-esc-vertical-tab`` = RGP ("v", [Token.``c-printable``])

    //  [48]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-form-feed
    member this.``ns-esc-form-feed`` = RGP ("f", [Token.``c-printable``])

    //  [49]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-carriage-return
    member this.``ns-esc-carriage-return`` = RGP ("r", [Token.``c-printable``])

    //  [50]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-escape
    member this.``ns-esc-escape`` = RGP ("e", [Token.``c-printable``])

    //  [51]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-space
    member this.``ns-esc-space`` = RGP ("\u0020", [Token.``t-space``])

    //  [52]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-double-quote
    member this.``ns-esc-double-quote`` = RGP ("\"", [Token.``t-double-quote``])

    //  [53]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-slash
    member this.``ns-esc-slash`` = RGP ("/", [Token.``c-printable``])

    //  [54]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-backslash
    member this.``ns-esc-backslash`` = RGP ("\\\\", [Token.``c-escape``])

    //  [55]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-next-line
    member this.``ns-esc-next-line`` = RGP ("N", [Token.``c-printable``])

    //  [56]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-non-breaking-space
    member this.``ns-esc-non-breaking-space`` = RGP ("_", [Token.``c-printable``])

    //  [57]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-line-separator
    member this.``ns-esc-line-separator`` = RGP ("L", [Token.``c-printable``])

    //  [58]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-paragraph-separator
    member this.``ns-esc-paragraph-separator`` = RGP ("P", [Token.``c-printable``])

    //  [59]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-8-bit
    member this.``ns-esc-8-bit`` = (RGP ("x", [Token.``c-printable``])) + Repeat(this.``ns-hex-digit``,2)

    //  [60]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-16-bit
    member this.``ns-esc-16-bit`` = RGP ("u", [Token.``c-printable``]) + Repeat(this.``ns-hex-digit``,4)

    //  [61]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-32-bit
    member this.``ns-esc-32-bit`` = RGP ("U", [Token.``c-printable``]) + Repeat(this.``ns-hex-digit``,8) // currently not supported

    //  [62]    http://www.yaml.org/spec/1.2/spec.html#c-ns-esc-char
    member this.``c-ns-esc-char`` = 
        RGP ("\\\\", [Token.``c-escape``]) +
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
    member this.``s-indent(n)`` ps = Repeat(RGP (this.``s-space``, [Token.``t-space``]), ps.n)

    //  [64]    http://www.yaml.org/spec/1.2/spec.html#s-indent(<n)
    member this.``s-indent(<n)`` ps = Range(RGP (this.``s-space``, [Token.``t-space``]), 0, (ps.n-1)) (* Where m < n *)

    //  [65]    http://www.yaml.org/spec/1.2/spec.html#s-indent(≤n)
    member this.``s-indent(<=n)`` ps = Range(RGP (this.``s-space``, [Token.``t-space``]), 0, ps.n)  (* Where m ≤ n *)

    //  [66]    http://www.yaml.org/spec/1.2/spec.html#s-separate-in-line
    member this.``s-separate-in-line`` = OOM(this.``s-white``) ||| ``start-of-line``

    //  [67]    http://www.yaml.org/spec/1.2/spec.html#s-line-prefix(n,c)
    member this.``s-line-prefix`` ps =
        logger "s-line-prefix" ps
        match ps.c with
        | Context.``Block-out`` ->  this.``s-block-line-prefix`` ps
        | Context.``Block-in``  ->  this.``s-block-line-prefix`` ps
        | Context.``Flow-out``  ->  this.``s-flow-line-prefix`` ps
        | Context.``Flow-in``   ->  this.``s-flow-line-prefix`` ps
        | _             ->  failwith "The context 'block-key' and 'flow-key' are not supported at this point"

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
        OPT(this.``s-separate-in-line``) + (this.``b-l-folded`` (ps.SetStyleContext Context.``Flow-in``)) + (this.``s-flow-line-prefix`` ps)

    //  [75]    http://www.yaml.org/spec/1.2/spec.html#c-nb-comment-text
    member this.``c-nb-comment-text`` = RGP("#", [Token.``t-hash``]) + ZOM(this.``nb-char``)

    //  [76]    http://www.yaml.org/spec/1.2/spec.html#b-comment
    member this.``b-comment`` = this.``b-non-content`` ||| RGP("\\z", [Token.EOF]) // EOF..

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
        | Context.``Block-out`` ->  this.``s-separate-lines`` ps
        | Context.``Block-in``  ->  this.``s-separate-lines`` ps
        | Context.``Flow-out``  ->  this.``s-separate-lines`` ps
        | Context.``Flow-in``   ->  this.``s-separate-lines`` ps
        | Context.``Block-key`` ->  this.``s-separate-in-line``
        | Context.``Flow-key``  ->  this.``s-separate-in-line``
        |   _   -> failwith "Illegal value for ps.c"

    //  [81]    http://www.yaml.org/spec/1.2/spec.html#s-separate-lines(n)
    member this.``s-separate-lines`` ps = (this.``s-l-comments`` + (this.``s-flow-line-prefix`` ps)) ||| this.``s-separate-in-line``

    //  [82]    http://www.yaml.org/spec/1.2/spec.html#l-directive
    member this.``l-directive`` (ps:ParseState) : FallibleOption<ParseState> = 
        let ParseDirective prs : FallibleOption<Directive*ParseState> =
            let ``ns-yaml-directive`` = this.``ns-yaml-directive`` + this.``s-l-comments``
            let ``ns-tag-directive`` = this.``ns-tag-directive``  + this.``s-l-comments``
            let ``ns-reserved-directive`` = GRP(this.``ns-reserved-directive``) + this.``s-l-comments``
            match prs.Input.Data with
            |   Regex2(``ns-yaml-directive``)  mt -> 
                //let ps = 
                match (mt.ge1.Split('.') |> List.ofArray) with
                | [a;b] when a="1" && b<"2" -> ps |> ParseState.AddWarningMessage (MessageAtLine.CreateContinue (ps.Location) MessageCode.Freeform (lazy sprintf "YAML %s document will be parsed as YAML 1.2" mt.ge1))
                | [a;b] when a="1" && b="2" -> FallibleOption<_>.NoResult()
                | [a;b] when a="1" && b>"2" -> ps |> ParseState.AddWarningMessage (MessageAtLine.CreateContinue (ps.Location) MessageCode.Freeform (lazy sprintf "YAML %s document will be parsed as YAML 1.2" mt.ge1))
                | [a;_] when a>"1"          -> ps |> ParseState.AddErrorMessage (MessageAtLine.CreateContinue (ps.Location) MessageCode.Freeform (lazy sprintf "YAML %s document cannot be parsed, only YAML 1.2 is supported" mt.ge1))
                | _                         -> ps |> ParseState.AddErrorMessage (MessageAtLine.CreateContinue (ps.Location) MessageCode.Freeform (lazy sprintf "Illegal directive: %%YAML %s, document cannot be parsed" mt.ge1))
                |> ignore
                let ymlcnt = ps.Directives |> List.filter(function | YAML _ -> true | _ -> false) |> List.length
                if ymlcnt>0 then 
                    (ps |> ParseState.AddErrorMessage (MessageAtLine.CreateContinue (ps.Location) MessageCode.Freeform (lazy "The YAML directive must only be given at most once per document.")))

                elif ps.Errors >0 then FallibleOption<_>.ErrorResult()
                else
                    FallibleOption<_>.Value(YAML(mt.ge1), ps.Advance())
            |   Regex2(``ns-tag-directive``)   mt    -> 
                let tg = mt.ge2 |> fst
                let ymlcnt = ps.Directives |> List.filter(function | TAG (t,_) ->  (t=tg) | _ -> false) |> List.length
                if ymlcnt>0 then 
                    (ps |> ParseState.AddErrorMessage (MessageAtLine.CreateContinue (ps.Location) (MessageCode.Freeform) (lazy sprintf "The TAG directive must only be given at most once per handle in the same document: %s" tg)))
                else
                    let tagPfx = snd mt.ge2
                    let lcTag = this.``c-primary-tag-handle`` + OOM(this.``ns-tag-char``)
                    if System.Uri.IsWellFormedUriString(tagPfx, UriKind.Absolute) || IsMatchStr(tagPfx, lcTag) then
                        FallibleOption<_>.Value(TAG(mt.ge2),  ps.Advance() |> ParseState.AddTagShortHand (TagShorthand.Create (mt.ge2)))
                    else
                        ps |> ParseState.AddErrorMessage (MessageAtLine.CreateContinue (ps.Location) MessageCode.Freeform (lazy sprintf "Tag is not a valid Uri-, or local-tag prefix: %s" tg))
            |   Regex2(``ns-reserved-directive``) mt -> 
                ps |> ParseState.Advance |> ParseState.AddWarningMessage (MessageAtLine.CreateContinue (ps.Location) (MessageCode.Freeform) (lazy sprintf "Reserved directive will ignored: %%%s" mt.ge1)) |> ignore
                FallibleOption<_>.Value(RESERVED(mt.Groups), ps)
            |   _   -> FallibleOption<_>.NoResult()


        ps 
        |> ParseState.``Match and Advance`` (RGP ("%", [Token.``t-percent``])) (ParseDirective)
        |> FallibleOption.bind(fun (t,prs) ->
            prs 
            |> ParseState.``Match and Advance`` (this.``s-l-comments``) 
                (fun prs2 -> prs2.AddDirective t |> FallibleOption<ParseState>.Value)
        )


    //  [83]    http://www.yaml.org/spec/1.2/spec.html#ns-reserved-directive
    member this.``ns-reserved-directive`` = 
        this.``ns-directive-name`` + ZOMNG(this.``s-separate-in-line`` + this.``ns-directive-parameter``)

    //  [84]    http://www.yaml.org/spec/1.2/spec.html#ns-directive-name
    member this.``ns-directive-name`` = OOM(this.``ns-char``)

    //  [85]    http://www.yaml.org/spec/1.2/spec.html#ns-directive-parameter
    member this.``ns-directive-parameter`` = OOM(this.``ns-char``)

    //  [86]    http://www.yaml.org/spec/1.2/spec.html#ns-yaml-directive
    member this.``ns-yaml-directive`` = RGP("YAML", [Token.``c-printable``]) + this.``s-separate-in-line`` + GRP(this.``ns-yaml-version``)

    //  [87]    http://www.yaml.org/spec/1.2/spec.html#ns-yaml-version
    member this.``ns-yaml-version`` = OOM(this.``ns-dec-digit``) + RGP("\\.", [Token.``c-printable``]) + OOM(this.``ns-dec-digit``)

    //  [88]    http://www.yaml.org/spec/1.2/spec.html#ns-tag-directive
    member this.``ns-tag-directive`` = 
        RGP ("TAG", [Token.``c-printable``]) + this.``s-separate-in-line`` + GRP(this.``c-tag-handle``) + this.``s-separate-in-line`` + GRP(this.``ns-tag-prefix``)

    //  [89]    http://www.yaml.org/spec/1.2/spec.html#c-tag-handle
    member this.``c-tag-handle`` = this.``c-named-tag-handle`` ||| this.``c-secondary-tag-handle`` ||| this.``c-primary-tag-handle``

    //  [90]    http://www.yaml.org/spec/1.2/spec.html#c-primary-tag-handle
    member this.``c-primary-tag-handle`` = RGP ("!", [Token.``t-quotationmark``])

    //  [91]    http://www.yaml.org/spec/1.2/spec.html#c-secondary-tag-handle
    member this.``c-secondary-tag-handle`` = RGP ("!!", [Token.``t-quotationmark``])

    //  [92]    http://www.yaml.org/spec/1.2/spec.html#c-named-tag-handle
    member this.``c-named-tag-handle`` = RGP ("!", [Token.``t-quotationmark``]) + OOM(this.``ns-word-char``) + RGP ("!", [Token.``t-quotationmark``]) 

    //  [93]    http://www.yaml.org/spec/1.2/spec.html#ns-tag-prefix
    member this.``ns-tag-prefix`` = this.``c-ns-local-tag-prefix`` ||| this.``ns-global-tag-prefix``

    //  [94]    http://www.yaml.org/spec/1.2/spec.html#c-ns-local-tag-prefix
    member this.``c-ns-local-tag-prefix`` = RGP ("!", [Token.``t-quotationmark``]) + ZOM(this.``ns-uri-char``)

    //  [95]    http://www.yaml.org/spec/1.2/spec.html#ns-global-tag-prefix
    member this.``ns-global-tag-prefix`` = this.``ns-tag-char`` + ZOM(this.``ns-uri-char``)

    //  [96]    http://www.yaml.org/spec/1.2/spec.html#c-ns-properties(n,c)
    member this.``c-ns-properties`` ps : FallibleOption<ParseState * (TagKind*DocumentLocation) * (string*DocumentLocation)> =
        logger "c-ns-properties" ps
        
        let anchor pst  =
            pst |> ParseState.``Match and Advance`` (RGP ("&", [Token.``t-ampersand``])) (fun psr ->
                let illAnchor = OOM(this.``ns-char``)
                match psr with
                |   Regex3(this.``ns-anchor-name``) (mt,prs) -> FallibleOption<_>.Value(prs, mt.FullMatch)
                |   Regex3(illAnchor) (mt,_) -> psr.AddErrorMessage <| MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrAnchorSyntax (lazy sprintf "Anchor has incorrect format: &%s" mt.FullMatch)
                |   _ -> FallibleOption<_>.NoResult()
            )
            |> FallibleOption.map(fun (p,a) -> p,(a,pst.Location))

        let tag pst =
            let lsvt = RGP ("!", [Token.``t-quotationmark``]) + RGP ("<", [Token.``c-printable``])
            let rsvt = RGP (">", [Token.``t-gt``])
            let verbatim = lsvt + GRP(OOM(this.``ns-uri-char``)) + rsvt
            let illVerbatim = lsvt + OOM(this.``ns-uri-char``)
            let illVerbatimNoLocaltag = lsvt + RGP ("!", [Token.``t-quotationmark``]) + rsvt
            let shorthandNamed = GRP(this.``c-named-tag-handle``) + GRP(OOM(this.``ns-tag-char``))
            let illShorthandNamed = GRP(this.``c-named-tag-handle``)
            let shorthandSecondary = this.``c-secondary-tag-handle`` + GRP(OOM(this.``ns-tag-char``))
            let illShorthandSecondary = this.``c-secondary-tag-handle`` 
            let shorthandPrimary = this.``c-primary-tag-handle``+ GRP(OOM(this.``ns-tag-char``))
            match pst with
            |   Regex3(illVerbatimNoLocaltag) _ -> pst.AddErrorMessage <| MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrVerbatimTagNoLocal (lazy "Verbatim tags aren't resolved, so ! is invalid.")
            |   Regex3(verbatim) (mt, prs) -> 
                let tag = mt.ge1
                let lcTag = this.``c-primary-tag-handle`` + OOM(this.``ns-tag-char``)
                if System.Uri.IsWellFormedUriString(tag, UriKind.Absolute) || IsMatchStr(tag, lcTag) then
                    FallibleOption<_>.Value(prs, Verbatim mt.ge1)
                else
                    pst.AddErrorMessage <| MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrVerbatimTagIncorrectFormat (lazy "Verbatim tag is neither a local or global tag.")
            |   Regex3(illVerbatim) _ -> pst.AddErrorMessage <| MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrVerbatimTag (lazy "Verbatim tag starting with '!<' is missing a closing '>'")
            |   Regex3(shorthandNamed) (mt, prs) -> FallibleOption<_>.Value(prs, ShortHandNamed mt.ge2)
            |   Regex3(illShorthandNamed) (mt,_) -> pst.AddErrorMessage <| MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrShorthandNamed (lazy sprintf "The %s handle has no suffix." mt.FullMatch)
            |   Regex3(shorthandSecondary) (mt, prs) -> FallibleOption<_>.Value(prs, ShortHandSecondary mt.ge1)
            |   Regex3(illShorthandSecondary) _ -> pst.AddErrorMessage <| MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrShorthandSecondary (lazy "The !! handle has no suffix.")
            |   Regex3(shorthandPrimary) (mt, prs) -> FallibleOption<_>.Value(prs, ShortHandPrimary mt.ge1)
            |   Regex3(this.``c-non-specific-tag``) (_, prs) -> FallibleOption<_>.Value(prs, NonSpecificQT)
            |   _ -> FallibleOption<_>.NoResult()
            |> FallibleOption.map(fun (p,t) -> p,(t,pst.Location))

        let matchTagAnchor pst =
            let afterTag ps tg =
                ps
                |> ParseState.``Match and Advance`` (OPT(this.``s-separate`` ps)) (anchor)
                |> FallibleOption.map(fun (psa,a) -> (psa, tg, a))

            let tg = (pst|> tag)
            match tg.Result with
            |   FallibleOption.NoResult    -> afterTag pst (TagKind.Empty, pst.Location)
            |   FallibleOption.Value       -> tg.Data ||>  afterTag 
            |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
            |   _   -> failwith "Illegal value for tg"

        let matchAnchorTag pst =
            let afterAnchor ps anch =
                ps
                |> ParseState.``Match and Advance`` (OPT(this.``s-separate`` ps)) (tag)
                |> FallibleOption.map(fun (psa,t) -> (psa, t, anch))

            let anch = (pst|> anchor)
            match anch.Result with
            |   FallibleOption.NoResult     -> afterAnchor pst ("",pst.Location)
            |   FallibleOption.Value        -> anch.Data ||> afterAnchor 
            |   FallibleOption.ErrorResult  -> FallibleOption<_>.ErrorResult()
            |   _   -> failwith "Illegal value for tg"

        (ps |> ParseState.OneOf) {
            either (matchTagAnchor)
            either (matchAnchorTag)
            ifneither(FallibleOption<_>.NoResult())
        }
        |> fun ct -> 
                ct.Result.Result |>
                function 
                |   FallibleOption.NoResult     -> ParseState.PreserveNoResult ct
                |   FallibleOption.Value        -> FallibleOption<_>.Value (ct.Result.Data)
                |   FallibleOption.ErrorResult  -> FallibleOption<_>.ErrorResult()
                |   _   -> failwith "Illegal value"
            

    //  [97]    http://www.yaml.org/spec/1.2/spec.html#c-ns-tag-property
    member this.``c-ns-tag-property`` = this.``c-verbatim-tag`` ||| this.``c-ns-shorthand-tag`` ||| this.``c-non-specific-tag``

    //  [98]    http://www.yaml.org/spec/1.2/spec.html#c-verbatim-tag
    member this.``c-verbatim-tag`` = RGP ("!", [Token.``t-quotationmark``]) + RGP ("<", [Token.``c-printable``]) + OOM(this.``ns-uri-char``) + RGP (">", [Token.``t-gt``]) 

    //  [99]    http://www.yaml.org/spec/1.2/spec.html#c-ns-shorthand-tag
    member this.``c-ns-shorthand-tag`` = this.``c-tag-handle`` + OOM(this.``ns-tag-char``)

    //  [100]   http://www.yaml.org/spec/1.2/spec.html#c-non-specific-tag
    member this.``c-non-specific-tag`` = RGP ("!", [Token.``t-quotationmark``])

    //  [101]   http://www.yaml.org/spec/1.2/spec.html#c-ns-anchor-property
    member this.``c-ns-anchor-property`` = RGP ("&", [Token.``t-ampersand``]) + this.``ns-anchor-name``

    //  [102]   http://www.yaml.org/spec/1.2/spec.html#ns-anchor-char
    member this.``ns-anchor-char`` =  this.``ns-char`` - this.``c-flow-indicator``

    //  [103]   http://www.yaml.org/spec/1.2/spec.html#ns-anchor-name
    member this.``ns-anchor-name`` = OOM(this.``ns-anchor-char``)

    //  [104]   http://www.yaml.org/spec/1.2/spec.html#c-ns-alias-node
    member this.``c-ns-alias-node`` ps : ParseFuncResult<_> =
        logger "c-ns-alias-node" ps
        ps |> ParseState.``Match and Advance`` (RGP ("\\*", [Token.``t-asterisk``])) (fun prs ->
            prs |> ParseState.``Match and Parse`` (this.``ns-anchor-name``) (fun mt prs2 ->
                let retrievedAnchor = ps.GetAnchor mt
                match retrievedAnchor with
                |   Some ra -> FallibleOption<_>.Value(ra, prs2)
                |   None    -> prs2.AddErrorMessage <|MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrAnchorNotExists (lazy sprintf "Referenced anchor '%s' is unknown." mt)
               
        ))
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-ns-alias-node" ps

    //  [105]   http://www.yaml.org/spec/1.2/spec.html#e-scalar
    member this.``e-scalar`` = RGP (String.Empty, [])     // we'll see if this works..

    //  [106]   http://www.yaml.org/spec/1.2/spec.html#e-node
    member this.``e-node`` = this.``e-scalar``

    //  [107]   http://www.yaml.org/spec/1.2/spec.html#nb-double-char
    member this.``nb-double-char`` = this.``c-ns-esc-char`` ||| (this.``nb-json`` - RGO("\\\\\"", [Token.``c-escape``; Token.``t-double-quote``]))

    //  [108]   http://www.yaml.org/spec/1.2/spec.html#ns-double-char
    member this.``ns-double-char`` = this.``c-ns-esc-char`` |||  (this.``nb-json`` - RGO("\\\\\"", [Token.``c-escape``; Token.``t-double-quote``]) - this.``s-white``)

    //  [109]   http://www.yaml.org/spec/1.2/spec.html#c-double-quoted(n,c)
    member this.``c-double-quoted`` ps : ParseFuncResult<_> = 
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
            |> CreateScalarNode (NonSpecific.NonSpecificTagQT) (getParseInfo ps prs)
            |> this.ResolveTag prs NonSpecificQT (prs.Location)
            |> this.PostProcessAndValidateNode

        let processMultiLine prs content =
            content 
            |> this.``split by linefeed``
            |> this.``double quote flowfold lines`` ps
            |> CreateScalarNode (NonSpecific.NonSpecificTagQT) (getParseInfo ps prs)
            |> this.ResolveTag prs NonSpecificQT (prs.Location)
            |> this.PostProcessAndValidateNode

        let patt = this.``c-double-quote`` + GRP(this.``nb-double-text`` ps) + this.``c-double-quote``
        let ``illegal-chars`` = this.``c-double-quote`` + OOM((this.``nb-json`` - this.``c-double-quote``) ||| this.``s-double-break`` ps) + this.``c-double-quote``
        let ``illegal-patt`` = this.``c-double-quote`` + GRP(this.``nb-double-text`` ps)

        match ps with
        |   Regex3(patt)    (mt,prs) ->
            let content = mt.ge1
            match ps.c with
            |  Context.``Flow-out`` |  Context.``Flow-in`` ->   //  multiline
                let lines = content |> this.``split by linefeed`` |> List.length
                if lines = 1 then
                    processSingleLine prs content
                else
                    processMultiLine prs content
            |   Context.``Block-key`` | Context.``Flow-key`` -> //  single line
                processSingleLine prs content
            | _  ->  failwith "The context 'block-out' and 'block-in' are not supported at this point"
        |   Regex3(``illegal-chars``) _ -> ps.AddErrorMessage <| MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrDquoteIllegalChars (lazy "Literal string contains illegal characters.")
        |   Regex3(``illegal-patt``) _  -> ps.AddErrorMessage <| MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrMissingDquote (lazy "Missing \" in string literal.")
        |   _ -> FallibleOption<_>.NoResult()
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-double-quoted" ps        

    //  [110]   http://www.yaml.org/spec/1.2/spec.html#nb-double-text(n,c)
    member this.``nb-double-text`` ps =
        match ps.c with
        | Context.``Flow-out``  ->  this.``nb-double-multi-line`` ps
        | Context.``Flow-in``   ->  this.``nb-double-multi-line`` ps
        | Context.``Block-key`` ->  this.``nb-double-one-line``
        | Context.``Flow-key``  ->  this.``nb-double-one-line``
        | _             ->  failwith "The context 'block-out' and 'block-in' are not supported at this point"

    //  [111]   http://www.yaml.org/spec/1.2/spec.html#nb-double-one-line
    member this.``nb-double-one-line`` = ZOM(this.``nb-double-char``)

    //  [112]   http://www.yaml.org/spec/1.2/spec.html#s-double-escaped(n)
    member this.``s-double-escaped`` (ps:ParseState) = ZOM(this.``s-white``) + this.``c-escape`` + this.``b-non-content`` + ZOM(this.``l-empty`` (ps.SetStyleContext Context.``Flow-in``)) + (this.``s-flow-line-prefix`` ps)

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
    member this.``c-quoted-quote`` = this.``c-single-quote`` + this.``c-single-quote``

    //  [118]   http://www.yaml.org/spec/1.2/spec.html#nb-single-char
    member this.``nb-single-char`` = this.``c-quoted-quote`` ||| (this.``nb-json`` - this.``c-single-quote``)

    //  [119]   http://www.yaml.org/spec/1.2/spec.html#ns-single-char
    member this.``ns-single-char`` = // this.``nb-single-char`` - this.``s-white``
        this.``c-quoted-quote`` ||| (this.``nb-json`` - this.``c-single-quote`` - this.``s-white``)

    //  [120]   http://www.yaml.org/spec/1.2/spec.html#c-single-quoted(n,c)
    member this.``c-single-quoted`` ps : ParseFuncResult<_> = 
        logger "c-single-quoted" ps
        let convertSQuoteEscapedSingleLine (str:string) = str.Replace("''", "'")

        let processSingleLine prs content =
            content 
            |> convertSQuoteEscapedSingleLine
            |> CreateScalarNode (NonSpecific.NonSpecificTagQT) (getParseInfo ps prs)
            |> this.ResolveTag prs NonSpecificQT (prs.Location)
            |> this.PostProcessAndValidateNode

        let processMultiLine prs content =
            content 
            |> this.``split by linefeed``
            |> this.``single quote flowfold lines`` ps
            |> CreateScalarNode (NonSpecific.NonSpecificTagQT) (getParseInfo ps prs)
            |> this.ResolveTag prs NonSpecificQT (prs.Location)
            |> this.PostProcessAndValidateNode

        let patt = this.``c-single-quote`` + GRP(this.``nb-single-text`` ps) + this.``c-single-quote``
        let ``illegal-patt`` = this.``c-single-quote`` + GRP(this.``nb-single-text`` ps) 

        match ps with
        |   Regex3(patt)    (mt,prs) ->
            let content = mt.ge1
            match ps.c with
            |  Context.``Flow-out`` |  Context.``Flow-in`` ->   //  multiline
                let lines = content |> this.``split by linefeed`` |> List.length
                if lines = 1 then
                    processSingleLine prs content
                else
                    processMultiLine prs content
            |   Context.``Block-key`` | Context.``Flow-key`` -> //  single line
                processSingleLine prs content
            | _             ->  failwith "The context 'block-out' and 'block-in' are not supported at this point"
        |   Regex3(``illegal-patt``) _ ->
            MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrMissingSquote (lazy "Missing \' in string literal.")
            |>  ps.AddErrorMessage
        |   _ -> FallibleOption<_>.NoResult()
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-single-quoted" ps        

    //  [121]   http://www.yaml.org/spec/1.2/spec.html#nb-single-text(n,c)
    member this.``nb-single-text`` ps =
        logger "nb-single-text" ps
        match ps.c with
        |   Context.``Flow-out``    -> this.``nb-single-multi-line`` ps
        |   Context.``Flow-in``     -> this.``nb-single-multi-line`` ps
        |   Context.``Block-key``   -> this.``nb-single-one-line``
        |   Context.``Flow-key``    -> this.``nb-single-one-line``
        | _             ->  failwith "The context 'block-out' and 'block-in' are not supported at this point"

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
    member this.``ns-plain-first`` ps = (this.``ns-char`` - this.``c-indicator``) ||| (this.``c-mapping-key`` ||| this.``c-mapping-value`` ||| this.``c-sequence-entry``) + (this.``ns-plain-safe`` ps)

    //  [127]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-safe(c)
    member this.``ns-plain-safe`` ps =
        logger "ns-plain-safe" ps

        let memFunc ps =
            match ps.c with
            |   Context.``Flow-out``    -> this.``ns-plain-safe-out``
            |   Context.``Flow-in``     -> this.``ns-plain-safe-in``
            |   Context.``Block-key``   -> this.``ns-plain-safe-out``
            |   Context.``Flow-key``    -> this.``ns-plain-safe-in``
            | _             ->  failwith "The context 'block-out' and 'block-in' are not supported at this point"
        let callMemoized = this.Memoize memFunc
        callMemoized (127, ps.n, ps.c) ps

    //  [128]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-safe-out
    member this.``ns-plain-safe-out`` = this.``ns-char``

    //  [129]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-safe-in
    member this.``ns-plain-safe-in`` = this.``ns-char`` - this.``c-flow-indicator``

    //  [130]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-char(c)
    member this.``ns-plain-char`` ps = (this.``ns-char`` + this.``c-comment``) ||| ((this.``ns-plain-safe`` ps) - (RGO (":#", [Token.``t-colon``; Token.``t-hash``]))) ||| (this.``c-mapping-value`` + (this.``ns-plain-safe`` ps))

    //  [131]   http://www.yaml.org/spec/1.2/spec.html#ns-plain(n,c)
    member this.``ns-plain`` (ps:ParseState) =
        logger "ns-plain" ps

        let memFunc ps =
            match ps.c with
            | Context.``Flow-out``  -> this.``ns-plain-multi-line`` ps
            | Context.``Flow-in``   -> this.``ns-plain-multi-line`` ps
            | Context.``Block-key`` -> this.``ns-plain-one-line`` ps
            | Context.``Flow-key``  -> this.``ns-plain-one-line`` ps
            | _              -> failwith "The context 'block-out' and 'block-in' are not supported at this point"
        let callMemoized = this.Memoize memFunc
        callMemoized (131, ps.n, ps.c) ps

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
        |   Context.``Flow-out`` -> Context.``Flow-in``
        |   Context.``Flow-in``  -> Context.``Flow-in``
        |   Context.``Block-key``-> Context.``Flow-key``
        |   Context.``Flow-key`` -> Context.``Flow-key``
        | _              -> failwith "The context 'block-out' and 'block-in' are not supported at this point"

    //  [137]   http://www.yaml.org/spec/1.2/spec.html#c-flow-sequence(n,c)
    member this.``c-flow-sequence`` (ps:ParseState) : ParseFuncResult<_> =
        logger "c-flow-sequence" ps
        ps |> ParseState.``Match and Advance`` (this.``c-sequence-start`` + OPT(this.``s-separate`` ps)) (fun prs ->
            let prs = prs.SetStyleContext(this.``in-flow`` prs)

            let noResult prs =
                prs |> ParseState.``Match and Advance`` (this.``c-sequence-end``) (fun psx -> 
                    CreateSeqNode (NonSpecific.NonSpecificTagQM) (getParseInfo ps psx) [] 
                    |> this.ResolveTag psx NonSpecificQM (prs.Location)
                    |> this.PostProcessAndValidateNode
                    )

            let nssflowseqentries = (this.``ns-s-flow-seq-entries`` prs)
            match  nssflowseqentries.Result with
            |   FallibleOption.Value  ->
                let (c, prs2) = nssflowseqentries.Data
                prs2 
                |> ParseState.``Match and Advance`` (this.``c-sequence-end``) (fun psx -> FallibleOption<_>.Value (c, psx))
                |> FallibleOption.ifnoresult(fun () -> 
                    MessageAtLine.CreateContinue (prs2.Location) MessageCode.ErrMissingMappingSymbol (lazy "Incorrect sequence syntax, are you missing a comma, or ]?")
                    |>  prs2.AddErrorMessage
                )
            |   FallibleOption.NoResult -> prs |> noResult
            |   FallibleOption.ErrorResult -> prs |> noResult |> FallibleOption.ifnoresult(fun () -> FallibleOption<_>.ErrorResult())
            | _ -> failwith "Illegal value for nssflowseqentries"

        )
        |> ParseState.TrackParseLocation ps
        |> ParseState.ResetEnv ps
        |> this.LogReturn "c-flow-sequence" ps        
                
    //  [138]   http://www.yaml.org/spec/1.2/spec.html#ns-s-flow-seq-entries(n,c)
    member this.``ns-s-flow-seq-entries`` (ps:ParseState) : ParseFuncResult<_> =
        logger "ns-s-flow-seq-entries" ps
        let rec ``ns-s-flow-seq-entries`` (psp:ParseState) (lst:Node list) : ParseFuncResult<_> =
            let noResult rs psr =
                if lst.Length = 0 then rs   // empty sequence
                else 
                    CreateSeqNode (NonSpecific.NonSpecificTagQM) (getParseInfo ps psr) lst 
                    |> this.ResolveTag psr NonSpecificQM (psr.Location)
                    |> this.PostProcessAndValidateNode

            let nsflowseqentry =(this.``ns-flow-seq-entry`` psp)
            match  nsflowseqentry.Result with
            |   FallibleOption.Value ->
                let (entry, prs) = nsflowseqentry.Data
                let lst = entry :: lst
                let prs = prs.SkipIfMatch (OPT(this.``s-separate`` prs))
                let commaPattern = this.``c-collect-entry`` + OPT(this.``s-separate`` prs)
                match prs with 
                |   Regex3(commaPattern) (_, prs2) -> ``ns-s-flow-seq-entries`` prs2 lst |> ParseState.MarkParseRange prs
                |   _ ->  
                    CreateSeqNode (NonSpecific.NonSpecificTagQM) (getParseInfo ps prs) lst
                    |> this.ResolveTag prs NonSpecificQM (prs.Location)
                    |> this.PostProcessAndValidateNode
            |   FallibleOption.NoResult     -> psp |> noResult (FallibleOption<_>.NoResult())
            |   FallibleOption.ErrorResult  -> psp |> noResult (FallibleOption<_>.ErrorResult())
            | _ -> failwith "Illegal value for nsflowseqentry"

        ``ns-s-flow-seq-entries`` ps []
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "ns-s-flow-seq-entries" ps        

    //  [139]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-seq-entry(n,c)
    member this.``ns-flow-seq-entry`` (ps:ParseState) : ParseFuncResult<_> =
        logger "ns-flow-seq-entry" ps

        (ps |> ParseState.OneOf) {
            either(this.``ns-flow-pair`` >> FallibleOption.bind(fun ((ck, cv), prs) -> CreateMapNode (NonSpecific.NonSpecificTagQM) (getParseInfo ps prs) [(ck,cv)] |> this.ResolveTag prs NonSpecificQM (prs.Location)))
            either(this.``ns-flow-node``)
            ifneither(FallibleOption<_>.NoResult())
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> ParseState.ResetEnv ps
        |> this.LogReturn "ns-flow-seq-entry"  ps

    //  [140]   http://www.yaml.org/spec/1.2/spec.html#c-flow-mapping(n,c)
    member this.``c-flow-mapping`` (ps:ParseState) : ParseFuncResult<_> =
        logger "c-flow-mapping" ps
        ps |> ParseState.``Match and Advance`` (this.``c-mapping-start`` + OPT(this.``s-separate`` ps)) (fun prs ->
            let prs = prs.SetStyleContext(this.``in-flow`` prs)
            let mres = this.``ns-s-flow-map-entries`` prs

            let noResult prs = prs |> ParseState.``Match and Advance`` (this.``c-mapping-end``) (fun prs2 -> CreateMapNode (NonSpecific.NonSpecificTagQM) (getParseInfo ps prs2) [] |> this.ResolveTag prs2 NonSpecificQM (prs2.Location))

            match mres.Result with
            |   FallibleOption.Value  -> 
                let (c, prs2) = mres.Data
                prs2 
                |> ParseState.``Match and Advance`` (this.``c-mapping-end``) (fun prs2 -> FallibleOption<_>.Value(c, prs2))
                |> FallibleOption.ifnoresult(fun () ->
                    MessageAtLine.CreateContinue (prs2.Location) MessageCode.ErrMissingMappingSymbol (lazy "Incorrect mapping syntax, are you missing a comma, or }?")
                    |>  prs2.AddErrorMessage
                )
            |   FallibleOption.NoResult     -> prs |> noResult 
            |   FallibleOption.ErrorResult  -> prs |> noResult |> FallibleOption.ifnoresult(fun () -> FallibleOption<_>.ErrorResult())
            | _ -> failwith "Illegal value for mres"
               
        )
        |> ParseState.ResetEnv ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-flow-mapping" ps       
            
    //  [141]   http://www.yaml.org/spec/1.2/spec.html#ns-s-flow-map-entries(n,c)
    member this.``ns-s-flow-map-entries`` (ps:ParseState) : ParseFuncResult<_> =
        logger "ns-s-flow-map-entries" ps
        let rec ``ns-s-flow-map-entries`` (psp:ParseState) (lst:(Node*Node) list) : ParseFuncResult<_> =
            let noResult rs psr =
                if lst.Length = 0 then rs   // empty sequence
                else
                    CreateMapNode (NonSpecific.NonSpecificTagQM) (getParseInfo ps psr) lst 
                    |> this.ResolveTag psr NonSpecificQM (psr.Location)
                    |> this.PostProcessAndValidateNode

            let nsflowmapentry = (this.``ns-flow-map-entry`` psp) 
            match nsflowmapentry.Result with
            |   FallibleOption.Value  -> 
                let ((ck, cv), prs) = nsflowmapentry.Data
                let lst = (ck, cv) :: lst
                let prs = prs.SkipIfMatch (OPT(this.``s-separate`` prs))
                let commaPattern = this.``c-collect-entry`` + OPT(this.``s-separate`` prs)
                match prs with
                |   Regex3(commaPattern) (_, prs2) -> ``ns-s-flow-map-entries`` prs2 lst |> ParseState.MarkParseRange prs
                |   _ -> 
                    CreateMapNode (NonSpecific.NonSpecificTagQM) (getParseInfo ps prs) lst  
                    |> this.ResolveTag prs NonSpecificQM (prs.Location)
                    |> this.PostProcessAndValidateNode
            |   FallibleOption.NoResult -> psp |> noResult (FallibleOption<_>.NoResult())
            |   FallibleOption.ErrorResult -> psp |> noResult (FallibleOption<_>.ErrorResult())
            | _ -> failwith "Illegal value for nsflowmapentry"

        ``ns-s-flow-map-entries`` ps []
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "ns-s-flow-map-entries" ps        

    //  [142]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-map-entry(n,c)
    member this.``ns-flow-map-entry`` (ps:ParseState) : ParseFuncResult<_> =
        logger "ns-flow-map-entry" ps
        let ``ns-flow-map-explicit-entry`` ps = 
            ps |> ParseState.``Match and Advance`` (this.``c-mapping-key`` + (this.``s-separate`` ps)) (this.``ns-flow-map-explicit-entry``)
        (ps |> ParseState.OneOf) {
            either (``ns-flow-map-explicit-entry``)
            either (this.``ns-flow-map-implicit-entry``)
            ifneither (FallibleOption<_>.NoResult())        
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "ns-flow-map-entry" ps        

    //  [143]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-map-explicit-entry(n,c)
    member this.``ns-flow-map-explicit-entry`` (ps:ParseState) : ParseFuncResult<_> =
        logger "ns-flow-map-explicit-entry" ps
        let nsflowmapimplicitentry = (this.``ns-flow-map-implicit-entry`` ps)
        match  nsflowmapimplicitentry.Result with
        |   FallibleOption.Value -> FallibleOption<_>.Value (nsflowmapimplicitentry.Data)
        |   FallibleOption.NoResult -> FallibleOption<_>.Value((this.ResolvedNullNode ps, this.ResolvedNullNode ps), ps)       // ( ``e-node`` + ``e-node``)
        |   FallibleOption.ErrorResult -> 
            FallibleOption<_>.Value((this.ResolvedNullNode ps, this.ResolvedNullNode ps), ps)       // ( ``e-node`` + ``e-node``)
        | _ -> failwith "Illegal value for nsflowmapimplicitentry"
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "ns-flow-map-explicit-entry" ps       

    //  [144]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-map-implicit-entry(n,c)
    member this.``ns-flow-map-implicit-entry`` (ps:ParseState) : ParseFuncResult<_> =
        logger "ns-flow-map-implicit-entry" ps
        (ps |> ParseState.OneOf) {
            either (this.``ns-flow-map-yaml-key-entry``)
            either (this.``c-ns-flow-map-empty-key-entry``)
            either (this.``c-ns-flow-map-json-key-entry``)
            ifneither (FallibleOption<_>.NoResult())        
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "ns-flow-map-implicit-entry" ps        

    //  [145]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-map-yaml-key-entry(n,c)
    member this.``ns-flow-map-yaml-key-entry`` (ps:ParseState) =
        logger "ns-flow-map-yaml-key-entry" ps
        let nsflowyamlnode = (this.``ns-flow-yaml-node`` (ps |> ParseState.RestrictMultiLine)) 
        match nsflowyamlnode.Result with
        |   FallibleOption.Value -> 
            let  (ck, prs) = nsflowyamlnode.Data
            let prs = prs.SkipIfMatch (OPT(this.``s-separate`` prs)) |> ParseState.ResetRestrictions
            let cnsflowmapseparatevalue = (this.``c-ns-flow-map-separate-value`` prs) 
            match cnsflowmapseparatevalue.Result with
            |   FallibleOption.Value -> 
                let (cv, prs2)  = cnsflowmapseparatevalue.Data
                FallibleOption<_>.Value((ck,cv), prs2)
            |   FallibleOption.NoResult -> FallibleOption<_>.Value((ck, this.ResolvedNullNode prs), prs)  //  ``e-node``
            |   FallibleOption.ErrorResult -> 
                FallibleOption<_>.Value((ck, this.ResolvedNullNode prs), prs)  //  ``e-node``
            | _ -> failwith "Illegal value for cnsflowmapseparatevalue"
        |   FallibleOption.NoResult   -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
        | _ -> failwith "Illegal value for nsflowyamlnode"
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "ns-flow-map-yaml-key-entry" ps     

    //  [146]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-map-empty-key-entry(n,c)
    member this.``c-ns-flow-map-empty-key-entry`` (ps:ParseState) : ParseFuncResult<_> =
        logger "c-ns-flow-map-empty-key-entry" ps
        let cnsflowmapseparatevalue = (this.``c-ns-flow-map-separate-value`` ps) 
        match cnsflowmapseparatevalue.Result with
        |   FallibleOption.Value -> 
            let (c, prs) = cnsflowmapseparatevalue.Data
            FallibleOption<_>.Value((this.ResolvedNullNode prs, c), prs)   //  ``e-node``
        |   FallibleOption.NoResult   -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
        | _ -> failwith "Illegal value for cnsflowmapseparatevalue"
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-ns-flow-map-empty-key-entry" ps     

    //  [147]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-map-separate-value(n,c)
    member this.``c-ns-flow-map-separate-value`` (ps:ParseState) : ParseFuncResult<_> =
        logger "c-ns-flow-map-separate-value" ps
        ps |> ParseState.``Match and Advance`` this.``c-mapping-value`` (fun prs ->
            if IsMatch(prs.Input.Data, (this.``ns-plain-safe`` prs)) then FallibleOption<_>.NoResult()
            else
                let nsflownode = prs |> ParseState.``Match and Advance`` (this.``s-separate`` prs) (this.``ns-flow-node``)
                match nsflownode.Result with
                |   FallibleOption.ErrorResult ->
                    PlainEmptyNode (getParseInfo ps prs) |> this.ResolveTag prs NonSpecificQM (prs.Location)  //  ``e-node``
                |   FallibleOption.NoResult -> PlainEmptyNode (getParseInfo ps prs) |> this.ResolveTag prs NonSpecificQM (prs.Location) //  ``e-node``
                |   _  -> nsflownode
        )
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-ns-flow-map-separate-value" ps     

    //  [148]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-map-json-key-entry(n,c)
    member this.``c-ns-flow-map-json-key-entry`` (ps:ParseState) =
        logger "c-ns-flow-map-json-key-entry" ps
        let cflowjsonnode = (this.``c-flow-json-node`` ps) 
        match cflowjsonnode.Result with
        |   FallibleOption.Value -> 
            let (ck, prs) = cflowjsonnode.Data
            let prs = prs.SkipIfMatch (OPT(this.``s-separate`` prs))
            let cnsflowmapadjacentvalue = (this.``c-ns-flow-map-adjacent-value`` prs) 
            match cnsflowmapadjacentvalue.Result with
            |   FallibleOption.Value  -> 
                let (cv, prs2) = cnsflowmapadjacentvalue.Data
                FallibleOption<_>.Value((ck,cv), prs2)
            |   FallibleOption.NoResult -> FallibleOption<_>.Value((ck, this.ResolvedNullNode prs), prs)  //  ``e-node``
            |   FallibleOption.ErrorResult -> 
                FallibleOption<_>.Value((ck, this.ResolvedNullNode prs), prs)  //  ``e-node``
            | _ -> failwith "Illegal value for cnsflowmapadjacentvalue"
        |   FallibleOption.NoResult -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
        | _ -> failwith "Illegal value for cflowjsonnode"
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-ns-flow-map-json-key-entry" ps

    //  [149]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-map-adjacent-value(n,c)
    member this.``c-ns-flow-map-adjacent-value`` (ps:ParseState) : FallibleOption<_> =
        logger "c-ns-flow-map-adjacent-value" ps
        ps |> ParseState.``Match and Advance`` (this.``c-mapping-value``) (fun prs ->
            let prs = prs.SkipIfMatch (OPT(this.``s-separate`` ps))
            let nsflownode = (this.``ns-flow-node`` prs) 
            match nsflownode.Result  with
            |   FallibleOption.Value -> FallibleOption<_>.Value <| nsflownode.Data
            |   FallibleOption.NoResult -> FallibleOption<_>.Value(this.ResolvedNullNode prs, prs)  //  ``e-node``
            |   FallibleOption.ErrorResult -> 
                FallibleOption<_>.Value(this.ResolvedNullNode prs, prs)  //  ``e-node``
            | _ -> failwith "Illegal value for nsflownode"
        )
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-ns-flow-map-adjacent-value" ps

    //  [150]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-pair(n,c)
    member this.``ns-flow-pair`` (ps:ParseState) : ParseFuncResult<_> =
        logger "ns-flow-pair" ps
        let ``ns-flow-map-explicit-entry`` (ps:ParseState) = 
            ps |> ParseState.``Match and Advance`` (this.``c-mapping-key`` + (this.``s-separate`` ps)) (this.``ns-flow-map-explicit-entry``)
        (ps |> ParseState.OneOf) {
            either (``ns-flow-map-explicit-entry``)
            either (this.``ns-flow-pair-entry``)
            ifneither (FallibleOption<_>.NoResult())        
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "ns-flow-pair" ps

    //  [151]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-pair-entry(n,c)
    member this.``ns-flow-pair-entry`` (ps:ParseState) : ParseFuncResult<_> =
        logger "ns-flow-pair-entry" ps
        (ps |> ParseState.OneOf) {
            either (this.``ns-flow-pair-yaml-key-entry``)
            either (this.``c-ns-flow-map-empty-key-entry``)
            either (this.``c-ns-flow-pair-json-key-entry``)
            ifneither (FallibleOption<_>.NoResult())        
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "ns-flow-pair-entry" ps

    //  [152]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-pair-yaml-key-entry(n,c)
    member this.``ns-flow-pair-yaml-key-entry`` (ps:ParseState) : ParseFuncResult<_> =
        logger "ns-flow-pair-yaml-key-entry" ps
        let ``ns-s-implicit-yaml-key`` (ps:ParseState) = (this.``ns-s-implicit-yaml-key`` (ps.SetStyleContext Context.``Flow-key``))
        let nssimplicityamlkey = (``ns-s-implicit-yaml-key`` ps) 
        match nssimplicityamlkey.Result with
        |   FallibleOption.Value ->
            let (ck, prs) = nssimplicityamlkey.Data
            let cnsflowmapseparatevalue = (this.``c-ns-flow-map-separate-value`` prs) 
            match cnsflowmapseparatevalue.Result with
            |   FallibleOption.Value -> 
                let (cv, prs2) = cnsflowmapseparatevalue.Data
                FallibleOption<_>.Value((ck, cv), prs2)
            |   FallibleOption.NoResult   -> FallibleOption<_>.NoResult()
            |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
            | _ -> failwith "Illegal value for cnsflowmapseparatevalue"
        |   FallibleOption.NoResult   -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
        | _ -> failwith "Illegal value for nssimplicityamlkey"
        |> ParseState.TrackParseLocation ps
        |> ParseState.ResetEnv ps
        |> this.LogReturn "ns-flow-pair-yaml-key-entry" ps

    //  [153]   http://www.yaml.org/spec/1.2/spec.html#c-ns-flow-pair-json-key-entry(n,c)
    member this.``c-ns-flow-pair-json-key-entry`` (ps:ParseState) : ParseFuncResult<_> =
        logger "c-ns-flow-pair-json-key-entry" ps
        let cssimplicitjsonkey = (this.``c-s-implicit-json-key`` (ps.SetStyleContext Context.``Flow-key``)) 
        match cssimplicitjsonkey.Result with
        |   FallibleOption.Value ->
            let (ck, prs) = cssimplicitjsonkey.Data
            let cnsflowmapadjacentvalue = (this.``c-ns-flow-map-adjacent-value`` prs) 
            match cnsflowmapadjacentvalue.Result with
            |   FallibleOption.Value -> 
                let (cv, prs2) = cnsflowmapadjacentvalue.Data
                FallibleOption<_>.Value((ck, cv), prs2)
            |   FallibleOption.NoResult -> FallibleOption<_>.NoResult()
            |   FallibleOption.ErrorResult  -> FallibleOption<_>.ErrorResult()
            | _ -> failwith "Illegal value for cnsflowmapadjacentvalue"
        |   FallibleOption.NoResult -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
        | _ -> failwith "Illegal value for cssimplicitjsonkey"
        |> ParseState.TrackParseLocation ps
        |> ParseState.ResetEnv ps
        |> this.LogReturn "c-ns-flow-pair-json-key-entry" ps

    //  [154]   http://www.yaml.org/spec/1.2/spec.html#ns-s-implicit-yaml-key(c)
    member this.``ns-s-implicit-yaml-key`` (ps:ParseState) : ParseFuncResult<_> =
        logger "ns-s-implicit-yaml-key" ps
//        let ``n/a`` = 0
//        let ps = ps.SetIndent ``n/a``
        let nsflowyamlnode = (this.``ns-flow-yaml-node`` (ps |> ParseState.ResetTrackLength |> ParseState.RestrictMultiLine)) 
        match nsflowyamlnode.Result with
        |   FallibleOption.Value -> 
            let (ck, prs) = nsflowyamlnode.Data
            let prs = prs.SkipIfMatch (OPT(this.``s-separate-in-line``))
            if prs.TrackLength > 1024 then
                MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrLengthExceeds1024 (lazy "The mapping key is too long. The maximum allowed length is 1024.)")
                |>  prs.AddErrorMessage
            else FallibleOption<_>.Value(ck, prs)
        |   FallibleOption.NoResult   -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult ()
        | _ -> failwith "Illegal value for nsflowyamlnode"
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "ns-s-implicit-yaml-key" ps

    //  [155]   http://www.yaml.org/spec/1.2/spec.html#c-s-implicit-json-key(c)
    member this.``c-s-implicit-json-key`` (ps:ParseState) : ParseFuncResult<_> = (* At most 1024 characters altogether *)
        logger "c-s-implicit-json-key" ps
//        let ``n/a`` = 0
//        let ps = ps.SetIndent ``n/a``
        let cflowjsonnode = (this.``c-flow-json-node`` (ps |> ParseState.ResetTrackLength |> ParseState.RestrictMultiLine)) 
        match cflowjsonnode.Result with
        |   FallibleOption.Value -> 
            let (c, prs) = cflowjsonnode.Data
            if prs.TrackLength > 1024 then
                MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrLengthExceeds1024 (lazy "The mapping key is too long. The maximum allowed length is 1024.)")
                |>  prs.AddErrorMessage
            else 
                let prs = prs.SkipIfMatch (OPT(this.``s-separate-in-line``))
                FallibleOption<_>.Value(c, prs)
        |   FallibleOption.NoResult   -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
        | _ -> failwith "Illegal value for cflowjsonnode"
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn  "c-s-implicit-json-key" ps

    //  [156]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-yaml-content(n,c)
    member this.``ns-flow-yaml-content`` (ps:ParseState) : ParseFuncResult<_> = 
        logger "ns-flow-yaml-content" ps

        //  illegal indicators, minus squote and dquote; see this.``c-indicator``
        let ``illegal-ns-plain`` ps = 
            let memFunc ps =
                this.``c-indicator`` + this.``ns-plain-first`` ps
            let callMemoized = this.Memoize memFunc
            callMemoized (1560, ps.n, ps.c) ps
        let ``illegl multiline`` ps = 
            let memFunc ps =
                (this.``ns-plain-one-line`` ps) + OOM(this.``s-ns-plain-next-line`` ps)
            let callMemoized = this.Memoize memFunc
            callMemoized (1561, ps.n, ps.c) ps
        let preErr = 
            if not(ps.Restrictions.AllowedMultiLine) then 
                match ps.c with
                | Context.``Flow-out``  | Context.``Flow-in``   -> FallibleOption<_>.NoResult()
                | Context.``Block-key`` | Context.``Flow-key``  -> 
                    match ps with
                    |   Regex3(``illegl multiline`` ps) _ -> 
                        MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrPlainScalarMultiLine (lazy "This plain scalar cannot span multiple lines; this restrictin applies to mapping keys.")
                        |>  ps.AddErrorMessage
                    |   _ -> FallibleOption<_>.NoResult()
                | _  -> failwith "The context 'block-out' and 'block-in' are not supported at this point"
            else FallibleOption<_>.NoResult()

        //  this is a 'hack', although the specs require mandatory whitespace in any non-empty line of a multiline plain scalar
        //  the regex does provide this mandatory rule.
        //  See: http://www.yaml.org/spec/1.2/spec.html#id2788756
        //  Tweaking rule 69 - making "s-separate-in-line" mandatory - did not solve this issue, so we inject this check in the parser.
        let postParseCondition : RollingStream<TokenData> * TokenData -> bool =
            let startPos = ps.Input.Position
            fun (rs, tokenData) ->
                if rs.Position = startPos|| tokenData.Token <> Token.NewLine then true
                else
                    let next = rs.Peek()
                    [Token.NewLine; Token.``t-tab``; Token.``t-space``]
                    |>  List.exists(fun e -> e = next.Token)

        let ck = ScalarMemoizeKey.Create (ps.Input.Position) 156

        if this.Caching.ContainsKey(ck) then
            logger "> ns-plain uncache" ps
            this.UncacheNode ck ps
        else
            match preErr.Result with
            |   FallibleOption.NoResult ->
                match (ps.c, ps) with
                |   Context.``Flow-out``, Regex4(this.``ns-plain`` ps, postParseCondition) (mt, prs) 
                |   Context.``Flow-in``,  Regex4(this.``ns-plain`` ps, postParseCondition) (mt, prs)  -> 
                    logger (sprintf "> ns-plain value: %s" mt.FullMatch) prs
                    let dl = ParseState.PositionDelta mt.FullMatch ||> DocumentLocation.Create
                    mt.FullMatch
                    |> this.``split by linefeed``
                    |> this.``plain flow fold lines`` prs
                    |> CreateScalarNode (NonSpecific.NonSpecificTagQM) (getParseInfo ps prs)
                    |> this.ResolveTag prs NonSpecificQM (prs.Location)
                    |> this.PostProcessAndValidateNode
                    |> this.CacheNode ck prs dl (mt.FullMatch.Length)
                |   Context.``Block-key``, Regex3(this.``ns-plain`` ps) (mt, prs) 
                |   Context.``Flow-key``,  Regex3(this.``ns-plain`` ps) (mt, prs)  -> 
                    logger (sprintf "> ns-plain value: %s" mt.FullMatch) prs
                    let dl = ParseState.PositionDelta mt.FullMatch ||> DocumentLocation.Create
                    mt.FullMatch
                    |> CreateScalarNode (NonSpecific.NonSpecificTagQM) (getParseInfo ps prs)
                    |> this.ResolveTag prs NonSpecificQM (prs.Location)
                    |> this.PostProcessAndValidateNode
                    |> this.CacheNode ck prs dl (mt.FullMatch.Length)
                |   _, Regex3(``illegal-ns-plain`` ps) (_,prs) -> 
                    MessageAtLine.CreateContinue (ps.Location) MessageCode.ErrPlainScalarRestrictedIndicator (lazy "Reserved indicators can't start a plain scalar.")
                    |> prs.AddErrorMessage
                    |> this.CacheNode ck prs (DocumentLocation.Empty) 0
                |   Context.``Block-out``, _
                |   Context.``Block-in``, _ -> failwith "The context 'block-out' and 'block-in' are not supported at this point"
                |   _ -> FallibleOption<_>.NoResult()
            | x -> preErr
        |> ParseState.ResetEnv ps 
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn  "ns-flow-yaml-content" ps

    //  [157]   http://www.yaml.org/spec/1.2/spec.html#c-flow-json-content(n,c)
    member this.``c-flow-json-content`` (ps:ParseState) : ParseFuncResult<_> =
        logger "c-flow-json-content" ps
        ps.OneOf {
            either (this.``c-single-quoted``)
            either (this.``c-double-quoted``)
            either (this.``c-flow-mapping``)
            either (this.``c-flow-sequence``)
            ifneither (FallibleOption<_>.NoResult())
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn  "c-flow-json-content" ps
        
    //  [158]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-content(n,c)
    member this.``ns-flow-content`` (ps:ParseState) : ParseFuncResult<_> =
        logger "ns-flow-content" ps
        ps.OneOf {
            either (this.``ns-flow-yaml-content``)
            either (this.``c-flow-json-content``)
            ifneither (FallibleOption<_>.NoResult())
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn  "ns-flow-content" ps

    //  [159]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-yaml-node(n,c)
    member this.``ns-flow-yaml-node`` (ps:ParseState) : ParseFuncResult<_> =
        logger "ns-flow-yaml-node" ps
        let ``ns-flow-yaml-content`` psp =
            psp 
            |> ParseState.``Match and Advance`` (this.``s-separate`` psp) (this.``ns-flow-yaml-content``)
            |> FallibleOption.ifnoresult (fun () -> FallibleOption<_>.Value (PlainEmptyNode (getParseInfo ps psp), psp))    //  ``e-scalar`` None

        ps.OneOf {
            either (this.``c-ns-alias-node``)
            either (this.``ns-flow-yaml-content``)
            either (this.``content with properties`` ``ns-flow-yaml-content``)
            ifneither (FallibleOption<_>.NoResult())
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn  "ns-flow-yaml-node" ps

    //  [160]   http://www.yaml.org/spec/1.2/spec.html#c-flow-json-node(n,c)
    member this.``c-flow-json-node`` (ps:ParseState) : ParseFuncResult<_> =
        logger "c-flow-json-node" ps
        let ``c-flow-json-content`` ps =
            ps |> ParseState.``Match and Advance`` (this.``s-separate`` ps) (this.``c-flow-json-content``)
        ps.OneOf {
            either (this.``c-flow-json-content``)
            either (this.``content with properties`` ``c-flow-json-content``)
            ifneither (FallibleOption<_>.NoResult())
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-flow-json-node" ps
    
    //  [161]   http://www.yaml.org/spec/1.2/spec.html#ns-flow-node(n,c)
    member this.``ns-flow-node`` (ps:ParseState) : ParseFuncResult<_> =
        logger "ns-flow-node" ps
        let ``ns-flow-content`` ps =
            ps |> ParseState.``Match and Advance`` (this.``s-separate`` ps) (this.``ns-flow-content``)

        let ``empty content`` psp = FallibleOption<_>.Value (PlainEmptyNode (getParseInfo ps psp), psp) //  ``e-scalar`` None
    
        ps.OneOf {
            either (this.``c-ns-alias-node``)
            either (this.``ns-flow-content``)
            either (this.``content with properties`` ``ns-flow-content``)
            either (this.``content with properties`` ``empty content``)
            ifneither (FallibleOption<_>.NoResult())
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "ns-flow-node" ps

    //  [162]   http://www.yaml.org/spec/1.2/spec.html#c-b-block-header(m,t)
    member this.``c-b-block-header`` (ps:ParseState) = 
        logger "c-b-block-header" ps
        let chomp indicator =
            match indicator with
            |   "-" -> Chomping.``Strip``
            |   "+" -> Chomping.``Keep``
            |   ""  -> Chomping.``Clip``
            |   _ -> failwith "Undetected illegal chomp indicator"
        let indent i = 
            if i = "" then None    
            else Some(Int32.Parse i)

        let ``indent chomp`` ps : FallibleOption<int option * ParseState> = 
            let p = GRP(this.``c-indentation-indicator``) + GRP(this.``c-chomping-indicator``) + this.``s-b-comment``
            match ps.Input.Data with
            | Regex2(p)  mt -> 
                let (i, c) = mt.ge2
                FallibleOption<_>.Value(indent  i, ps.SetChomping (chomp c) |> ParseState.Advance |> ParseState.TrackPosition mt.FullMatch)
            |   _ -> FallibleOption<_>.NoResult()

        let ``chomp indent`` ps : FallibleOption<int option * ParseState> = 
            let p = GRP(this.``c-chomping-indicator``) + GRP(this.``c-indentation-indicator``) + this.``s-b-comment``
            match ps.Input.Data with
            | Regex2(p)  mt -> 
                let (c, i) = mt.ge2
                FallibleOption<_>.Value(indent  i, ps.SetChomping (chomp c) |> ParseState.Advance |> ParseState.TrackPosition mt.FullMatch)
            |   _ -> FallibleOption<_>.NoResult()

        let ``illformed chomping`` ps : FallibleOption<int option * ParseState> =
            let p = GRP(OOMNG(this.``nb-char``)) + this.``s-b-comment``
            match ps.Input.Data with
            | Regex2(p)  mt -> ps.AddErrorMessage <| MessageAtLine.CreateTerminate (ps.Location) MessageCode.ErrFoldedChompIndicator (lazy sprintf "Illegal chomp indicator '%s'" (mt.ge1))
            |   _ -> FallibleOption<_>.NoResult()

        let nochomp = FallibleOption<_>.Value(None, ps.SetChomping Chomping.``Clip``)
        (ps |> ParseState.OneOf) {
            either(``indent chomp``)
            either(``chomp indent``)
            either(``illformed chomping``)
            ifneither(nochomp)
        }
        |> ParseState.PreserveErrors ps
        |> this.LogReturn "c-b-block-header" ps

    //  [163]   http://www.yaml.org/spec/1.2/spec.html#c-indentation-indicator(m)
    member this.``c-indentation-indicator`` = OPT(this.``ns-dec-digit``)

    //  [164]   http://www.yaml.org/spec/1.2/spec.html#c-chomping-indicator(t)
    member this.``c-chomping-indicator`` = OPT(RGP("\\+", [Token.``t-plus``]) ||| this.``c-sequence-entry``)

    //  [165]   http://www.yaml.org/spec/1.2/spec.html#b-chomped-last(t)
    member this.``b-chomped-last`` ps =
        logger "b-chomped-last" ps
        match ps.t with
        |   Chomping.``Strip``   -> this.``b-non-content``    ||| RGP("\\z", [Token.EOF])
        |   Chomping.``Clip``    -> this.``b-as-line-feed``   ||| RGP("\\z", [Token.EOF])
        |   Chomping.``Keep``    -> this.``b-as-line-feed``   ||| RGP("\\z", [Token.EOF])

    //  [166]   http://www.yaml.org/spec/1.2/spec.html#l-chomped-empty(n,t)
    member this.``l-chomped-empty`` (ps:ParseState) =
        logger "l-chomped-empty" ps
        match ps.t with
        |   Chomping.``Strip``   -> this.``l-strip-empty`` ps
        |   Chomping.``Clip``    -> this.``l-strip-empty`` ps
        |   Chomping.``Keep``    -> this.``l-keep-empty`` ps

    //  [167]   http://www.yaml.org/spec/1.2/spec.html#l-strip-empty(n)
    member this.``l-strip-empty`` ps = ZOM((this.``s-indent(<=n)`` ps) + this.``b-non-content``) + OPT(this.``l-trail-comments`` ps)

    //  [168]   http://www.yaml.org/spec/1.2/spec.html#l-keep-empty(n)
    member this.``l-keep-empty`` ps = ZOM(this.``l-empty`` (ps.SetStyleContext Context.``Block-in``)) + OPT(this.``l-trail-comments`` ps)

    //  [169]   http://www.yaml.org/spec/1.2/spec.html#l-trail-comments(n)
    member this.``l-trail-comments`` ps = (this.``s-indent(<n)`` ps) + this.``c-nb-comment-text`` + this.``b-comment`` + ZOM(this.``l-comment``)

    //  [170]   http://www.yaml.org/spec/1.2/spec.html#c-l+literal(n)
    member this.``c-l+literal`` (ps:ParseState) = 
        logger "c-l+literal" ps
        let trimIndent pst (slist: string list) =
            let skipIndent s = 
                match s with
                |   Regex(RGS(this.``s-indent(n)`` pst))    _ -> s.Substring(pst.n)
                |   Regex(RGS(this.``s-indent(<n)`` pst))   _ -> ""
                |   _ -> failwith (sprintf "Problem with indentation: %s" s)
            let unIndent s = if s <> "" then skipIndent s else s

            let ``l-empty`` = RGSF((this.``s-line-prefix`` (pst.SetStyleContext Context.``Block-in``)) ||| (this.``s-indent(<n)`` pst))
            let ``l-literaltext`` = RGSF((this.``s-indent(n)`` pst) + OOM(this.``nb-char``))

            let trimTail sin sout =
                match sin with
                |   []  -> sout |> List.rev |> FallibleOption<_>.Value
                |   h :: _ ->
                    let patt = this.``l-chomped-empty`` pst + RGP("\\z", [Token.EOF])
                    if (h="") || IsMatchStr(h, patt) then sout |> List.rev |> FallibleOption<_>.Value
                    else 
                        ps.AddErrorMessage <| MessageAtLine.CreateTerminate (ps.Location) MessageCode.ErrBadFormatLiteral (lazy sprintf "Unexpected characters '%s'" h) 

            let rec trimMain sin sout =
                match sin with
                |   []  -> sout |> List.rev |> FallibleOption<_>.Value
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
                |   []  -> sout |> List.rev |> FallibleOption<_>.Value
                |   h :: rest ->
                    if (h="") then
                        trimHead rest (unIndent h :: sout)
                    else
                        let tooManySpaces = RGSF((this.``s-indent(n)`` pst) + OOM(RGP (this.``s-space``, [Token.``t-space``])))
                        match h with
                        |   Regex(``l-empty``)  _ -> trimHead rest (unIndent h :: sout)
                        |   Regex(tooManySpaces) _ -> ps.AddErrorMessage <| MessageAtLine.CreateContinue (pst.Location) MessageCode.ErrTooManySpacesLiteral (lazy "A leading all-space line must not have too many spaces.")
                        |   _ -> trimMain sin sout
            trimHead slist []


        let ck = ScalarMemoizeKey.Create (ps.Input.Position) 170

        if this.Caching.ContainsKey(ck) then
            logger "> c-l+literal uncache" ps
            this.UncacheNode ck ps
        else
            ps |> ParseState.``Match and Advance`` (RGP ("\\|", [Token.``t-pipe``])) (fun prs ->
                let ``literal-content`` (ps:ParseState) =
                    let ps = if ps.n < 1 then (ps.SetIndent 1) else ps
                    let p = this.``l-literal-content`` ps
                    match ps.Input.Data  with
                    |   Regex2(p)  m -> FallibleOption<_>.Value(m.ge1, ps |> ParseState.TrackPosition m.FullMatch)
                    |   _ -> FallibleOption<_>.NoResult()
                (this.``c-b-block-header`` prs)
                |> FallibleOption.bind(fun (pm, prs2) ->
                    match pm with
                    |   Some(m) -> FallibleOption<_>.Value m
                    |   None    ->
                        let literalcontent = (``literal-content`` prs2) 
                        match literalcontent.Result with
                        |   FallibleOption.Value ->  
                            let (ms, _) = literalcontent.Data
                            let split = ms |> this.``split by linefeed`` 
                            let aut = split |> this.``auto detect indent in block`` prs2.n
                            if aut < 0 then failwith "Autodetected indentation is less than zero"
                            prs2.Input.Reset()
                            FallibleOption<_>.Value aut
                        |   _  -> prs2.AddErrorMessage <| MessageAtLine.CreateContinue (prs2.Location) MessageCode.ErrTooLessIndentedLiteral (lazy "Could not detect indentation of literal block scalar after '|'")
                    |> FallibleOption.bind(fun m ->
                        (``literal-content`` (prs2 |> ParseState.SetIndent (prs2.n+m) |> ParseState.SetSubIndent 0))
                        |> FallibleOption.bind(fun (ms, ps2) ->  
                            if ms = "" then
                                let detectLessIndented = (``literal-content`` (prs2 |> ParseState.SetIndent 1 |> ParseState.SetSubIndent 0))
                                match detectLessIndented.Result with
                                |   FallibleOption.Value ->  ps2.AddErrorMessage <| MessageAtLine.CreateContinue (ps2.Location) MessageCode.ErrTooLessIndentedLiteral (lazy "The text is less indented than the indicated level.")
                                |   _ -> ps2.AddErrorMessage <| MessageAtLine.CreateContinue (ps2.Location) MessageCode.ErrBadFormatLiteral (lazy "The literal has bad syntax.")
                            else
                                let dl = ParseState.PositionDelta ms ||> DocumentLocation.Create
                                let split = ms |> this.``split by linefeed`` 
                                let mapScalar (s, prs) =  
                                    CreateScalarNode (NonSpecific.NonSpecificTagQT) (getParseInfo ps prs) (s)
                                    |> this.ResolveTag prs NonSpecificQT (prs.Location)
                                split 
                                |> trimIndent ps2
                                |> FallibleOption.map(fun prs -> 
                                    let s = 
                                        prs
                                        |> this.``chomp lines`` ps2 
                                        |> this.``join lines``
    #if DEBUG
                                    logger (sprintf "c-l+literal value: %s" s) ps2
    #endif
                                    (s, ps2 |> ParseState.Advance)
                                    )
                                |> FallibleOption.bind mapScalar
                                |> this.PostProcessAndValidateNode
                                |> this.CacheNode ck prs dl (ms.Length)
                        )
                    )
                )
            )
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-l+literal" ps

    //  [171]   http://www.yaml.org/spec/1.2/spec.html#l-nb-literal-text(n)
    member this.``l-nb-literal-text`` (ps:ParseState) = ZOM(this.``l-empty`` (ps.SetStyleContext Context.``Block-in``)) + (this.``s-indent(n)`` ps) + OOM(this.``nb-char``)

    //  [172]   http://www.yaml.org/spec/1.2/spec.html#b-nb-literal-next(n)
    member this.``b-nb-literal-next`` ps = this.``b-as-line-feed`` + (this.``l-nb-literal-text`` ps)
    
    //  [173]   http://www.yaml.org/spec/1.2/spec.html#l-literal-content(n,t)
    member this.``l-literal-content`` (ps:ParseState) = 
        let memFunc ps =
            GRP(OPT((this.``l-nb-literal-text`` ps) + ZOM(this.``b-nb-literal-next`` ps) + (this.``b-chomped-last`` ps)) + (this.``l-chomped-empty`` ps))
        let callMemoized = this.Memoize memFunc
        callMemoized (173 (* member num *) , ps.n, ps.c) ps


    //  [174]   http://www.yaml.org/spec/1.2/spec.html#c-l+folded(n)
    member this.``c-l+folded`` ps =
        logger "c-l+folded" ps

        let ``block fold lines`` ps (strlst: string list) =
            let IsTrimmable s = IsMatchStr(s, RGSF((this.``s-line-prefix`` ps) ||| (this.``s-indent(<n)`` ps)))
            let IsSpacedText s = IsMatchStr(s, RGSF(this.``s-nb-spaced-text`` ps))

            let skipIndent s = 
                if IsMatchStr(s, this.``s-indent(n)`` ps) then s.Substring(ps.n)
                else raise (ParseException "Problem with indentation")
            let unIndent s = if s <> "" then skipIndent s else s

            let rec trimLines (inLines: string list) outLines noFold =
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

            let rec foldEverything (inLines: string list) outLines folded =
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
            foldEverything strlst [] false |> List.map(unIndent)

        let ck = ScalarMemoizeKey.Create (ps.Input.Position) 174

        if this.Caching.ContainsKey(ck) then
            logger "> c-l+folded uncache" ps
            this.UncacheNode ck ps
        else
            ps |> ParseState.``Match and Advance`` (this.``c-folded``) (fun prs ->
                let ``folded-content`` (ps:ParseState) =
                    let ps = if ps.n < 1 then ps.SetIndent 1 else ps
                    let patt = this.``l-folded-content`` (ps.FullIndented)
                    match ps with
                    |   Regex3(patt)  (m,p) -> FallibleOption<_>.Value(m.ge1, p |> ParseState.TrackPosition m.FullMatch)
                    |   _ -> FallibleOption<_>.NoResult()

                (this.``c-b-block-header`` prs)
                |> FallibleOption.bind(fun (pm, prs2) -> 
                    match pm with
                    |   Some(m) -> FallibleOption<_>.Value m
                    |   None    ->
                        let foldedcontent = (``folded-content`` prs2) 
                        match foldedcontent.Result with
                        |   FallibleOption.Value ->  
                            let (ms, _) = foldedcontent.Data
                            let split = ms |> this.``split by linefeed`` 
                            let aut = split |> this.``auto detect indent in block`` prs2.n
                            if aut < 0 then failwith "Autodetected indentation is less than zero"
                            prs2.Input.Reset()
                            FallibleOption<_>.Value aut
                        |   _  -> prs2.AddErrorMessage <| MessageAtLine.CreateContinue (prs2.Location) MessageCode.ErrTooLessIndentedLiteral (lazy "Could not detect indentation of literal block scalar after '>'")
                    |> FallibleOption.bind(fun m ->
                        let mapScalar (s, prs) =  
                            CreateScalarNode (NonSpecific.NonSpecificTagQT) (getParseInfo ps prs) (s)
                            |> this.ResolveTag prs NonSpecificQT (prs.Location)
                        (``folded-content`` (prs2 |> ParseState.SetIndent (prs2.n+m) |> ParseState.SetSubIndent 0))
                        |> FallibleOption.bind(fun (ms, ps2) -> 
                            let dl = ParseState.PositionDelta ms ||> DocumentLocation.Create

                            let s = 
                                ms 
                                |> this.``split by linefeed`` 
                                |> ``block fold lines`` ps2
                                |> this.``chomp lines`` ps2 
                                |> this.``join lines``
                            FallibleOption<_>.Value(s, ps2 |> ParseState.Advance)
                            |> FallibleOption.bind mapScalar
                            |> this.PostProcessAndValidateNode
                            |> this.CacheNode ck prs dl (ms.Length)
                        )
                    )
                )
             )
        |> ParseState.ResetEnv ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-l+folded" ps 


    //  [175]   http://www.yaml.org/spec/1.2/spec.html#s-nb-folded-text(n)
    member this.``s-nb-folded-text`` ps = (this.``s-indent(n)`` ps) + ZOM(this.``nb-char``)

    //  [176]   http://www.yaml.org/spec/1.2/spec.html#l-nb-folded-lines(n)
    member this.``l-nb-folded-lines`` (ps:ParseState) = (this.``s-nb-folded-text`` ps) + ZOM((this.``b-l-folded`` (ps.SetStyleContext Context.``Block-in``)) + this.``s-nb-folded-text`` ps)

    //  [177]   http://www.yaml.org/spec/1.2/spec.html#s-nb-spaced-text(n)
    member this.``s-nb-spaced-text`` ps = (this.``s-indent(n)`` ps) + this.``s-white`` + ZOM(this.``nb-char``)

    //  [178]   http://www.yaml.org/spec/1.2/spec.html#b-l-spaced(n)
    member this.``b-l-spaced`` (ps:ParseState) = this.``b-as-line-feed`` + ZOM(this.``l-empty`` (ps.SetStyleContext Context.``Block-in``))

    //  [179]   http://www.yaml.org/spec/1.2/spec.html#l-nb-spaced-lines(n)
    member this.``l-nb-spaced-lines`` ps = (this.``s-nb-spaced-text`` ps) + ZOM((this.``b-l-spaced``ps) + (this.``s-nb-spaced-text`` ps))

    //  [180]   http://www.yaml.org/spec/1.2/spec.html#l-nb-same-lines(n)
    member this.``l-nb-same-lines`` (ps:ParseState) = 
        ZOM(this.``l-empty`` (ps.SetStyleContext Context.``Block-in``)) + ((this.``l-nb-folded-lines`` ps) ||| (this.``l-nb-spaced-lines`` ps))

    //  [181]   http://www.yaml.org/spec/1.2/spec.html#l-nb-diff-lines(n)
    member this.``l-nb-diff-lines`` ps = (this.``l-nb-same-lines`` ps) + ZOM(this.``b-as-line-feed`` + (this.``l-nb-same-lines`` ps))

    //  [182]   http://www.yaml.org/spec/1.2/spec.html#l-folded-content(n,t)
    member this.``l-folded-content`` (ps:ParseState) =
        let memFunc ps =
            GRP(OPT((this.``l-nb-diff-lines`` ps) + (this.``b-chomped-last`` ps))) + (this.``l-chomped-empty`` ps)
        let callMemoized = this.Memoize memFunc
        callMemoized (182 (* member num *) , ps.n, ps.c) ps

    //  [183]   http://www.yaml.org/spec/1.2/spec.html#l+block-sequence(n)
    member this.``l+block-sequence`` (ps:ParseState) = 
        logger "l+block-sequence" ps
        let m = this.``auto detect indent in line`` ps
        if m < 1 then 
            if ps.Input.Peek().Token = Token.``t-quotationmark`` then
                ps.AddErrorMessage <| CreateErrorMessage.TabIndentError ps
            else
                FallibleOption<_>.NoResult()
        else
            let rec ``l+block-sequence`` (psp:ParseState) (acc: Node list) =
                let contentOrNone rs psr = 
                    if (ParseState.HasNoTerminatingError psr) then
                        if (acc.Length = 0) then rs
                        else 
                            CreateSeqNode (NonSpecific.NonSpecificTagQM) (getParseInfo ps psr) acc 
                            |> this.ResolveTag psp NonSpecificQM (psp.Location)
                            |> this.PostProcessAndValidateNode
                    else
                        let prsc = psr |> ParseState.ProcessErrors
                        FallibleOption<_>.ErrorResult()

                let pspf = psp.FullIndented
                let clblockseqentry = pspf |> ParseState.``Match and Advance`` (this.``s-indent(n)`` pspf) (this.``c-l-block-seq-entry``)
                match clblockseqentry.Result with
                |   FallibleOption.Value -> 
                    let (c, prs2) = clblockseqentry.Data
                    ``l+block-sequence`` prs2 (c :: acc)
                |   FallibleOption.NoResult       -> contentOrNone (FallibleOption<_>.NoResult()) psp
                |   FallibleOption.ErrorResult    -> psp |> contentOrNone (FallibleOption<_>.ErrorResult())
                | _ -> failwith "Illegal value for clblockseqentry"
            let ps = ps.SetSubIndent m
            ``l+block-sequence`` ps []
        |> ParseState.ResetEnv ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "l+block-sequence" ps

    //  [184]   http://www.yaml.org/spec/1.2/spec.html#c-l-block-seq-entry(n)
    member this.``c-l-block-seq-entry`` ps =
        logger "c-l-block-seq-entry" ps

        ps |> ParseState.``Match and Advance`` (RGP("-", [Token.``t-hyphen``])) (fun prs ->
            if IsMatch(prs.Input.Data, (this.``ns-char``)) then // Not followed by an ns-char
                FallibleOption<_>.NoResult()
            else
                //prs.Input.Reset()
                let prs = prs.SetStyleContext Context.``Block-in``
                this.``s-l+block-indented`` prs
        )
        |> ParseState.ResetEnv ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-l-block-seq-entry" ps

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
                        ifneither (FallibleOption<_>.NoResult())
                    }
                    |> ParseState.PreserveErrors ps
            )

        (ps |> ParseState.OneOf) {
            either (``indented compact``)
            either (this.``s-l+block-node``)
            ifneitherfn (fun() ->
                let prs2 = ps.SkipIfMatch (this.``e-node`` + this.``s-l-comments``)
                FallibleOption<_>.Value(this.ResolvedNullNode prs2, prs2))
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> ParseState.ResetEnv ps
        |> this.LogReturn "s-l+block-indented" ps

    //  [186]   http://www.yaml.org/spec/1.2/spec.html#ns-l-compact-sequence(n)
    member this.``ns-l-compact-sequence`` ps = 
        logger "ns-l-compact-sequence" ps
        let rec ``ns-l-compact-sequence`` psp (acc: Node list) =
            let contentOrNone rs psr = 
                if (acc.Length = 0) then rs
                else 
                    CreateSeqNode (NonSpecific.NonSpecificTagQM) (getParseInfo ps psr) acc 
                    |> this.ResolveTag psr NonSpecificQM (psr.Location)
                    |> this.PostProcessAndValidateNode

            if not(IsMatch(psp.Input.Data, (this.``s-indent(n)`` psp))) then
                let ilen =
                    match HasMatches(psp.Input.Data, ZOM(RGP(this.``s-space``,[Token.``t-space``]))) with
                    |   (true, mt) -> mt.Length
                    |   (false, _) -> 0
                psp.Input.Reset()
                if (ilen > psp.n) || (psp.n :: psp.IndentLevels) |> List.contains(ilen) then
                    if psp.Input.EOF then
                        contentOrNone (FallibleOption<_>.NoResult()) psp
                    else
                        let ws = psp.Input.Data.Take()
                        psp.Input.Reset()
                        if ws.Token = Token.``t-tab`` then
                            psp.AddErrorMessage <| CreateErrorMessage.TabIndentError psp
                        else
                            contentOrNone (FallibleOption<_>.NoResult()) psp
                else
                    psp.AddErrorMessage <| CreateErrorMessage.IndentLevelError psp
            else
                //psp.Input.Reset()
                let clblockseqentry = psp |> ParseState.``Match and Advance`` (this.``s-indent(n)`` psp) (this.``c-l-block-seq-entry``)
                match clblockseqentry.Result  with
                |   FallibleOption.Value -> 
                    let (c, prs2) = clblockseqentry.Data
                    ``ns-l-compact-sequence`` (prs2.Advance()) (c :: acc)
                |   FallibleOption.NoResult        -> contentOrNone (FallibleOption<_>.NoResult()) psp
                |   FallibleOption.ErrorResult   -> psp |> contentOrNone (FallibleOption<_>.ErrorResult())
                | _ -> failwith "Illegal value for clblockseqentry"
        let clblockseqentry = (this.``c-l-block-seq-entry`` ps) 
        match clblockseqentry.Result with
        |   FallibleOption.Value  -> 
            let (c, prs) = clblockseqentry.Data
            ``ns-l-compact-sequence`` prs [c]
        |   FallibleOption.NoResult   -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
        | _ -> failwith "Illegal value for clblockseqentry"
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "ns-l-compact-sequence" ps

    //  [187]   http://www.yaml.org/spec/1.2/spec.html#l+block-mapping(n)
    member this.``l+block-mapping`` ps =
        logger "l+block-mapping" ps
        let m = this.``auto detect indent in line`` ps
        if m < 1 then 
            if ps.Input.Peek().Token = Token.``t-tab`` then
                ps.AddErrorMessage <| CreateErrorMessage.TabIndentError ps
            else
                FallibleOption<_>.NoResult() 
        else
            let rec ``l+block-mapping`` (psp:ParseState) (acc:(Node*Node) list) = 
                let contentOrNone rs psr = 
                    if (ParseState.HasNoTerminatingError psr) then
                        if (acc.Length = 0) then rs
                        else 
                            CreateMapNode (NonSpecific.NonSpecificTagQM) (getParseInfo ps psr) acc 
                            |> this.ResolveTag psr NonSpecificQM (psr.Location)
                            |> this.PostProcessAndValidateNode
                    else
                        let prsc = psr |> ParseState.ProcessErrors
                        FallibleOption<_>.ErrorResult()

                let nslblockmapentry = (psp.FullIndented) |> ParseState.``Match and Advance`` (this.``s-indent(n)`` psp.FullIndented) (this.``ns-l-block-map-entry``)
                match nslblockmapentry.Result with
                |   FallibleOption.Value  -> 
                    let ((ck, cv), prs) = nslblockmapentry.Data
                    ``l+block-mapping`` prs ((ck,cv) :: acc)
                |   FallibleOption.NoResult    ->  contentOrNone (FallibleOption<_>.NoResult()) psp
                |   FallibleOption.ErrorResult -> psp |> contentOrNone (FallibleOption<_>.ErrorResult())
                | _ -> failwith "Illegal value for nslblockmapentry"
            let ps = ps.SetSubIndent m
            ``l+block-mapping`` ps []
        |> ParseState.ResetEnv ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "l+block-mapping" ps

    //  [188]   http://www.yaml.org/spec/1.2/spec.html#ns-l-block-map-entry(n)
    member this.``ns-l-block-map-entry`` (ps:ParseState) : ParseFuncResult<_> = 
        logger "ns-l-block-map-entry" ps
        (ps |> ParseState.OneOf) {
            either (this.``c-l-block-map-explicit-entry``)
            either (this.``ns-l-block-map-implicit-entry``)
            ifneither (FallibleOption<_>.NoResult())
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "ns-l-block-map-entry" ps

    //  [189]   http://www.yaml.org/spec/1.2/spec.html#c-l-block-map-explicit-entry(n)
    member this.``c-l-block-map-explicit-entry`` (ps:ParseState) : ParseFuncResult<_> =
        logger "c-l-block-map-explicit-entry" ps
        let clblockmapexplicitkey = (this.``c-l-block-map-explicit-key`` ps) 
        match clblockmapexplicitkey.Result with
        |   FallibleOption.Value ->
            let (ck, prs1) = clblockmapexplicitkey.Data
            let noResult prs1 =
                prs1 |> ParseState.``Match and Advance`` (this.``e-node``) (fun prs2 -> FallibleOption<_>.Value((ck, this.ResolvedNullNode prs2), prs2))
            let lblockmapexplicitvalue = (this.``l-block-map-explicit-value`` prs1) 
            match lblockmapexplicitvalue.Result with
            |   FallibleOption.Value  -> 
                let (cv, prs1) = lblockmapexplicitvalue.Data
                FallibleOption<_>.Value((ck, cv), prs1)
            |   FallibleOption.NoResult  -> prs1 |> noResult
            |   FallibleOption.ErrorResult -> prs1 |> noResult
            | _ -> failwith "Illegal value for lblockmapexplicitvalue"
        |   FallibleOption.NoResult -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
        | _ -> failwith "Illegal value for clblockmapexplicitkey"
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-l-block-map-explicit-entry" ps

    //  [190]   http://www.yaml.org/spec/1.2/spec.html#c-l-block-map-explicit-key(n)
    member this.``c-l-block-map-explicit-key`` ps : ParseFuncResult<_> =
        logger "c-l-block-map-explicit-key" ps
        ps |> ParseState.``Match and Advance`` (this.``c-mapping-key``) (fun prs ->
            this.``s-l+block-indented`` (prs |> ParseState.SetStyleContext Context.``Block-out``)
        )
        |> ParseState.ResetEnv ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-l-block-map-explicit-key" ps

    //  [191]   http://www.yaml.org/spec/1.2/spec.html#l-block-map-explicit-value(n)
    member this.``l-block-map-explicit-value`` ps : ParseFuncResult<_> = 
        logger "l-block-map-explicit-value" ps
        ps |> ParseState.``Match and Advance`` ((this.``s-indent(n)`` ps) + this.``c-mapping-value``) (fun prs ->
            this.``s-l+block-indented`` (prs |> ParseState.SetStyleContext Context.``Block-out``)
        )
        |> ParseState.ResetEnv ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "l-block-map-explicit-value" ps

    //  [192]   http://www.yaml.org/spec/1.2/spec.html#ns-l-block-map-implicit-entry(n)
    member this.``ns-l-block-map-implicit-entry`` ps : ParseFuncResult<_> =
        logger "ns-l-block-map-implicit-entry" ps
        let matchValue (ck, prs) =
            let clblockmapimplicitvalue = (this.``c-l-block-map-implicit-value`` prs) 
            match clblockmapimplicitvalue.Result with
            |   FallibleOption.Value -> 
                let (cv, prs2) = clblockmapimplicitvalue.Data
                FallibleOption<_>.Value((ck, cv), prs2)
            |   FallibleOption.NoResult   -> FallibleOption<_>.NoResult()
            |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
            | _ -> failwith "Illegal value for clblockmapimplicitvalue"

        let noResult psp =
            if (ParseState.HasNoTerminatingError psp) then
                psp |> ParseState.``Match and Advance`` (this.``e-node``) (fun prs ->
                    (this.ResolveTag prs NonSpecificQM (prs.Location) (PlainEmptyNode (getParseInfo ps prs))) 
                    |> FallibleOption.bind(matchValue)
                )
            else
                let prsc = psp |> ParseState.ProcessErrors
                FallibleOption<_>.ErrorResult ()

        let nssblockmapimplicitkey = (this.``ns-s-block-map-implicit-key`` ps) 
        match nssblockmapimplicitkey.Result with
        |   FallibleOption.Value -> matchValue(nssblockmapimplicitkey.Data)
        |   FallibleOption.NoResult -> ps |> noResult 
        |   FallibleOption.ErrorResult -> ps |> noResult |> FallibleOption.ifnoresult(fun () -> FallibleOption<_>.ErrorResult())
        | _ -> failwith "Illegal value for nssblockmapimplicitkey"
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "ns-l-block-map-implicit-entry" ps

    //  [193]   http://www.yaml.org/spec/1.2/spec.html#ns-s-block-map-implicit-key
    member this.``ns-s-block-map-implicit-key`` ps : ParseFuncResult<_> = 
        logger "ns-s-block-map-implicit-key" ps
        (ps |> ParseState.SetStyleContext Context.``Block-key`` |> ParseState.OneOf) {
            either (this.``c-s-implicit-json-key``)
            either (this.``ns-s-implicit-yaml-key``)
            ifneither (FallibleOption<_>.NoResult())
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> ParseState.ResetEnv ps
        |> this.LogReturn "ns-s-block-map-implicit-key" ps

    //  [194]   http://www.yaml.org/spec/1.2/spec.html#c-l-block-map-implicit-value(n)
    member this.``c-l-block-map-implicit-value`` (ps:ParseState) : ParseFuncResult<_> =
        logger "c-l-block-map-implicit-value" ps
        ps |> ParseState.``Match and Advance`` (this.``c-mapping-value``) (fun prs ->
            let prs = prs.SetStyleContext Context.``Block-out``
            let noResult prs =
                if (ParseState.HasNoTerminatingError prs) then
                    prs |> ParseState.``Match and Advance`` (this.``e-node`` +  this.``s-l-comments``) (fun prs ->
                        this.ResolveTag prs NonSpecificQM (prs.Location) (PlainEmptyNode (getParseInfo ps prs))
                    )
                else
                    let prsc = prs |> ParseState.ProcessErrors
                    FallibleOption<_>.ErrorResult()

            let slblocknode = (this.``s-l+block-node`` prs) 
            match slblocknode.Result with
            |   FallibleOption.Value -> FallibleOption<_>.Value(slblocknode.Data)
            |   FallibleOption.NoResult        -> prs |> noResult
            |   FallibleOption.ErrorResult    -> prs |> noResult |> FallibleOption.ifnoresult(fun () -> FallibleOption<_>.ErrorResult())
            | _ -> failwith "Illegal value for slblocknode"
        )
        |> ParseState.ResetEnv ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "c-l-block-map-implicit-value" ps

    //  [195]   http://www.yaml.org/spec/1.2/spec.html#ns-l-compact-mapping(n)
    member this.``ns-l-compact-mapping`` ps =
        logger "ns-l-compact-mapping" ps
        let rec ``ns-l-compact-mapping`` psp (acc: (Node * Node) list) =
            let contentOrNone rs prs = 
                if (acc.Length = 0) then rs
                else 
                    CreateMapNode (NonSpecific.NonSpecificTagQM) (getParseInfo ps prs) acc 
                    |> this.ResolveTag prs NonSpecificQM (prs.Location)
                    |> this.PostProcessAndValidateNode

            let nslblockmapentry = psp |> ParseState.``Match and Advance`` (this.``s-indent(n)`` psp) (this.``ns-l-block-map-entry``)
            match nslblockmapentry.Result with
            |   FallibleOption.Value  ->  
                let ((ck, cv), prs) = nslblockmapentry.Data
                ``ns-l-compact-mapping`` prs ((ck,cv) :: acc)
            |   FallibleOption.NoResult ->  contentOrNone (FallibleOption<_>.NoResult()) psp
            |   FallibleOption.ErrorResult ->  psp |> contentOrNone (FallibleOption<_>.ErrorResult())
            | _ -> failwith "Illegal value for nslblockmapentry"
        let nslblockmapentry = (this.``ns-l-block-map-entry`` ps) 
        match nslblockmapentry.Result with
        |   FallibleOption.Value -> 
            let ((ck, cv), prs) = nslblockmapentry.Data
            ``ns-l-compact-mapping`` prs [(ck,cv)]
        |   FallibleOption.NoResult   -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
        | _ -> failwith "Illegal value for nslblockmapentry"
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn  "ns-l-compact-mapping" ps

    //  [196]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-node(n,c)
    member this.``s-l+block-node`` (ps:ParseState) : ParseFuncResult<_> =
        logger "s-l+block-node" ps
        (ps |> ParseState.OneOf) {
            either (this.``s-l+block-in-block``)
            either (this.``s-l+flow-in-block``)
            ifneither (FallibleOption<_>.NoResult())
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn  "s-l+block-node" ps

    //  [197]   http://www.yaml.org/spec/1.2/spec.html#s-l+flow-in-block(n)
    member this.``s-l+flow-in-block`` (ps:ParseState) : ParseFuncResult<_> =
        logger "s-l+flow-in-block" ps
        let prs = ps |> ParseState.SetIndent (ps.n + 1) |> ParseState.SetStyleContext Context.``Flow-out``
        prs |> ParseState.``Match and Advance`` (this.``s-separate`` prs) (fun prs ->
            let nsflownode = (this.``ns-flow-node`` prs) 
            match nsflownode.Result with
            |   FallibleOption.Value -> 
                let (c, prs2) = nsflownode.Data
                FallibleOption<_>.Value(c, prs2.SkipIfMatch (this.``s-l-comments``)) 
            |   FallibleOption.NoResult   -> FallibleOption<_>.NoResult()
            |   FallibleOption.ErrorResult -> FallibleOption<_>.ErrorResult()
            | _ -> failwith "Illegal value for nsflownode"
        )
        |> ParseState.ResetEnv ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "s-l+flow-in-block" ps

    //  [198]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-in-block(n,c)
    member this.``s-l+block-in-block`` (ps:ParseState) : ParseFuncResult<_> =
        logger "s-l+block-in-block" ps
        ps.OneOf {
            either (this.``s-l+block-scalar``)
            either (this.``s-l+block-collection``)
            ifneither (FallibleOption<_>.NoResult())
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "s-l+block-in-block" ps

    //  [199]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-scalar(n,c)
    member this.``s-l+block-scalar`` (ps:ParseState) : ParseFuncResult<_> =
        logger "s-l+block-scalar" ps
        let psp1 = ps.SetIndent (ps.n + 1)
        let ``literal or folded`` (psp:ParseState) =
            let psp1 = psp.SetIndent (psp.n + 1)
            psp1 |> ParseState.``Match and Advance`` (this.``s-separate`` psp1) (fun prs ->
                (prs |> ParseState.SetIndent (prs.n-1) |> ParseState.OneOf)
                    {
                        either   (this.``c-l+literal``)
                        either   (this.``c-l+folded``)
                        ifneither (FallibleOption<_>.NoResult())
                    }
                    |> ParseState.PreserveErrors psp
            )
        psp1 |> ParseState.``Match and Advance`` (this.``s-separate`` psp1) (fun prs ->
            (prs |> ParseState.SetIndent (prs.n-1) |> ParseState.OneOf)
                {
                    either(this.``content with properties`` ``literal or folded``)
                    either   (this.``c-l+literal``)
                    either   (this.``c-l+folded``)
                    ifneither (FallibleOption<_>.NoResult())
                }
                |> ParseState.PreserveErrors ps

        )
        |> ParseState.ResetEnv ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "s-l+block-scalar" ps

    //  [200]   http://www.yaml.org/spec/1.2/spec.html#s-l+block-collection(n,c)
    member this.``s-l+block-collection`` (ps:ParseState) : ParseFuncResult<_> =
        logger "s-l+block-collection" ps
        let omit f d1 _ = f d1
        let psp1 = ps.SetIndent (ps.n + 1)
        let ``seq or map`` (pls:ParseState) =
            let pls = pls.SkipIfMatch(this.``s-l-comments``) 
            let pls = pls.SetIndent (ps.n)
            pls.OneOf {
                either (omit this.``l+block-sequence`` (this.``seq-spaces`` pls))
                either (this.``l+block-mapping``)
                ifneither(FallibleOption<_>.NoResult())
            }
            |> ParseState.PreserveErrors ps
            
        let ``optional spaced content with properties`` ps =
            ps |> ParseState.``Match and Advance`` (this.``s-separate`` ps) (this.``content with properties`` ``seq or map``)

        psp1.OneOf {
            either (``optional spaced content with properties``)
            either (``seq or map``)
            ifneither (FallibleOption<_>.NoResult())
        }
        |> ParseState.PreserveErrors ps
        |> ParseState.TrackParseLocation ps
        |> this.LogReturn "s-l+block-collection" ps

    //  [201]   http://www.yaml.org/spec/1.2/spec.html#seq-spaces(n,c)
    member this.``seq-spaces`` ps = 
        match ps.c with
        |   Context.``Block-out``   ->  ps.SetIndent (ps.n-1)
        |   Context.``Block-in``    ->  ps
        |   _ ->    failwith "Unsupported document style."

    //  [202]   http://www.yaml.org/spec/1.2/spec.html#l-document-prefix
    member this.``l-document-prefix`` = OPT(this.``c-byte-order-mark``) + ZOM(this.``l-comment``)

    //  [203]   http://www.yaml.org/spec/1.2/spec.html#c-directives-end
    member this.``c-directives-end`` = this.``c-sequence-entry`` + this.``c-sequence-entry`` + this.``c-sequence-entry`` // RGP ("---", [Token.``c-directives-end``])

    //  [204]   http://www.yaml.org/spec/1.2/spec.html#c-document-end
    member this.``c-document-end`` = 
        let dot = RGP("\\.", [Token.``t-dot``])
        //RGP ("\\.\\.\\.", [Token.``c-document-end``])
        dot + dot + dot

    //  [205]   http://www.yaml.org/spec/1.2/spec.html#l-document-suffix
    member this.``l-document-suffix`` = this.``c-document-end`` + this.``s-l-comments``

    //  [206]   http://www.yaml.org/spec/1.2/spec.html#c-forbidden
    member this.``c-forbidden`` =
        (``start-of-line`` ||| this.``b-break``) +
        ( this.``c-directives-end`` ||| this.``c-document-end``) +
        (this.``b-char`` ||| this.``s-white`` ||| ``end-of-file``)

    //  [207]   http://www.yaml.org/spec/1.2/spec.html#l-bare-document
    member this.``l-bare-document`` (ps:ParseState) : ParseFuncResult<_> = 
        logger "l-bare-document" ps
        if ps.Errors = 0 then
            (* Excluding c-forbidden content *)
            if IsMatch(ps.Input.Data, this.``c-forbidden``) then
                FallibleOption<_>.NoResult()
            else
                //ps.Input.Reset()
                ps 
                |> ParseState.SetIndent -1
                |> ParseState.SetStyleContext Context.``Block-in``
                |> this.``s-l+block-node`` 
        else
            FallibleOption<_>.ErrorResult()
        |> this.LogReturn "l-bare-document" ps

    //  [208]   http://www.yaml.org/spec/1.2/spec.html#l-explicit-document
    member this.``l-explicit-document`` (ps:ParseState) : ParseFuncResult<_> =
        logger "l-explicit-document" ps
        if ps.Errors = 0 then
            ps 
            |> ParseState.``Match and Advance`` (this.``c-directives-end``) (fun prs ->
                (prs |> ParseState.OneOf)
                    {
                        either(this.``l-bare-document``)
                        ifneitherfn (fun () ->
                            let prs2 = prs.SkipIfMatch (this.``e-node`` + this.``s-l-comments``)
                            this.ResolveTag prs2 NonSpecificQM (prs2.Location) (PlainEmptyNode (getParseInfo ps prs) )
                        )
                    }
                |> ParseState.PreserveErrors ps
            )
        else FallibleOption<_>.ErrorResult ()
        |> this.LogReturn "l-explicit-document" ps

    //  [209]   http://www.yaml.org/spec/1.2/spec.html#l-directive-document
    member this.``l-directive-document`` (ps:ParseState) : ParseFuncResult<_> = 
        logger "l-directive-document" ps
        let rec readDirectives ps =
            let ldirective = (this.``l-directive`` ps) 
            match ldirective.Result with
            |   FallibleOption.Value -> readDirectives (ldirective.Data)
            |   FallibleOption.NoResult -> ps
            |   FallibleOption.ErrorResult -> ps
            | _ -> failwith "Illegal value for ldirective"
        let psr = readDirectives ps
        if psr.Errors = 0 then 
            if psr.Directives.Length > 0 then
                this.``l-explicit-document`` psr
            else
                FallibleOption<_>.NoResult()
        else
            FallibleOption<_>.ErrorResult()
        |> this.LogReturn "l-directive-document" ps

    //  [210]   http://www.yaml.org/spec/1.2/spec.html#l-any-document
    member this.``l-any-document`` (ps:ParseState) : ParseFuncResult<_> =
        logger "l-any-document" ps
        this.Caching.Clear()
        (ps |> ParseState.ResetDocumentParseState |> ParseState.OneOf) {
            either(this.``l-directive-document``)
            either(this.``l-explicit-document``)
            either(this.``l-bare-document``)
            ifneither (FallibleOption<_>.NoResult())
        }
        |> ParseState.PreserveErrors ps
        |> this.LogReturn "l-any-document" ps

    //  [211]   http://www.yaml.org/spec/1.2/spec.html#l-yaml-stream
    member this.``l-yaml-stream`` (input:string) : Representation list= 
        let ps = ParseState.Create input 
        logger "l-yaml-stream" ps

        let IsEndOfStream psp =
            let isEof() = psp.Input.EOF || psp.Input.Peek().Token = Token.EOF
            psp.Input.Data.Stream
            |>  Seq.takeWhile(fun (t) ->  [Token.``t-space`` ;Token.``t-tab`` ;Token.EOF] |> List.exists(fun te -> t.Token = te))
            |>  ignore
            if not(isEof()) then
                psp.Input.Reset()
                false
            else
                true


        ps |> 
        ParseState.``Match and Advance`` (ZOM(this.``l-document-prefix``)) (fun psr ->
            let addToList acc (r,ps) = (ps, r :: acc)

            let rec successorDoc (ps:ParseState, representations) =
                //  quitNode is a Sentinel value, which is realized via its tag
                let quitNode = Node.ScalarNode(NodeData<string>.Create (TagKind.NonSpecific (LocalTag.Create "#QUITNODE#" (this.GlobalTagSchema.LocalTags))) ("#ILLEGALVALUE#") (getParseInfo ps ps))
                let noResultNode = Node.ScalarNode(NodeData<string>.Create (TagKind.NonSpecific (LocalTag.Create "#NORESULTNODE#" (this.GlobalTagSchema.LocalTags))) ("#ILLEGALVALUE#") (getParseInfo ps ps))

                if not(IsEndOfStream ps) then
                    (ps |> ParseState.OneOf) {
                        either (ParseState.``Match and Advance`` (OOM(this.``l-document-suffix``) + ZOM(this.``l-document-prefix``)) (this.``l-any-document``))
                        either (ParseState.``Match and Advance`` (OOM(this.``l-document-suffix``) + ZOM(this.``l-document-prefix``)) (fun psr -> if (IsEndOfStream psr) then FallibleOption<_>.Value(noResultNode, psr) else FallibleOption<_>.Value(quitNode, psr))) // for missing ``l-any-document``; which is optional
                        either (ParseState.``Match and Advance`` (ZOM(this.``l-document-prefix``)) (this.``l-explicit-document``))
                        ifneither(ps.AddErrorMessage <| MessageAtLine.CreateContinue (ps.Location) MessageCode.Freeform (lazy "Incorrect Syntax, this content cannot be related to previous document structure."))
                    }
                    |> ParseState.PreserveErrors ps
                    |> ParseState.PostProcessErrors
                    |>  fun parsedDocument ->
                        match parsedDocument.Result with
                        |   FallibleOption.Value -> 
                            let (n:Node, ps2) = parsedDocument.Data
                            if (n.NodeTag.EqualIfNonSpecific(quitNode.NodeTag)) then 
                                FallibleOption<_>.NoResult()
                                |> ParseState.ToRepresentation ps 
                                |> addToList representations
                            else
                                if (n.NodeTag.EqualIfNonSpecific(noResultNode.NodeTag)) then
                                    (ps2, representations)
                                else 
                                    FallibleOption<_>.Value (n, ps2)
                                    |> ParseState.ToRepresentation ps 
                                    |> addToList representations
                                    |> successorDoc
                        |   FallibleOption.NoResult -> 
                                FallibleOption<_>.NoResult()
                                |> ParseState.ToRepresentation ps 
                                |> addToList representations
                        |   FallibleOption.ErrorResult ->
                                FallibleOption<_>.ErrorResult()
                                |> ParseState.ToRepresentation ps
                                |> fun (r,ps) -> (ps, r :: (representations |> List.skip 1))
                        | _ -> failwith "Illegal value for parsedDocument"
                else
                    (ps, representations)

            psr 
            |>  this.``l-any-document``
            |>  ParseState.PostProcessErrors
            |>  ParseState.ToRepresentation psr
            |>  function
                |   (NoRepresentation rr,ps2) -> 
                    (NoRepresentation rr,ps2)
                    |>  addToList []
                |   (EmptyRepresentation rr,ps2) -> 
                    (EmptyRepresentation rr,ps2)
                    |>  addToList []
                |   x -> 
                    x
                    |>  addToList []
                    |>  successorDoc
            |>  FallibleOption<_>.Value
        )
        |>  fun representationOption ->
            match representationOption.Result with
            | FallibleOption.Value  -> 
                let (_, representations) = representationOption.Data
                representations |> List.rev
            |   _ -> []



