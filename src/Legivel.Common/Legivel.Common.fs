module Legivel.Common

open System
open System.Diagnostics
open ErrorsAndWarnings
open Legivel.Utilities.SpookyHash
open System.Text.RegularExpressions
open Legivel.Utilities.RegexDSL

module InternalUtil =
    let equalsOn f (yobj:obj) =
        match yobj with
        | :? 'T as y -> (f y)
        | _ -> false

    let compareOn f (yobj: obj) =
        match yobj with
        | :? 'T as y -> (f y)
        | _ -> invalidArg "yobj" "cannot compare values of different types"


[<CustomEquality; CustomComparison>]
[<StructuredFormatDisplay("{AsString}")>]
type NodeHash = private {
        hash1 : uint64
        hash2 : uint64
    }
    with
        static member Create (s:string) =
            let msg = System.Text.Encoding.Unicode.GetBytes(s)
            let (a,b) = Hash128 msg 0UL 0UL
            { hash1 = a; hash2 = b}

        static member Merge (nhl: NodeHash list) =
            let msg = 
                nhl
                |> List.map(fun nh -> 
                    let b1 = BitConverter.GetBytes(nh.hash1)
                    let b2 = BitConverter.GetBytes(nh.hash2)
                    Array.append b1 b2)
                |> Array.ofList
                |> Array.collect(fun e -> e)
            let (a,b) = Hash128 msg 0UL 0UL
            { hash1 = a; hash2 = b}
            

        static member private compare (a:NodeHash) (b:NodeHash) =
            if a.hash1 = b.hash1 then a.hash2.CompareTo(b.hash2)
            else a.hash1.CompareTo(b.hash1)

        override this.Equals(yobj) = yobj |> InternalUtil.equalsOn (fun that -> this.hash1 = that.hash1 && this.hash2 = that.hash2)
        
        override this.GetHashCode() = this.hash1.GetHashCode() ^^^ this.hash2.GetHashCode()

        override this.ToString() = sprintf "#%X%X" (this.hash1) (this.hash2)
        member m.AsString = m.ToString()

        interface System.IComparable with
            member this.CompareTo yobj = yobj |> InternalUtil.compareOn (fun that -> NodeHash.compare this that)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NodeHash = 
    let Create s = NodeHash.Create s
    let Merge nhl = NodeHash.Merge nhl


[<StructuredFormatDisplay("{AsString}")>]
[<StructuralEquality; CustomComparison>]
type    DocumentLocation = {
        Line    : int
        Column  : int
    }
    with
        static member Create l cl = { Line = l; Column = cl}
        static member Empty = { Line = 0; Column = 0}

        member this.AddAndSet l cl = { Line = this.Line+l; Column = cl}

        member this.ToPrettyString() = sprintf "line %d; column %d" this.Line this.Column

        override this.ToString() = sprintf "(l%d, c%d)" this.Line this.Column

        member m.AsString = m.ToString()

        interface System.IComparable with
            member this.CompareTo yobj =
               match yobj with
                 | :? DocumentLocation as that ->
                    let r = this.Line.CompareTo that.Line
                    if r = 0 then this.Column.CompareTo that.Column else r
                 | _ -> invalidArg "yobj" "cannot compare values of different types" 

type MessageAction =
    |   Continue
    |   Terminate
                        
[<DebuggerDisplay("{this.DebuggerInfo}")>]
type MessageAtLine = {
        Location: DocumentLocation
        Code    : MessageCode
        Action  : MessageAction
        Message : Lazy<string>
    }
    with
        static member CreateContinue dl cd s = {Location = dl; Code = cd; Action = Continue; Message = s}
        static member CreateTerminate dl cd s = {Location = dl; Code = cd; Action = Terminate; Message = s}

        member this.DebuggerInfo 
            with get() = sprintf "%s: %O %O" (this.Location.ToPrettyString()) (this.Code) (this.Action)


type MessageAtLineList = System.Collections.Generic.List<MessageAtLine>


[<NoComparison>]
type ParseMessage = {
        Warn  : MessageAtLineList
        Error : MessageAtLineList
        mutable Cancel: DocumentLocation // to cancel errors
        Terminates : MessageAtLineList
    }
    with
        static member Create() = {Warn = MessageAtLineList();Error=MessageAtLineList(); Cancel=DocumentLocation.Empty; Terminates=MessageAtLineList()}

        member this.AddError (mal:MessageAtLine)   = 
            this.Error.Add mal
            if mal.Action = MessageAction.Terminate then this.Terminates.Add mal
        member this.AddWarning (mal:MessageAtLine) = 
            this.Warn.Add mal
        member this.AddCancel mal   = this.Cancel <- mal
        member this.TrackPosition mal = this.Cancel <- mal; this
        member this.Unionise pm =
            if pm.Error.Count > 0 then this.Error.AddRange(pm.Error)
            if pm.Warn.Count > 0 then this.Warn.AddRange(pm.Warn)
            if pm.Terminates.Count > 0 then this.Terminates.AddRange(pm.Terminates)
            this

type FallibleOptionValue =
    |   Value       = 0
    |   NoResult    = 1
    |   ErrorResult = 2


type FallibleOption<'a> = private {
    Result'     : FallibleOptionValue
    DataValue  : 'a option
}
    with
    member this.Data 
       with get() =
            match this.Result' with
            |   FallibleOptionValue.Value -> this.DataValue.Value
            |   _ -> failwith "This instance has no value"

    member this.Result with get() = this.Result'


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FallibleOption =
    let NoResult() = { Result' = FallibleOptionValue.NoResult; DataValue = None}

    let ErrorResult() = { Result' = FallibleOptionValue.ErrorResult; DataValue = None}

    let Value v = { Result' = FallibleOptionValue.Value; DataValue = Some v}


    let bind<'a,'b,'c> (f: 'a -> FallibleOption<'b>*'c) (o:FallibleOption<'a>*'c) : FallibleOption<'b>*'c =
        let (fo, pm) = o
        match fo.Result with
        |   FallibleOptionValue.Value        -> f (fo.Data)
        |   FallibleOptionValue.NoResult     -> NoResult(), pm
        |   FallibleOptionValue.ErrorResult  -> ErrorResult(), pm
        |   _ -> failwith "FallibleOption: illegal value"

    let map<'a,'b,'c> f (o:FallibleOption<'a>*'c) : FallibleOption<'b>*'c =
        let (fo, pm) = o
        match fo.Result with
        |   FallibleOptionValue.NoResult     -> NoResult(), pm
        |   FallibleOptionValue.ErrorResult  -> ErrorResult(), pm
        |   FallibleOptionValue.Value        -> Value (f (fo.Data)), pm
        |   _ -> failwith "FallibleOption: illegal value"

    let ifnoresult<'a,'c> f (o:FallibleOption<'a>*'c) : FallibleOption<'a>*'c =
        let (fo, pm) = o
        match fo.Result with
        |   FallibleOptionValue.NoResult    -> f()
        |   FallibleOptionValue.Value       -> Value (fo.Data), pm
        |   FallibleOptionValue.ErrorResult -> ErrorResult (), pm
        |   _ -> failwith "FallibleOption: illegal value"


///// Returns list of match groups, for pattern p on string s
[<DebuggerStepThrough>]
let Match(s, p) = 
    let mt = Regex.Matches(s, RGS(p), RegexOptions.Multiline)
    if mt.Count = 0 then 
        []
    else
        [ for g in mt -> g.Value ]

/// Returns whether pattern p matches on string s
[<DebuggerStepThrough>]
let IsMatchStr(s, p) = 
    let ml = Match(s, p)
    ml.Length > 0


/// Regex Active pattern to match string pattern on string input, and returns a list of matches
[<DebuggerStepThrough>]
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.Multiline)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

