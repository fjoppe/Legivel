module Legivel.Common

open System
open System.Diagnostics
open ErrorsAndWarnings
open Legivel.Utilities.SpookyHash

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
[<Struct>]
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


type MessageAction =
    |   Continue
    |   Terminate
                        
[<DebuggerDisplay("{this.DebuggerInfo}")>]
[<Struct>]
type MessageAtLine = {
        Location: DocumentLocation
        Code    : MessageCode
        Action  : MessageAction
        Message : string
    }
    with
        static member CreateContinue dl cd s = {Location = dl; Code = cd; Action = Continue; Message = s}
        static member CreateTerminate dl cd s = {Location = dl; Code = cd; Action = Terminate; Message = s}

        member this.DebuggerInfo 
            with get() = sprintf "%s: %s" (this.Location.ToPrettyString()) (this.Message)


type ErrorMessage = MessageAtLine list

type FallibleOption =
    |   Value       = 0
    |   NoResult    = 1
    |   ErrorResult = 2


[<Struct>]
type FallibleOption<'a,'b> = private {
    Result'     : FallibleOption
    DataValue  : 'a option
    ErrorValue : 'b option
}
    with
    member this.Data 
       with get() =
            match this.Result' with
            |   FallibleOption.Value -> this.DataValue.Value
            |   _ -> failwith "This instance has no value"

    member this.Error
       with get() =
            match this.Result' with
            |   FallibleOption.ErrorResult -> this.ErrorValue.Value
            |   _ -> failwith "This instance has no error"

    member this.Result with get() = this.Result'

    static member NoResult() = { Result' = FallibleOption.NoResult; DataValue = None; ErrorValue = None}

    static member ErrorResult e = { Result' = FallibleOption.ErrorResult; DataValue = None; ErrorValue = Some e}

    static member Value v = { Result' = FallibleOption.Value; DataValue = Some v; ErrorValue = None }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FallibleOption =
    let bind<'a,'b,'c> (f: 'a -> FallibleOption<'b,'c>) (o:FallibleOption<'a,'c>) : FallibleOption<'b,'c> =
        match o.Result with
        |   FallibleOption.Value        -> f (o.Data)
        |   FallibleOption.NoResult     -> FallibleOption<'b,'c>.NoResult()
        |   FallibleOption.ErrorResult  -> FallibleOption<'b,'c>.ErrorResult (o.Error)
        |   _ -> failwith "FallibleOption: illegal value"

    let map<'a,'b,'c> f (o:FallibleOption<'a,'c>) : FallibleOption<'b,'c> =
        match o.Result with
        |   FallibleOption.NoResult     -> FallibleOption<'b,'c>.NoResult()
        |   FallibleOption.ErrorResult  -> FallibleOption<'b,'c>.ErrorResult (o.Error)
        |   FallibleOption.Value        -> FallibleOption<'b,'c>.Value (f (o.Data))
        |   _ -> failwith "FallibleOption: illegal value"

    let ifnoresult<'a,'b> f (o:FallibleOption<'a,'b>) : FallibleOption<'a,'b> =
        match o.Result with
        |   FallibleOption.NoResult    -> f()
        |   FallibleOption.Value       -> FallibleOption<'a,'b>.Value (o.Data)
        |   FallibleOption.ErrorResult -> FallibleOption<'a,'b>.ErrorResult (o.Error)
        |   _ -> failwith "FallibleOption: illegal value"

    //let Value v = FallibleOption<_,_>.Value v


