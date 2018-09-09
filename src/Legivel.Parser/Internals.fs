namespace Legivel.Internals

open Legivel.Common
open System.Diagnostics

module internal ParserMonads =
    [<Struct>]
    type EitherResult<'a,'b> = {
        Context : 'a
        Result'  : FallibleOption<'b>*ParseMessage
        HasErrorOccurred : bool
    }
    with
        static member Create c r = { Context = c; Result' = r; HasErrorOccurred = false}
        member this.SetContext c = { this with Context = c}
        member this.SetResult  r = { this with Result' = r }
        member this.SetError()   = { this with HasErrorOccurred = true }
        member this.Result with get() = this.Result' |> fst
        member this.ResultValue with get() = this.Result.Result
        member this.Messages with get() = (snd this.Result')


    //[<DebuggerStepThrough>]
    type EitherBuilder<'a,'c,'d>(context: 'c, messages : ParseMessage, resetNoRes:'c->unit, advance:'c->'c, contAfterErr: 'c-> bool) =
        member this.Yield (_ : 'd) : EitherResult<_,_> = EitherResult<_,_>.Create context (FallibleOption.NoResult(), messages)

        [<CustomOperation("setcontext")>]
        member this.SetContext (er:EitherResult<_,_>, nw) = er.SetContext nw

        [<CustomOperation("either")>]
        member this.Either (pv:EitherResult<_,_>, nw) =
            match pv.ResultValue with
            |   FallibleOptionValue.Value -> pv
            |   FallibleOptionValue.NoResult -> 
                resetNoRes pv.Context
                if contAfterErr pv.Context then 
                    pv.SetResult <| nw (pv.Context)
                else 
                    pv.SetResult (FallibleOption.NoResult(), pv.Messages)
            |   FallibleOptionValue.ErrorResult -> 
                resetNoRes <| pv.Context
                let ctn = pv.SetError()
                if contAfterErr ctn.Context then 
                    ctn.SetResult <| nw (ctn.Context)
                else 
                    ctn.SetResult (FallibleOption.ErrorResult(), ctn.Messages)
            |   _ -> failwith "Illegal value for pv"


        [<CustomOperation("ifneither")>]
        member this.IfNeither (pv:EitherResult<_,_>, nw) = 
            match pv.ResultValue with
            |   FallibleOptionValue.NoResult    -> 
                resetNoRes (pv.Context)
                if contAfterErr (pv.Context) then 
                    pv.SetResult (nw, pv.Messages)
                else 
                    pv.SetResult (FallibleOption.NoResult(), pv.Messages)
            |   FallibleOptionValue.Value  -> pv.Context |> advance |> pv.SetContext
            |   FallibleOptionValue.ErrorResult -> 
                resetNoRes pv.Context
                let ctn = pv.SetError()            
                if contAfterErr (pv.Context) then 
                    ctn.SetResult (nw, pv.Messages)
                else 
                    ctn.SetResult (FallibleOption.ErrorResult(),ctn.Messages)
            |   _ -> failwith "Illegal value for pv"

        [<CustomOperation("ifneitherpm")>]
        member this.IfNeitherPm (pv:EitherResult<_,_>, nw:FallibleOption<_>*ParseMessage) = 
            match pv.ResultValue with
            |   FallibleOptionValue.NoResult    -> 
                resetNoRes (pv.Context)
                if contAfterErr (pv.Context) then 
                    let pv = if (fst nw).Result = FallibleOptionValue.ErrorResult then pv.SetError() else pv
                    pv.SetResult (nw)
                else 
                    pv.SetResult (FallibleOption.NoResult(), pv.Messages)
            |   FallibleOptionValue.Value  -> pv.Result.Data |> snd |> advance |> pv.SetContext
            |   FallibleOptionValue.ErrorResult -> 
                resetNoRes pv.Context
                let ctn = pv.SetError()            
                if contAfterErr (pv.Context) then 
                    ctn.SetResult (nw)
                else 
                    ctn.SetResult (FallibleOption.ErrorResult(),ctn.Messages)
            |   _ -> failwith "Illegal value for pv"

        [<CustomOperation("ifneitherfn")>]
        member this.IfNeitherFn (pv:EitherResult<_,_>, nw:unit -> FallibleOption<_>*ParseMessage) = 
            match pv.ResultValue with
            |   FallibleOptionValue.NoResult    -> 
                resetNoRes (pv.Context)
                if contAfterErr (pv.Context) then 
                    let nw = nw()
                    let pv = if (fst nw).Result = FallibleOptionValue.ErrorResult then pv.SetError() else pv
                    pv.SetResult (nw)
                else 
                    pv.SetResult (FallibleOption.NoResult(),pv.Messages)
            |   FallibleOptionValue.Value -> pv.Result.Data |> snd |> advance |> pv.SetContext
            |   FallibleOptionValue.ErrorResult -> 
                resetNoRes pv.Context
                let ctn = pv.SetError()            
                if contAfterErr (pv.Context) then 
                    let nw = nw()
                    let ctn = if (fst nw).Result = FallibleOptionValue.ErrorResult then ctn.SetError() else ctn
                    ctn.SetResult (nw)
                else 
                    ctn.SetResult (FallibleOption.ErrorResult(),ctn.Messages)
            |   _ -> failwith "Illegal value for pv"


module internal Option =
    let ifnone f v=
        match v with
        |   None-> f()
        |   Some x   -> Some x
        
