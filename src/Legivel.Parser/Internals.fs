namespace Legivel.Internals

open Legivel.Common
open System.Diagnostics

module internal ParserMonads =
    [<Struct>]
    type EitherResult<'a,'b> = {
        Context : 'a
        Result  : FallibleOption<'b>
        HasErrorOccurred : bool
    }
    with
        static member Create c r = { Context = c; Result = r; HasErrorOccurred = false}
        member this.SetContext c = { this with Context = c}
        member this.SetResult  r = { this with Result = r }
        member this.SetError()   = { this with HasErrorOccurred = true }


    //[<DebuggerStepThrough>]
    type EitherBuilder<'a,'c,'d>(context : 'c, resetNoRes:'c->unit, advance:'c->'c, contAfterErr: 'c-> bool) =
        member this.Yield (_ : 'd) : EitherResult<_,_> = EitherResult<_,_>.Create context (FallibleOption<'a>.NoResult())

        [<CustomOperation("setcontext")>]
        member this.SetContext (er:EitherResult<_,_>, nw) = er.SetContext nw

        [<CustomOperation("either")>]
        member this.Either (pv:EitherResult<_,_>, nw) =
            match pv.Result.Result with
            |   FallibleOptionValue.Value -> pv
            |   FallibleOptionValue.NoResult -> 
                resetNoRes pv.Context
                if contAfterErr pv.Context then 
                    pv.SetResult <| nw (pv.Context)
                else 
                    pv.SetResult <| FallibleOption<_>.NoResult()
            |   FallibleOptionValue.ErrorResult -> 
                resetNoRes <| pv.Context
                let ctn = pv.SetError()
                if contAfterErr ctn.Context then 
                    ctn.SetResult <| nw (ctn.Context)
                else 
                    ctn.SetResult <| FallibleOption<_>.NoResult()
            |   _ -> failwith "Illegal value for pv"


        [<CustomOperation("ifneither")>]
        member this.IfNeither (pv:EitherResult<_,_>, nw) = 
            match pv.Result.Result with
            |   FallibleOptionValue.NoResult    -> 
                resetNoRes (pv.Context)
                if contAfterErr (pv.Context) then 
                    pv.SetResult <| nw
                else 
                    pv.SetResult <| FallibleOption<_>.NoResult()
            |   FallibleOptionValue.Value  -> pv.Context |> advance |> pv.SetContext
            |   FallibleOptionValue.ErrorResult -> 
                resetNoRes pv.Context
                let ctn = pv.SetError()            
                if contAfterErr (pv.Context) then 
                    ctn.SetResult <| nw
                else 
                    ctn.SetResult <| FallibleOption<_>.NoResult()
            |   _ -> failwith "Illegal value for pv"

        [<CustomOperation("ifneitherfn")>]
        member this.IfNeitherFn (pv:EitherResult<_,_>, nw) = 
            match pv.Result.Result with
            |   FallibleOptionValue.NoResult    -> 
                resetNoRes (pv.Context)
                if contAfterErr (pv.Context) then 
                    pv.SetResult <| nw() 
                else 
                    pv.SetResult <| FallibleOption<_>.NoResult()
            |   FallibleOptionValue.Value -> pv.Context |> advance |> pv.SetContext
            |   FallibleOptionValue.ErrorResult -> 
                resetNoRes pv.Context
                let ctn = pv.SetError()            
                if contAfterErr (pv.Context) then 
                    ctn.SetResult <| nw () 
                else 
                    ctn.SetResult <| FallibleOption<_>.NoResult()
            |   _ -> failwith "Illegal value for pv"


module internal Option =
    let ifnone f v=
        match v with
        |   None-> f()
        |   Some x   -> Some x
        
