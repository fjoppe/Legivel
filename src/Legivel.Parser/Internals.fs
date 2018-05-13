namespace Legivel.Internals

open Legivel.Common
open System.Diagnostics


module internal ParserMonads =
    [<DebuggerStepThrough>]
    type EitherBuilder<'a,'b,'c,'d>(context : 'c, resetNoRes:'c->unit, advance:'c->'c, addErr:'c->'b->'c, contAfterErr: 'c-> bool) =
        member this.Yield (_ : 'd) : 'c * FallibleOption<'a,'b> = (context, FallibleOption<_,_>.NoResult())

        [<CustomOperation("setcontext")>]
        member this.SetContext (((_:'c), pv), nw) = (nw, pv)

        [<CustomOperation("either")>]
        member this.Either (((ct:'c), (pv: FallibleOption<_,_>)), nw) =
            match pv.Result with
            |   FallibleOption.Value -> (ct, pv)
            |   FallibleOption.NoResult -> resetNoRes ct; if contAfterErr ct then (ct, nw ct) else (ct,FallibleOption<_,_>.NoResult())
            |   FallibleOption.ErrorResult -> 
                let e = pv.Error
                resetNoRes ct
                let ctn = addErr ct e
                if contAfterErr ctn then (ctn, nw ctn) else (ctn,FallibleOption<_,_>.NoResult())
            |   _ -> failwith "Illegal value for pv"


        [<CustomOperation("ifneither")>]
        member this.IfNeither (((ct:'c), (pv: FallibleOption<_,_>)), nw) = 
            match pv.Result with
            |   FallibleOption.NoResult    -> resetNoRes ct; if contAfterErr ct then (ct,nw) else (ct,FallibleOption<_,_>.NoResult())
            |   FallibleOption.Value       -> (advance ct, pv)
            |   FallibleOption.ErrorResult -> 
                let e = pv.Error
                resetNoRes ct;
                let ctn = addErr ct e            
                if contAfterErr ctn then (ctn,nw) else (ctn,FallibleOption<_,_>.NoResult())
            |   _ -> failwith "Illegal value for pv"

        [<CustomOperation("ifneitherfn")>]
        member this.IfNeitherFn (((ct:'c), (pv: FallibleOption<_,_>)), nw) = 
            match pv.Result with
            |   FallibleOption.NoResult    -> resetNoRes ct; if contAfterErr ct then (ct,nw()) else (ct,FallibleOption<_,_>.NoResult())
            |   FallibleOption.Value       -> (advance ct, pv)
            |   FallibleOption.ErrorResult -> 
                let e = pv.Error
                resetNoRes ct;
                let ctn = addErr ct e            
                if contAfterErr ctn then (ctn,nw()) else (ctn,FallibleOption<_,_>.NoResult())
            |   _ -> failwith "Illegal value for pv"


module internal Option =
    let ifnone f v=
        match v with
        |   None-> f()
        |   Some x   -> Some x
        
