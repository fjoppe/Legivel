namespace Legivel.Internals

open Legivel.Common
open System.Diagnostics


module internal ParserMonads =
    [<DebuggerStepThrough>]
    type EitherBuilder<'a,'b,'c,'d>(context : 'c, resetNoRes:'c->unit, advance:'c->'c, addErr:'c->'b->'c, contAfterErr: 'c-> bool) =
        member this.Yield (_ : 'd) : struct ('c * FallibleOption<'a,'b>) = struct (context, FallibleOption<_,_>.NoResult())

        [<CustomOperation("setcontext")>]
        member this.SetContext (struct ((_:'c), pv), nw) = struct (nw, pv)

        [<CustomOperation("either")>]
        member this.Either (struct ((ct:'c), (pv: FallibleOption<_,_>)), nw) =
            match pv.Result with
            |   FallibleOption.Value -> struct (ct, pv)
            |   FallibleOption.NoResult -> resetNoRes ct; if contAfterErr ct then struct (ct, nw ct) else struct (ct,FallibleOption<_,_>.NoResult())
            |   FallibleOption.ErrorResult -> 
                let e = pv.Error
                resetNoRes ct
                let ctn = addErr ct e
                if contAfterErr ctn then struct (ctn, nw ctn) else struct (ctn,FallibleOption<_,_>.NoResult())
            |   _ -> failwith "Illegal value for pv"


        [<CustomOperation("ifneither")>]
        member this.IfNeither (struct ((ct:'c), (pv: FallibleOption<_,_>)), nw) = 
            match pv.Result with
            |   FallibleOption.NoResult    -> resetNoRes ct; if contAfterErr ct then struct (ct,nw) else struct (ct,FallibleOption<_,_>.NoResult())
            |   FallibleOption.Value       -> struct (advance ct, pv)
            |   FallibleOption.ErrorResult -> 
                let e = pv.Error
                resetNoRes ct;
                let ctn = addErr ct e            
                if contAfterErr ctn then struct (ctn,nw) else struct (ctn,FallibleOption<_,_>.NoResult())
            |   _ -> failwith "Illegal value for pv"

        [<CustomOperation("ifneitherfn")>]
        member this.IfNeitherFn (struct ((ct:'c), (pv: FallibleOption<_,_>)), nw) = 
            match pv.Result with
            |   FallibleOption.NoResult    -> resetNoRes ct; if contAfterErr ct then struct (ct,nw()) else struct (ct,FallibleOption<_,_>.NoResult())
            |   FallibleOption.Value       -> struct (advance ct, pv)
            |   FallibleOption.ErrorResult -> 
                let e = pv.Error
                resetNoRes ct;
                let ctn = addErr ct e            
                if contAfterErr ctn then struct (ctn,nw()) else struct (ctn,FallibleOption<_,_>.NoResult())
            |   _ -> failwith "Illegal value for pv"


module internal Option =
    let ifnone f v=
        match v with
        |   None-> f()
        |   Some x   -> Some x
        
