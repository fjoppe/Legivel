namespace Legivel.Internals

open Legivel.Common
open System.Diagnostics


module internal ParserMonads =
    [<DebuggerStepThrough>]
    type EitherBuilder<'a,'b,'c,'d>(context : 'c, resetNoRes:'c->unit, addErr:'c->'b->'c, contAfterErr: 'c-> bool) =
        member this.Yield (_ : 'd) : 'c * FallibleOption<'a,'b> = (context, NoResult)

        [<CustomOperation("setcontext")>]
        member this.SetContext (((_:'c), pv), nw) = (nw, pv)

        [<CustomOperation("either")>]
        member this.Either (((ct:'c), pv), nw) =
            match pv with
            |   Value v  -> (ct, Value v)
            |   NoResult -> resetNoRes ct; if contAfterErr ct then (ct, nw ct) else (ct,NoResult)
            |   ErrorResult e -> 
                resetNoRes ct
                let ctn = addErr ct e
                if contAfterErr ctn then (ctn, nw ctn) else (ctn,NoResult)

        [<CustomOperation("ifneither")>]
        member this.IfNeither (((ct:'c), pv), nw) = 
            match pv with
            |   NoResult      -> resetNoRes ct; if contAfterErr ct then (ct,nw) else (ct,NoResult)
            |   Value v       -> (ct, Value v)
            |   ErrorResult e -> 
                resetNoRes ct;
                let ctn = addErr ct e            
                if contAfterErr ctn then (ctn,nw) else (ctn,NoResult)



module internal Option =
    let ifnone f v=
        match v with
        |   None-> f
        |   Some x   -> Some x
        
