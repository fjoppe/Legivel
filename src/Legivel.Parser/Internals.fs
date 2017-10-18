namespace Legivel.Internals

open Legivel.Common
open ErrorsAndWarnings
open System.Diagnostics


module internal ParserMonads =
    [<DebuggerStepThrough>]
    type EitherBuilder<'a,'b,'c,'d>(context : 'c, addErr:'c->'b->'c, contAfterErr: 'c-> bool) =
        member this.Yield (_ : 'd) : 'c * FallibleOption<'a,'b> = (context, NoResult)

        [<CustomOperation("setcontext")>]
        member this.SetContext (((_:'c), pv), nw) = (nw, pv)

        [<CustomOperation("either")>]
        member this.Either (((ct:'c), pv), nw) =
            match pv with
            |   Value v  -> (ct, Value v)
            |   NoResult -> if contAfterErr ct then (ct, nw ct) else (ct,NoResult)
            |   ErrorResult e -> 
                let ctn = addErr ct e
                if contAfterErr ctn then (ctn, nw ctn) else (ctn,NoResult)

        [<CustomOperation("ifneither")>]
        member this.IfNeither (((ct:'c), pv), nw) = 
            match pv with
            |   NoResult      -> if contAfterErr ct then (ct,nw) else (ct,NoResult)
            |   Value v       -> (ct, Value v)
            |   ErrorResult e -> 
                let ctn = addErr ct e            
                if contAfterErr ctn then (ctn,nw) else (ctn,NoResult)


type internal MessageAction =
    |   Continue
    |   Terminate
                        
[<DebuggerDisplay("{this.DebuggerInfo}")>]
type internal MessageAtLine = {
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


type internal ErrorMessage = MessageAtLine list


module internal Option =
    let ifnone f v=
        match v with
        |   None-> f
        |   Some x   -> Some x
        
