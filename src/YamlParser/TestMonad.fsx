
type EitherMonad () =
    member this.Return (x : 'a) : Option<'a> = Some x
    member this.ReturnFrom (x : Option<'a>) : Option<'a> = x
    member this.Bind (result : Option<'a>, rest : unit -> Option<'b>) : Option<'b> =
        match result with
        | Some x -> Some x
        | None -> rest()

let OneOf= EitherMonad()

let f1 a = Some(a * 3)
let f2 a = Some(a * 4)

let (r1 : Option<int>) = OneOf {
        do! (f1 2)
        do! (f2 2)
        return! None
    }


type EitherMonad2<'a>(context : 'a) =
    member this.Yield (item : 'b) : 'a * Option<'a> = (context, None)

    member this.Parse i = EitherMonad2(i)

    [<CustomOperation("setcontext")>]
    member this.SetContext ((ct, pv), nw) = (nw, pv)

    [<CustomOperation("either")>]
    member this.Either ((ct, pv), nw) =
        match pv with
        |   None    -> (ct, nw <| ct)
        |   Some v  -> (ct, Some v)

    [<CustomOperation("ifneiter")>]
    member this.IfNeither ((ct, pv), nw) = 
        match pv with
        |   None    -> nw
        |   Some v  -> Some v

//    [<CustomOperation("context")>]
//    member this.Context ((ct, pv)) = ct


let OneOf2 = EitherMonad2(0)

let fa1 a = None // Some(a * 3)
let fa2 a = Some(a * 4)

let (r2 : Option<int>) = OneOf2.Parse 2 {
        either (fa1)
        setcontext 3
        either (fa2)
//        pintf "%A" context
        ifneiter (Some 2)
    }


