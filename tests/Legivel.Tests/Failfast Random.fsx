

type FallibleOptionEnum =
    |   Value       = 0
    |   NoResult    = 1
    |   ErrorResult = 2


type FallibleOption<'a,'b> = {
    Result     : FallibleOptionEnum
    DataValue  : 'a option
    ErrorValue : 'b option
}
    with
    member this.Data 
        with get() =
            match this.Result with
            |   FallibleOptionEnum.Value -> this.DataValue.Value
            |   _ -> failwith "This instance has no value"

    member this.Error
        with get() =
            match this.Result with
            |   FallibleOptionEnum.ErrorResult -> this.ErrorValue.Value
            |   _ -> failwith "This instance has no error"


    static member NoResult() = 
            { Result = FallibleOptionEnum.NoResult; DataValue = None; ErrorValue = None}

    static member ErrorResult e = { Result = FallibleOptionEnum.ErrorResult; DataValue = None; ErrorValue = Some e}

    static member Value v = { Result = FallibleOptionEnum.Value; DataValue = Some v; ErrorValue = None }



let bind<'a,'b,'c> (f: 'a -> FallibleOption<'b,'c>) (o:FallibleOption<'a,'c>) : FallibleOption<'b,'c> =
    match o.Result with
    |   FallibleOptionEnum.Value        -> f (o.Data)
    |   FallibleOptionEnum.NoResult     -> FallibleOption<'b,'c>.NoResult()
    |   FallibleOptionEnum.ErrorResult  -> FallibleOption<'b,'c>.ErrorResult (o.Error)
    |   _ -> failwith "FallibleOption: illegal value"


