type FallibleOption<'a,'b> =
    |   Value of 'a
    |   NoResult
    |   ErrorResult of 'b
    member this.Data 
        with get() =
            match this with
            |   Value v -> v
            |   _ -> failwith "This instance has no value"


//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//module FallibleOption =
//    let bind<'a,'b,'c> (f:'a -> FallibleOption<'b,'c>) o =
//        match o with
//        |   Value v -> f v
//        |   NoResult -> NoResult
//        |   ErrorResult e -> ErrorResult e
//    let map<'a,'b,'c> (f:'a -> 'b) (o:FallibleOption<'a,'c>) =
//        match o with
//        |   Value v -> Value (f v)
//        |   NoResult -> NoResult
//        |   ErrorResult e -> ErrorResult e

//    let ifnoresult<'a,'b,'c> f (o:FallibleOption<'a,'c>) =
//        match o with
//        |   NoResult    -> f()
//        |   Value v     -> Value v
//        |   ErrorResult e -> ErrorResult e

//    let Value v = Value v


type SerieBuilder() =
    member this.Bind(mx: FallibleOption<'a,'c>, f: 'a -> FallibleOption<'b,'c>) : FallibleOption<'b,'c> =
        match mx with
        |   Value v -> f v
        |   NoResult -> NoResult
        |   ErrorResult e -> ErrorResult e

    member this.Return (x: 'a): FallibleOption<'a,'c> = Value x

let serie = new SerieBuilder()

let MyFunc a : FallibleOption<int,unit> = a
let NoFunc() = NoResult 

let test = serie {
        let! a = NoFunc()
        let! b = MyFunc a
        return b
    }


