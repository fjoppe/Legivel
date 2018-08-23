module Legivel.Common

open Legivel.Common
open Legivel.Customization.Utilities


type FallibleOption<'a>
with
    member internal this.IsErrorResult =
        match this.Result with
        |   FallibleOption.ErrorResult -> true
        |   _ -> false

    member this.IsNoResult =
        match this.Result with
        |   FallibleOption.NoResult -> true
        |   _ -> false

module FallibleOption =
    let forCollection<'a,'b,'c> (f:'a -> FallibleOption<'b> list) (r:FallibleOption<'a>) =
        match r.Result with
        |   FallibleOption.NoResult -> [FallibleOption<_>.NoResult()]
        |   FallibleOption.ErrorResult ->  [FallibleOption<_>.ErrorResult()]
        |   FallibleOption.Value -> f (r.Data)
        |   _ -> failwith "Illegal value for r"

    let errorsOrValues f l =
        let errors = l |> GetErrors
        if errors.Length > 0 then
            FallibleOption<_>.ErrorResult()
        else
            l
            |>  List.filter(fun (mr:FallibleOption<_>) -> not(mr.IsNoResult))  // this may hide errors
            |>  f


type SerieBuilder() =
    member this.Bind(mx: FallibleOption<'a>, f: 'a -> FallibleOption<'b>) : FallibleOption<'b> =
        match mx.Result with
        |   FallibleOption.Value -> f (mx.Data)
        |   FallibleOption.NoResult  -> FallibleOption<_>.NoResult()
        |   FallibleOption.ErrorResult  -> FallibleOption<_>.ErrorResult()
        |   _ -> failwith "Illegal value for mx"

    member this.Return (x: 'a): FallibleOption<'a> = FallibleOption<_>.Value x


let faillableSequence = new SerieBuilder()


module List =
    let choosefo<'a,'b, 'c> (f:'c->FallibleOption<'a>) l = l |> List.map(f) |> List.filter(fun (i:FallibleOption<'a>) -> not(i.IsNoResult))
    let tryFindFo<'a,'b, 'c> (f:'c->FallibleOption<'a>) l = 
        l 
        |>  List.skipWhile(fun e -> 
                let i = f e
                i.IsNoResult) 
        |>  function 
            | []    ->  FallibleOption<_>.NoResult()
            | h::_  ->  f h


