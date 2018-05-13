module Legivel.Common

open Legivel.Common
open Legivel.Customization.Utilities


type FallibleOption<'a,'b>
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
    let forCollection f (r:FallibleOption<_,_>) =
        match r.Result with
        |   FallibleOption.NoResult -> [r]
        |   FallibleOption.ErrorResult ->  [r]
        |   FallibleOption.Value -> f (r.Data)
        |   _ -> failwith "Illegal value for r"

    let errorsOrValues f l =
        let errors = l |> GetErrors
        if errors.Length > 0 then
            FallibleOption<_,_>.ErrorResult errors
        else
            l
            |>  List.filter(fun (mr:FallibleOption<_,_>) -> not(mr.IsNoResult))  // this may hide errors
            |>  f


type SerieBuilder() =
    member this.Bind(mx: FallibleOption<'a,'c>, f: 'a -> FallibleOption<'b,'c>) : FallibleOption<'b,'c> =
        match mx.Result with
        |   FallibleOption.Value -> f (mx.Data)
        |   _ -> mx

    member this.Return (x: 'a): FallibleOption<'a,'c> = FallibleOption<_,_>.Value x


let faillableSequence = new SerieBuilder()


module List =
    let choosefo<'a,'b, 'c> (f:'c->FallibleOption<'a,'b>) l = l |> List.map(f) |> List.filter(fun (i:FallibleOption<'a,'b>) -> not(i.IsNoResult))
    let tryFindFo<'a,'b, 'c> (f:'c->FallibleOption<'a,'b>) l = 
        l 
        |>  List.skipWhile(fun e -> 
                let i = f e
                i.IsNoResult) 
        |>  function 
            | []    ->  FallibleOption<_,_>.NoResult()
            | h::_  ->  f h


