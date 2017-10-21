module Legivel.Common

open Legivel.Common
open Legivel.Customization.Utilities


type FallibleOption<'a,'b>
with
    member internal this.IsErrorResult =
        match this with
        |   ErrorResult _ -> true
        |   _ -> false

    member this.IsNoResult =
        match this with
        |   NoResult -> true
        |   _ -> false

module FallibleOption =
    let forCollection f =
        function
        |   NoResult -> [NoResult]
        |   ErrorResult e ->  [ErrorResult e]
        |   Value dt -> f dt

    let errorsOrValues f l =
        let errors = l |> GetErrors
        if errors.Length > 0 then
            ErrorResult errors
        else
            l
            |>  List.filter(fun mr -> not(mr.IsNoResult))  // this may hide errors
            |>  f


type SerieBuilder() =
    member this.Bind(mx: FallibleOption<'a,'c>, f: 'a -> FallibleOption<'b,'c>) : FallibleOption<'b,'c> =
        match mx with
        |   Value v -> f v
        |   NoResult -> NoResult
        |   ErrorResult e -> ErrorResult e

    member this.Return (x: 'a): FallibleOption<'a,'c> = Value x


let faillableSequence = new SerieBuilder()


module List =
    let choosefo<'a,'b, 'c> (f:'c->FallibleOption<'a,'b>) l = l |> List.map(f) |> List.filter(fun (i:FallibleOption<'a,'b>) -> not(i.IsNoResult))
    let tryFindFo<'a,'b, 'c> (f:'c->FallibleOption<'a,'b>) l = 
        l 
        |>  List.skipWhile(fun e -> 
                let i = f e
                i.IsNoResult) 
        |>  function 
            | []    ->  NoResult
            | h::_  ->  f h


