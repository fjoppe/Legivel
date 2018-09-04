﻿module Legivel.Common

open Legivel.Common
open Legivel.Customization.Utilities


type FallibleOption<'a>
with
    member internal this.IsErrorResult =
        match this.Result with
        |   FallibleOptionValue.ErrorResult -> true
        |   _ -> false

    member this.IsNoResult =
        match this.Result with
        |   FallibleOptionValue.NoResult -> true
        |   _ -> false

module FallibleOption =
    let forCollection<'a,'b,'c> (f:'a -> FallibleOption<'b> list) (r:FallibleOption<'a>) =
        match r.Result with
        |   FallibleOptionValue.NoResult -> [FallibleOption.NoResult()]
        |   FallibleOptionValue.ErrorResult ->  [FallibleOption.ErrorResult()]
        |   FallibleOptionValue.Value -> f (r.Data)
        |   _ -> failwith "Illegal value for r"

    let errorsOrValues f l =
        let errors = l |> GetErrors
        if errors.Length > 0 then
            FallibleOption.ErrorResult()
        else
            l
            |>  List.filter(fun (mr:FallibleOption<_>) -> not(mr.IsNoResult))  // this may hide errors
            |>  f


type SerieBuilder() =
    member this.Bind(mx: FallibleOption<'a>, f: 'a -> FallibleOption<'b>) : FallibleOption<'b> =
        match mx.Result with
        |   FallibleOptionValue.Value -> f (mx.Data)
        |   FallibleOptionValue.NoResult  -> FallibleOption.NoResult()
        |   FallibleOptionValue.ErrorResult  -> FallibleOption.ErrorResult()
        |   _ -> failwith "Illegal value for mx"

    member this.Return (x: 'a): FallibleOption<'a> = FallibleOption.Value x


let faillableSequence = new SerieBuilder()


module List =
    let choosefo<'a,'b, 'c> (f:'c->FallibleOption<'a>) l = l |> List.map(f) |> List.filter(fun (i:FallibleOption<'a>) -> not(i.IsNoResult))
    let tryFindFo<'a,'b, 'c> (f:'c->FallibleOption<'a>) l = 
        l 
        |>  List.skipWhile(fun e -> 
                let i = f e
                i.IsNoResult) 
        |>  function 
            | []    ->  FallibleOption.NoResult()
            | h::_  ->  f h


