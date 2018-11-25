module Legivel.Common

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
    let forCollection<'a,'b,'c> (f:'a*ProcessMessages -> (FallibleOption<'b>*ProcessMessages) list) (r:FallibleOption<'a>, pm:ProcessMessages) =
        match r.Result with
        |   FallibleOptionValue.NoResult -> [FallibleOption.NoResult(), pm]
        |   FallibleOptionValue.ErrorResult ->  [FallibleOption.ErrorResult(), pm]
        |   FallibleOptionValue.Value -> f (r.Data, pm)
        |   _ -> failwith "Illegal value for r"

    let errorsOrValues pm f l =
        let errors = l |> GetErrors
        if errors.Length > 0 then
            FallibleOption.ErrorResult(), pm
        else
            l
            |>  List.filter(fun (mr:FallibleOption<_>, pm) -> not(mr.IsNoResult))  // this may hide errors
            |>  f


type SerieBuilder(pm:ProcessMessages) =
    member this.Bind(mx: FallibleOption<'a>*ProcessMessages, f: 'a -> FallibleOption<'b>*ProcessMessages) : FallibleOption<'b>*ProcessMessages =
        let (vl,pm2) = mx
        match vl.Result with
        |   FallibleOptionValue.Value -> f (vl.Data)
        |   FallibleOptionValue.NoResult  -> FallibleOption.NoResult(), pm2
        |   FallibleOptionValue.ErrorResult  -> FallibleOption.ErrorResult(),pm2
        |   _ -> failwith "Illegal value for mx"

    member this.Return (x: 'a): FallibleOption<'a>*ProcessMessages = FallibleOption.Value x, pm


let faillableSequence pm = new SerieBuilder(pm)


module List =
    let choosefo<'a,'b, 'c> (f:'c->FallibleOption<'a>*ProcessMessages) l = l |> List.map(f) |> List.filter(fun (i:FallibleOption<'a>*ProcessMessages) -> not((fst i).IsNoResult))
    let tryFindFo<'a,'b, 'c> pm (f:'c->FallibleOption<'a>*ProcessMessages) l = 
        l
        |>  List.skipWhile(fun e -> 
                let (i,pm) = f e
                i.IsNoResult) 
        |>  function 
            | []    ->  FallibleOption.NoResult(), pm
            | h::_  ->  f h

type CrossMatch =
    |   None = 0
    |   Warn = 1
    |   Error= 2

type ProcessingOptions = {
        //UsePrimitiveDefaultWhenMissing : bool   // use default value for missing source data (primitive types only)
    CrossMatch : CrossMatch
}

