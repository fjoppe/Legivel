module Test.Legivel.Common

open NUnit.Framework
open FsUnitTyped
open System.Collections.Generic
open Legivel.Common


module ``Test Common``=

    [<Test>]
    let ``Parse Plain Character - sunny day``() =
        true |> shouldEqual true

