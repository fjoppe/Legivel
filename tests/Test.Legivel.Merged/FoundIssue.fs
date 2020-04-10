module FoundIssue

open NUnit.Framework
open FsUnitTyped
open TestUtils
open Legivel.Traverse


[<Test(Description="https://github.com/fjoppe/Legivel/issues/23")>]
let ``#23 Unable to deserialize sequence with compact indentation``() =
    let yaml = "
- behavior: merge
  envs:
  - base.env
  name: base-name
"

    let yml = YamlParse yaml

    let p = YamlPath.Create (sprintf "//[]/{#'behavior'}?" )
    yml |> p.Select |> ToScalar |> shouldEqual "merge"

    let p = YamlPath.Create (sprintf "//[]/{#'envs'}?/[]/#'base.env'" )
    yml |> p.Select |> ToScalar |> shouldEqual "base.env"

    let p = YamlPath.Create (sprintf "//[]/{#'name'}?/#" )
    yml |> p.Select |> ToScalar |> shouldEqual "base-name"

