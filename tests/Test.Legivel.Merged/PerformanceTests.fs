module PerformanceTests

open NUnit.Framework
open FsUnitTyped
open TestUtils
open Legivel.Traverse
open System.Text
open Legivel.TagResolution

//[<Test(Description="Test sinqle quote scalar performance")>]
let ``Performance Test - Single Quote Scalar``() =
    let sb = StringBuilder()

    sb.Append("TestKey:\n")

    //  a few phrases from: https://www.gutenberg.org/files/1184/1184-0.txt
    let randomPhrases = [
        "- 'When the young man on board saw this person approach, he left his'\n"
        "- 'much disliked by the crew as Edmond Dantès was beloved by them.'\n"
        "- 'course she had taken, and what was her cargo. I believe, if she had not'\n"
        "- 'three months after that; only be back again in three months, for the'\n"
    ]

    let cartesionProduct sl ll =
        let rec innerLoop ll csl acc =
            match (ll,csl) with
            |   ([], _) -> acc
            |   (_, []) -> innerLoop ll sl acc
            |   (hl::rl, hs::rs) -> innerLoop rl rs ((hl,hs)::acc)
        innerLoop ll sl []

    
    [0..23000]
    |>  cartesionProduct randomPhrases
    |>  List.map(fun (_, s) -> s)
    |>  List.fold(fun (s:StringBuilder) (i:string)  -> s.Append(i)) sb
    |>  ignore

    YamlParseForSchema (Failsafe.Schema) (sb.ToString())

    ()


