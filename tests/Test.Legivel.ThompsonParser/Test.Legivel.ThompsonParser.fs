module Test.Legivel.ThompsonParser

open Legivel.Tokenizer
open NUnit.Framework
open FsUnitTyped
open Legivel.Utilities.RegexDSL
open Legivel.ThompsonParser



let assertFullMatch nfa str =
    let r = parseIt nfa str
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual str

let assertPartialMatch nfa str strmatched =
    let r = parseIt nfa str
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual strmatched

let assertNoMatch nfa str =
    let r = parseIt nfa "XYC"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []
 
[<Test>]
let ``Simple Concat``() =
    let nfa = rgxToNFA <| RGP("A", [Token.``c-printable``]) + RGP("A", [Token.``c-printable``]) + RGP("B", [Token.``c-printable``])

    assertFullMatch nfa "AAB"
    assertNoMatch nfa "aab"


[<Test>]
let ``Simple Or``() =
    let nfa = rgxToNFA <|  (RGP("A", [Token.``c-printable``]) ||| RGP("B", [Token.``c-printable``]))

    assertFullMatch nfa "A"
    assertFullMatch nfa "B"

    assertNoMatch nfa "C"


[<Test>]
let ``Simple Or with nested concat``() =
    let nfa = rgxToNFA <|  (RGP("AC", [Token.``c-printable``]) ||| RGP("BC", [Token.``c-printable``]))

    assertFullMatch nfa "AC"
    assertFullMatch nfa "BC"

    assertNoMatch nfa "AB"
    assertNoMatch nfa "BD"
    assertNoMatch nfa "C"

[<Test>]
let ``Simple Or with concat before``() =
    let nfa = rgxToNFA <|  RGP("A", [Token.``c-printable``]) + (RGP("C", [Token.``c-printable``]) ||| RGP("B", [Token.``c-printable``]))

    assertFullMatch nfa "AC"
    assertFullMatch nfa "AB"
    
    assertNoMatch nfa "B"
    assertNoMatch nfa "AD"


[<Test>]
let ``Simple Or with concat after``() =
    let nfa = rgxToNFA <|  (RGP("A", [Token.``c-printable``]) ||| RGP("B", [Token.``c-printable``])) + RGP("GH", [Token.``c-printable``])  

    assertFullMatch nfa "AGH"
    assertFullMatch nfa "BGH"
    
    assertNoMatch nfa "C"
    assertNoMatch nfa "BA"


[<Test>]
let ``Complex Or with various nested concats``() =
    let nfa = 
        rgxToNFA <| 
        RGP("XY", [Token.``c-printable``]) + 
        (RGP("A", [Token.``c-printable``]) ||| RGP("B", [Token.``c-printable``])) + 
        RGP("GH", [Token.``c-printable``]) + 
        (RGP("ABD", [Token.``c-printable``]) ||| RGP("ABDAC", [Token.``c-printable``]))

    assertFullMatch nfa "XYAGHABD"
    assertFullMatch nfa "XYBGHABD"
    assertFullMatch nfa "XYBGHABDAC"

    assertNoMatch nfa "XYC"
    assertNoMatch nfa "XYCABE"


[<Test>]
let ``Complex Or with deep nested concats``() =
    let nfa = 
        rgxToNFA <| 
        RGP("XY", [Token.``c-printable``]) + 
        (RGP("AB", [Token.``c-printable``]) ||| (RGP("BA", [Token.``c-printable``]) + 
            (RGP("CX", [Token.``c-printable``]) ||| RGP("DX", [Token.``c-printable``])))) + 
            RGP("GH", [Token.``c-printable``]) + 
            (RGP("ABD", [Token.``c-printable``]) ||| RGP("ABDAC", [Token.``c-printable``]))

    assertFullMatch nfa "XYABGHABD"
    assertFullMatch  nfa "XYBACXGHABDAC"
    assertFullMatch nfa "XYBADXGHABD"

    assertNoMatch nfa "XYC"
    assertNoMatch nfa "XYBADY"


let ``Simple Or with simple overlapping concat``() =
    let nfa = rgxToNFA <|  (RGP("AB", [Token.``c-printable``]) ||| RGP("AC", [Token.``c-printable``]))

    assertFullMatch nfa "AB"
    assertFullMatch nfa "AC"

    assertNoMatch nfa "AD"
    assertNoMatch nfa "B"


let ``Simple Or with nested overlapping concat``() =
    let nfa = rgxToNFA <|  
        (
            RGP("AAB",  [Token.``c-printable``]) 
        ||| RGP("AACA", [Token.``c-printable``]) 
        ||| RGP("AACB", [Token.``c-printable``]) 
        ||| RGP("AABA", [Token.``c-printable``]) 
        ||| RGP("BA",   [Token.``c-printable``])
        ||| RGP("BC",   [Token.``c-printable``])
        ||| RGP("CD",   [Token.``c-printable``])
        )

    assertFullMatch nfa "AAB"
    assertFullMatch nfa "AACA"
    assertFullMatch nfa "AACB"
    assertFullMatch nfa "AABA"
    assertFullMatch nfa "BA"
    assertFullMatch nfa "BC"
    assertFullMatch nfa "CD"

    assertPartialMatch nfa "AABD" "AAB"

    assertNoMatch nfa "AACD"
    assertNoMatch nfa "AAD"
    assertNoMatch nfa "AD"
    assertNoMatch nfa "D"


[<Test>]
let ``Conflicting Plain/OneOf within Or with simple concat``() =
    let nfa = rgxToNFA <|  (
        (RGP("\n", [Token.NewLine]) + RGP("A", [Token.``c-printable``])) ||| 
        (RGO("B\n", [Token.``c-printable``;Token.NewLine]) + RGP("X", [Token.``c-printable``]))
    )

    assertFullMatch nfa "\nA"
    assertFullMatch nfa "\nX"
    assertFullMatch nfa "BX"

    assertNoMatch nfa "?"
    assertNoMatch nfa "\nB"
    assertNoMatch nfa "BA"


[<Test>]
let ``Conflicting Plain/OneOf within Or with canabalizing refactoring``() =
    let nfa = 
        rgxToNFA <| (
            (RGP("\n", [Token.NewLine]) + RGP("A", [Token.``c-printable``])) ||| 
            (RGO("\t\n", [Token.``t-tab``;Token.NewLine]) + RGP("X", [Token.``c-printable``])) |||
            (RGP("\t", [Token.``t-tab``]) + RGP("Y", [Token.``c-printable``]))
        )

    assertFullMatch nfa "\nA"
    assertFullMatch nfa "\tX"
    assertFullMatch nfa "\nX"
    assertFullMatch nfa "\tY"

    assertNoMatch nfa "?"
    assertNoMatch nfa "\nY"
    assertNoMatch nfa "\nY"
    assertNoMatch nfa "BA"


[<Test>]
let ``Conflicting double OneOf within Or``() =
    let nfa = 
        rgxToNFA <| (
            (RGP("\n", [Token.NewLine]) + RGP("A", [Token.``c-printable``])) ||| 
            (RGO("\t\n", [Token.``t-tab``;Token.NewLine]) + RGP("X", [Token.``c-printable``])) |||
            (RGO("\t-", [Token.``t-tab``; Token.``t-hyphen``]) + RGP("Y", [Token.``c-printable``])) 
        )

    assertFullMatch nfa "\nA"
    assertFullMatch nfa "\tX"
    assertFullMatch nfa "\nX"
    assertFullMatch nfa "\tY"
    assertFullMatch nfa "-Y"

    assertNoMatch nfa "?"
    assertNoMatch nfa "\nY"
    assertNoMatch nfa "\nY"
    assertNoMatch nfa "-X"
    assertNoMatch nfa "BA"


[<Test>]
let ``Simple optional at the end``() =
    let nfa = 
        rgxToNFA <| RGP("A", [Token.``c-printable``]) + OPT(RGP("X", [Token.``c-printable``]))
    
    assertFullMatch nfa "AX"
    assertFullMatch nfa "A"

    assertPartialMatch nfa "AY" "A"

    assertNoMatch nfa "B"


[<Test>]
let ``Simple optional at the beginnig``() =
    let nfa = 
        rgxToNFA <| OPT(RGP("X", [Token.``c-printable``])) + RGP("A", [Token.``c-printable``]) 
    
    assertFullMatch nfa "XA"
    assertFullMatch nfa "A"
    assertPartialMatch nfa "AY" "A"

    assertNoMatch nfa "B"
    assertNoMatch  nfa "XB"



[<Test>]
let ``Complex optional with conflicting plain enter-and-exit paths``() =
    let nfa = 
        rgxToNFA <| 
            OPT(RGP("AAC", [Token.``c-printable``])) + 
                RGP("AAB", [Token.``c-printable``]) 
    
    assertFullMatch nfa "AAB"
    assertFullMatch nfa "AACAAB"

    assertNoMatch nfa "AAD"
    assertNoMatch nfa "B"
    assertNoMatch nfa "AACAD"

[<Test>]
let ``Complex optional with conflicting plain enter-and-exit paths to MultiPath``() =
    let nfa = 
        rgxToNFA <| 
            OPT(RGP("AA", [Token.``c-printable``]) + (RGP("B", [Token.``c-printable``]) ||| RGP("C", [Token.``c-printable``]))) + 
               (RGP("AA", [Token.``c-printable``]) + (RGP("D", [Token.``c-printable``]) ||| RGP("E", [Token.``c-printable``])))
    
    assertFullMatch nfa "AABAAD"
    assertFullMatch nfa "AACAAD"
    assertFullMatch nfa "AABAAE"
    assertFullMatch nfa "AACAAE"

    assertFullMatch nfa "AAD"
    assertFullMatch nfa "AAE"

    assertNoMatch nfa "AABAABAAD"
    assertNoMatch nfa "AAF"
    assertNoMatch nfa "AAB"
    assertNoMatch nfa "AABAA"


[<Test>]
let ``Complex optional with conflicting oneinset enter-and-exit paths``() =
    let nfa = 
        rgxToNFA <| 
            OPT(RGO("\t\n", [Token.``t-tab``; Token.NewLine]) + RGP("A", [Token.``c-printable``])) + 
                RGO("\t[0-9]", [Token.``t-tab``; Token.``ns-dec-digit``]) + RGP("B", [Token.``c-printable``]) 

    assertFullMatch nfa "\tA\tB"
    assertFullMatch nfa "\nA\tB"

    assertFullMatch nfa "\tA0B"
    assertFullMatch nfa "\tA9B"
    assertFullMatch nfa "\nA0B"
    assertFullMatch nfa "\nA9B"

    assertFullMatch nfa "\tB"
    assertFullMatch nfa "0B"
    assertFullMatch nfa "9B"

    assertPartialMatch nfa "\tBcc" "\tB"

    assertNoMatch nfa "\tC"
    assertNoMatch nfa "\tA\nA0B"


[<Test>]
let ``Complex optional with conflicting oneinset enter-and-exit paths, splitting in MultiPaths``() =
    let nfa = 
        rgxToNFA <| 
            OPT(RGO("\t\n", [Token.``t-tab``; Token.NewLine]) + (RGP("A", [Token.``c-printable``]) ||| RGP("B", [Token.``c-printable``]))) + 
                RGO("\t[0-9]", [Token.``t-tab``; Token.``ns-dec-digit``]) + (RGP("C", [Token.``c-printable``]) ||| RGP("D", [Token.``c-printable``])) 

    assertFullMatch nfa "\tA\tC"
    assertFullMatch nfa "\tB\tC"
    assertFullMatch nfa "\nA\tC"
    assertFullMatch nfa "\nB\tC"

    assertFullMatch nfa "\tA0C"
    assertFullMatch nfa "\tB1C"
    assertFullMatch nfa "\nA2C"
    assertFullMatch nfa "\nB3C"

    assertFullMatch nfa "\tA\tD"
    assertFullMatch nfa "\tB\tD"
    assertFullMatch nfa "\nA\tD"
    assertFullMatch nfa "\nB\tD"

    assertFullMatch nfa "\tA0D"
    assertFullMatch nfa "\tB1D"
    assertFullMatch nfa "\nA2D"
    assertFullMatch nfa "\nB3D"

    assertFullMatch nfa "\tC"
    assertFullMatch nfa "0C"
    assertFullMatch nfa "1D"
    assertFullMatch nfa "2C"
    assertFullMatch nfa "3D"

    assertPartialMatch nfa "\nB3Czzzz" "\nB3C"

    assertNoMatch nfa "\tA\tB0C"


[<Test>]
let ``Complex optional with conflicting oneinset/plain enter-and-exit paths with plain in exit-path``() =
    let nfa = 
        rgxToNFA <| 
            OPT(RGO("\t\n", [Token.``t-tab``; Token.NewLine]) + RGP("A", [Token.``c-printable``])) + 
                (RGP("\t", [Token.``t-tab``]) + RGP("D", [Token.``c-printable``])) 

    assertFullMatch nfa "\tA\tD"
    assertFullMatch nfa "\nA\tD"
    assertFullMatch nfa "\tD"

    assertPartialMatch nfa "\tA\tD123" "\tA\tD"

    assertNoMatch nfa "\tA\tA\tD"
    assertNoMatch nfa "\nA\tA\tD"
    assertNoMatch nfa "\tA\nA\tD"

[<Test>]
let ``Complex optional with conflicting oneinset/plain enter-and-exit paths with plain in iter-path``() =
    let nfa = 
        rgxToNFA <| 
            OPT(RGP("\t", [Token.``t-tab``]) + RGP("D", [Token.``c-printable``])) + 
               (RGO("\t\n", [Token.``t-tab``; Token.NewLine]) + RGP("A", [Token.``c-printable``])) 

    assertFullMatch nfa "\tD\tA"
    assertFullMatch nfa "\tD\nA"
    assertFullMatch nfa "\tA"
    assertFullMatch nfa "\nA"

    assertPartialMatch nfa "\tD\tA123" "\tD\tA"

    assertNoMatch nfa "\tD\tD\tA"
    assertNoMatch nfa "\tD\tD\nA"
    assertNoMatch nfa "\nD"
    assertNoMatch nfa "\nD\tA"

[<Test>]
let ``Simple zero or more test``() =
    let nfa = 
        rgxToNFA <| 
                ZOM(RGP("CD", [Token.``c-printable``])) + RGP("CE", [Token.``c-printable``]) 

    assertFullMatch nfa "CE"
    assertFullMatch nfa "CDCE"
    assertFullMatch nfa "CDCDCE"

    assertNoMatch  nfa "CD"


[<Test>]
let ``Zero or more with non-distinctive iter/exit repeat-paths``() =
    let nfa = 
        rgxToNFA <| 
                ZOM(RGP("A", [Token.``c-printable``])) + RGP("AB", [Token.``c-printable``]) 

    assertFullMatch nfa "AB"
    assertFullMatch nfa "AAB"
    assertFullMatch nfa "AAAB"

    assertNoMatch nfa "X"
    assertNoMatch nfa "A"

[<Test>]
let ``Simple one or more test``() =
    let nfa = 
        rgxToNFA <| 
                OOM(RGP("CD", [Token.``c-printable``])) + RGP("CE", [Token.``c-printable``]) 

    assertFullMatch nfa "CDCE"
    assertFullMatch nfa "CDCDCE"

    assertNoMatch nfa "CE"
    assertNoMatch nfa "C"
    assertNoMatch nfa "A"

[<Test>]
let ``One or more with non-distinctive iter/exit repeat-paths``() =
    let nfa = 
        rgxToNFA <| 
                OOM(RGP("A", [Token.``c-printable``])) + RGP("AB", [Token.``c-printable``]) 

    assertFullMatch nfa "AAB"
    assertFullMatch nfa "AAAB"
    assertFullMatch nfa "AAAAB"

    assertNoMatch nfa "AB"
    assertNoMatch nfa "A"
    assertNoMatch nfa "X"


[<Test>]
let ``Simple Overlapping plain multipaths in iter/exit repeat-paths``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    RGP("A", [Token.``c-printable``]) |||
                    RGP("B", [Token.``c-printable``]) 
                    ) +
                    (
                    RGP("A", [Token.``c-printable``]) |||
                    RGP("C", [Token.``c-printable``]) 
                    )

    assertFullMatch nfa "AA"
    assertFullMatch nfa "BA"
    assertFullMatch nfa "AC"
    assertFullMatch nfa "BC"

    assertFullMatch nfa "A"
    assertFullMatch nfa "C"

    assertNoMatch nfa "BAC"

[<Test>]
let ``Complex Overlapping plain multipaths in iter/exit repeat-paths``() =
    
    let nfa = 
        rgxToNFA <| 
                OPT(
                    (RGP("A", [Token.``c-printable``]) |||
                     RGP("B", [Token.``c-printable``])) + RGP("E", [Token.``c-printable``])
                    ) +
                    (
                    RGP("A", [Token.``c-printable``]) |||
                    RGP("C", [Token.``c-printable``]) 
                    ) + RGP("F", [Token.``c-printable``])

    assertFullMatch nfa "AEAF"
    assertFullMatch nfa "BEAF"
    assertFullMatch nfa "AECF"
    assertFullMatch nfa "BECF"

    assertFullMatch nfa "AF"
    assertFullMatch nfa "CF"

    assertNoMatch nfa "BEAEAF"
    assertNoMatch nfa "BEAECF"


[<Test>]
let ``Simple Overlapping OneInSet multipaths in iter/exit repeat-paths``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    RGP("A", [Token.``c-printable``]) |||
                    RGO("\t-", [Token.``t-tab``; Token.``t-hyphen``]) 
                    ) +
                    (
                    RGP("B", [Token.``c-printable``]) |||
                    RGO("\t\n", [Token.``t-tab``; Token.NewLine])
                    )

    assertFullMatch nfa "AB"
    assertFullMatch nfa "\tB"
    assertFullMatch nfa "-B"

    assertFullMatch nfa "A\t"
    assertFullMatch nfa "\t\t"
    assertFullMatch nfa "-\t"

    assertFullMatch nfa "A\n"
    assertFullMatch nfa "\t\n"
    assertFullMatch nfa "-\n"


    assertFullMatch nfa "B"
    assertFullMatch nfa "\t"
    assertFullMatch nfa "\n"


    assertNoMatch nfa "AAA"


[<Test>]
let  ``Complex Overlapping OneInSet multipaths in iter/exit repeat-paths``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    (RGP("A", [Token.``c-printable``]) |||
                     RGO("\t-", [Token.``t-tab``; Token.``t-hyphen``]))
                    + RGP("E", [Token.``c-printable``])
                    ) +
                    (
                    RGP("B", [Token.``c-printable``]) |||
                    RGO("\t\n", [Token.``t-tab``; Token.NewLine])
                    ) + RGP("F", [Token.``c-printable``])


    assertFullMatch nfa "AEBF"
    assertFullMatch nfa "\tEBF"
    assertFullMatch nfa "-EBF"

    assertFullMatch nfa "AE\tF"
    assertFullMatch nfa "\tE\tF"
    assertFullMatch nfa "-E\tF"

    assertFullMatch nfa "AE\nF"
    assertFullMatch nfa "\tE\nF"
    assertFullMatch nfa "-E\nF"


    assertFullMatch nfa "BF"
    assertFullMatch nfa "\tF"
    assertFullMatch nfa "\nF"

    assertNoMatch nfa "\tE\tE\tF"


[<Test>]
let ``Complex double refactor overlapping OneInSet,plains multipaths in iter/exit repeat-paths``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    RGP("A", [Token.``c-printable``]) |||
                    RGO("\t-", [Token.``t-tab``; Token.``t-hyphen``]) 
                    ) +
                    (
                    RGP("A", [Token.``c-printable``]) |||
                    RGO("\t\n", [Token.``t-tab``; Token.NewLine])
                    )

    assertFullMatch nfa "AA"
    assertFullMatch nfa "\tA"
    assertFullMatch nfa "-A"

    assertFullMatch nfa "A\t"
    assertFullMatch nfa "\t\t"
    assertFullMatch nfa "-\t"

    assertFullMatch nfa "A\n"
    assertFullMatch nfa "\t\n"
    assertFullMatch nfa "-\n"


    assertFullMatch nfa "A"
    assertFullMatch nfa "\t"
    assertFullMatch nfa "\n"



[<Test>]
let  ``Complex Overlapping I-OneInSet and X-Plain in I/X multipaths repeat-paths``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    (RGP("A", [Token.``c-printable``]) |||
                     RGO("\t-", [Token.``t-tab``; Token.``t-hyphen``]))
                    + RGP("E", [Token.``c-printable``])
                    ) +
                    (
                    RGP("B", [Token.``c-printable``]) |||
                    RGP("\t", [Token.``t-tab``])
                    ) + RGP("F", [Token.``c-printable``])

    assertFullMatch nfa "AEBF"
    assertFullMatch nfa "\tEBF"
    assertFullMatch nfa "-EBF"

    assertFullMatch nfa "AE\tF"
    assertFullMatch nfa "\tE\tF"
    assertFullMatch nfa "-E\tF"

    assertFullMatch nfa "BF"
    assertFullMatch nfa "\tF"

    assertNoMatch nfa "\tE\tE\tF"


[<Test>]
let ``Complex Overlapping I-Plain and X-OneInSet in I/X multipaths repeat-paths``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    (RGP("A", [Token.``c-printable``]) |||
                     RGP("\t", [Token.``t-tab``]))
                    + RGP("E", [Token.``c-printable``])
                    ) +
                    (
                    RGP("B", [Token.``c-printable``]) |||
                    RGO("\t\n", [Token.``t-tab``; Token.NewLine])
                    ) + RGP("F", [Token.``c-printable``])


    assertFullMatch nfa "AEBF"
    assertFullMatch nfa "\tEBF"

    assertFullMatch nfa "AE\tF"
    assertFullMatch nfa "\tE\tF"

    assertFullMatch nfa "AE\nF"
    assertFullMatch nfa "\tE\nF"

    assertFullMatch nfa "BF"
    assertFullMatch nfa "\tF"
    assertFullMatch nfa "\nF"

    assertNoMatch nfa "\tE\tE\tF"
