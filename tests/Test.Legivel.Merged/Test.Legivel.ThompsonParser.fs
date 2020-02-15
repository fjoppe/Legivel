module Test.Legivel.ThompsonParser

open Legivel.Tokenizer
open NUnit.Framework
open FsUnitTyped
open Legivel.Utilities.RegexDSL



let ``start-of-line`` = RGP ("^", [Token.NoToken])
let ``end-of-file`` = RGP ("\\z", [Token.NoToken])

let clts (cl:char list) = System.String.Concat(cl)

let assertFullMatch nfa yaml =
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "\x00")
    let r = parseIt nfa stream
    r.IsMatch   |> shouldEqual true
    r.FullMatch |> clts |> shouldEqual yaml


let assertGroupMatch nfa yaml gn mt =
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "\x00")
    let r = parseIt nfa stream
    r.IsMatch|> shouldEqual true
    r.Groups |> List.item gn |> clts |> shouldEqual mt


let assertPartialMatch nfa yaml strmatched =
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "\x00")
    let r = parseIt nfa stream
    r.IsMatch |> shouldEqual true
    r.FullMatch |> clts |> shouldEqual strmatched

    let rest = yaml.Substring(strmatched.Length)
    let c = stream.Get()
    c.Source.[0] |> shouldEqual rest.[0]

let assertNoMatch nfa yaml =
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "\x00")
    let r = parseIt nfa stream
    r.IsMatch |> shouldEqual false
    r.FullMatch |> shouldEqual []
 
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
let ``Colliding Plain/OneOf within Or with simple concat``() =
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
let ``Colliding Plain/OneOf within Or with canabalizing refactoring``() =
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
let ``Colliding double OneOf within Or``() =
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
let ``Complex optional with Colliding plain enter-and-exit paths``() =
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
let ``Complex optional with Colliding plain enter-and-exit paths to MultiPath``() =
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
let ``Complex optional with Colliding oneinset enter-and-exit paths``() =
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
let ``Complex optional with Colliding oneinset enter-and-exit paths, splitting in MultiPaths``() =
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
let ``Complex optional with Colliding oneinset/plain enter-and-exit paths with plain in exit-path``() =
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
let ``Complex optional with Colliding oneinset/plain enter-and-exit paths with plain in iter-path``() =
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

    //assertFullMatch nfa "CE"
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

    assertPartialMatch nfa "BAC" "BA"

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

    assertNoMatch nfa "AE\tEBF"
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

[<Test>]
let ``Simple non-colliding nested Repeater paths``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    OPT(RGP("A", [Token.``c-printable``]))+ RGP("B", [Token.``c-printable``])
                ) +
                RGP("C", [Token.``c-printable``]) 
    
    assertFullMatch nfa "ABC"
    assertFullMatch nfa "BC"
    assertFullMatch nfa "C"

    assertNoMatch nfa "AAC"
    assertNoMatch nfa "AABC"
    assertNoMatch nfa "ABBC"
    assertNoMatch nfa "BAC"
    assertNoMatch nfa "BBC"


[<Test>]
let ``Colliding plains in nested Repeater I-path with one state deep``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    OPT(RGP("A", [Token.``c-printable``]))+ RGP("B", [Token.``c-printable``])
                ) +
                RGP("A", [Token.``c-printable``]) 
    
    assertFullMatch nfa "ABA"
    assertFullMatch nfa "BA"
    assertFullMatch nfa "A"

    assertPartialMatch nfa "ABABA" "ABA"
    assertPartialMatch nfa "BABA" "BA"


[<Test>]
let ``Colliding plains in nested Repeater I-path with two states deep``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    OPT(RGP("AC", [Token.``c-printable``]))+ RGP("B", [Token.``c-printable``])
                ) +
                RGP("AD", [Token.``c-printable``])
    
    assertFullMatch nfa "ACBAD"
    assertFullMatch nfa "BAD"
    assertFullMatch nfa "AD"

    assertNoMatch nfa "ACBABAD" 
    assertNoMatch nfa "BACBAD"


[<Test>]
let ``Colliding OiS in nested Repeater I-path with one state deep``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    OPT(RGO("\t\n", [Token.``t-tab``; Token.NewLine]))+ RGP("B", [Token.``c-printable``])
                ) +
                RGO("-\t", [Token.``t-hyphen``; Token.``t-tab``]) 
    
    assertFullMatch nfa "\tB\t"
    assertFullMatch nfa "\nB\t"

    assertFullMatch nfa "\tB-"
    assertFullMatch nfa "\nB-"

    assertFullMatch nfa "-"
    assertFullMatch nfa "\t"

    assertPartialMatch nfa "\tB\tB-" "\tB\t"
    assertPartialMatch nfa "\nB\tB-" "\nB\t"


[<Test>]
let ``Colliding OiS in nested Repeater I-paths with two states deep``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    OPT(RGO("\t\n", [Token.``t-tab``; Token.NewLine]) + RGP("A", [Token.``c-printable``]))+ RGP("B", [Token.``c-printable``])
                ) +
                RGO("-\t", [Token.``t-hyphen``; Token.``t-tab``]) + RGP("D", [Token.``c-printable``])
    
    assertFullMatch nfa "\tAB\tD"
    assertFullMatch nfa "\nAB\tD"

    assertFullMatch nfa "B-D"
    assertFullMatch nfa "B\tD"

    assertFullMatch nfa "-D"
    assertFullMatch nfa "\tD"


[<Test>]
let ``Colliding Plain/OiS in nested Repeater I-paths with one state deep``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    OPT(RGP("\t", [Token.``t-tab``]))+ RGP("B", [Token.``c-printable``])
                ) +
                RGO("-\t", [Token.``t-hyphen``; Token.``t-tab``]) 
    
    assertFullMatch nfa "\tB\t"

    assertFullMatch nfa "\tB-"

    assertFullMatch nfa "-"
    assertFullMatch nfa "\t"

    assertPartialMatch nfa "\tB\tB-" "\tB\t"


[<Test>]
let ``Colliding Plain/OiS in nested Repeater I-path with two states deep``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    OPT(RGO("\t\n", [Token.``t-tab``; Token.NewLine]) + RGP("A", [Token.``c-printable``]))+ RGP("B", [Token.``c-printable``])
                ) +
                RGP("\t", [Token.``t-tab``]) + RGP("D", [Token.``c-printable``])
    
    assertFullMatch nfa "\tAB\tD"
    assertFullMatch nfa "\nAB\tD"
    assertFullMatch nfa "B\tD"
    assertFullMatch nfa "\tD"



[<Test>]
let ``Colliding plains in nested Repeater X-path with one state deep``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    OPT(RGP("B", [Token.``c-printable``]))+ RGP("A", [Token.``c-printable``])
                ) +
                RGP("A", [Token.``c-printable``]) 
    
    assertFullMatch nfa "BAA"
    assertFullMatch nfa "AA"
    assertFullMatch nfa "A"

    assertNoMatch nfa "BABAA"
    assertPartialMatch nfa "ABAA" "A"


[<Test>]
let ``Colliding plains in nested Repeater X-path with two states deep``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    OPT(RGP("B", [Token.``c-printable``]))+ RGP("AC", [Token.``c-printable``])
                ) +
                RGP("AD", [Token.``c-printable``])
    
    assertFullMatch nfa "BACAD"
    assertFullMatch nfa "ACAD"
    assertFullMatch nfa "AD"

    assertNoMatch nfa "BACACAD" 
    assertNoMatch nfa "BACBACAD"


[<Test>]
let ``Colliding Plain/OiS in nested Repeater X-paths with one state deep``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    OPT(RGP("B", [Token.``c-printable``]))+ RGP("\t", [Token.``t-tab``])
                ) +
                RGO("-\t", [Token.``t-hyphen``; Token.``t-tab``]) 
    
    assertFullMatch nfa "B\t\t"
    assertFullMatch nfa "B\t-"


    assertFullMatch nfa "\t\t"
    assertFullMatch nfa "\t-"

    assertFullMatch nfa "-"
    assertFullMatch nfa "\t"

    assertNoMatch nfa "B\tB\t-"


[<Test>]
let ``Colliding Plain/OiS in nested Repeater X-path with two states deep``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    OPT(RGP("B", [Token.``c-printable``]) + RGP("A", [Token.``c-printable``]))+ RGO("\t\n", [Token.``t-tab``; Token.NewLine])
                ) +
                RGP("\t", [Token.``t-tab``]) + RGP("D", [Token.``c-printable``])
    
    assertFullMatch nfa "BA\t\tD"
    assertFullMatch nfa "BA\n\tD"
    assertFullMatch nfa "\t\tD"
    assertFullMatch nfa "\n\tD"

    assertFullMatch nfa "\tD"
    assertFullMatch nfa "\tD"



[<Test>]
let ``Colliding plains in main path with one state deep``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    RGP("A", [Token.``c-printable``]) +
                    OPT(RGP("B", [Token.``c-printable``]))+ RGP("C", [Token.``c-printable``])
                ) +
                RGP("AD", [Token.``c-printable``]) 
    
    assertFullMatch nfa "ABCAD"
    assertFullMatch nfa "ACAD"
    assertFullMatch nfa "AD"



[<Test>]
let ``Colliding plains in main path with two state deep``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    RGP("AE", [Token.``c-printable``]) +
                    OPT(RGP("B", [Token.``c-printable``]))+ RGP("C", [Token.``c-printable``])
                ) +
                RGP("AED", [Token.``c-printable``]) 
    
    assertFullMatch nfa "AEBCAED"
    assertFullMatch nfa "AECAED"
    assertFullMatch nfa "AED"


[<Test>]
let ``Colliding plains in main path into the I-path with one state deep``() =
    let nfa = 
        rgxToNFA <| 
                OPT(
                    RGP("A", [Token.``c-printable``]) +
                    OPT(RGP("B", [Token.``c-printable``]))+ RGP("C", [Token.``c-printable``])
                ) +
                RGP("ABD", [Token.``c-printable``]) 
    
    assertFullMatch nfa "ABCABD"
    assertFullMatch nfa "ACABD"
    assertFullMatch nfa "ABD"


[<Test>]
let ``Partial Repeat match After Fullmatch``() =
    let nfa = 
        rgxToNFA <| 
                ZOM(RGP("ABC", [Token.``c-printable``]))
    
    assertPartialMatch nfa "ABCABCABD" "ABCABC"
    assertPartialMatch nfa "ABCAB" "ABC"



[<Test>]
let ``Simple group test``() =
    let nfa = 
        rgxToNFA <| 
                RGP("AB", [Token.``c-printable``]) + GRP(RGP("CD", [Token.``c-printable``])) + RGP("AB", [Token.``c-printable``])
    
    assertFullMatch nfa "ABCDAB" 
    assertGroupMatch nfa "ABCDAB" 0 "CD"


[<Test>]
let ``Group with nested Repeat test``() =
    let nfa = 
        rgxToNFA <| 
                RGP("AB", [Token.``c-printable``]) + GRP(ZOM(RGP("CD", [Token.``c-printable``]))) + RGP("AB", [Token.``c-printable``])
    
    assertFullMatch nfa "ABAB" 
    assertFullMatch nfa "ABCDAB" 

    assertGroupMatch nfa "ABCDAB" 0 "CD"
    assertGroupMatch nfa "ABCDCDAB" 0 "CDCD"


[<Test>]
let ``Two groups with nested Repeat test``() =
    let nfa = 
        rgxToNFA <| 
                RGP( "AB", [Token.``c-printable``]) + 
                GRP(ZOM(RGP("CD", [Token.``c-printable``]))) + 
                RGP("AB", [Token.``c-printable``]) + 
                GRP(ZOM(RGP("EF", [Token.``c-printable``]))) + 
                RGP("AB", [Token.``c-printable``])
    
    assertFullMatch nfa "ABABAB" 
    assertFullMatch nfa "ABCDABAB" 
    assertFullMatch nfa "ABABEFAB" 

    assertGroupMatch nfa "ABCDABAB" 0 "CD"
    assertGroupMatch nfa "ABCDCDABAB" 0 "CDCD"

    assertGroupMatch nfa "ABCDABEFAB" 1"EF"
    assertGroupMatch nfa "ABCDCDABEFEFAB" 1 "EFEF"


[<Test>]
let ``Start of Line optional match``() =
    let nfa = 
        rgxToNFA <|
                (RGO("-\n", [Token.``t-hyphen``; Token.NewLine])) +
                (RGP("AB", [Token.``c-printable``]) ||| 
                    ``start-of-line`` + RGP("CD", [Token.``c-printable``])) +
                RGP("EF", [Token.``c-printable``])

    assertFullMatch nfa "-ABEF"
    assertFullMatch nfa "\nABEF"
    assertFullMatch nfa "\nCDEF"

    assertNoMatch nfa "-CDEF"



[<Test>]
let ``Something with hardvalues``() =
    let nfa = 
        rgxToNFA <| HardValues.``c-ns-local-tag-prefix``
    
    assertFullMatch nfa "!%12"    


[<Test>]
let ``Something else with hardvalues``() =
    let nfa = 
        rgxToNFA <| HardValues.``c-single-quote`` + GRP(HardValues.``nb-single-one-line``) + HardValues.``c-single-quote``

    assertGroupMatch nfa "''" 0 ""



