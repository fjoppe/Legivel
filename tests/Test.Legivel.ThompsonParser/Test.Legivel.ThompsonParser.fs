module Test.Legivel.ThompsonParser

open Legivel.Tokenizer
open System.Drawing
open System.Diagnostics
open NUnit.Framework
open FsUnitTyped
open TestUtils
open System.Text.RegularExpressions
open Legivel.Utilities.RegexDSL
open Legivel.ThompsonParser


[<Test>]
let ``Simple Concat - match string``() =
    let nfa = rgxToNFA <| RGP("A", [Token.``c-printable``]) + RGP("A", [Token.``c-printable``]) + RGP("B", [Token.``c-printable``])
    let r = parseIt nfa "AAB"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AAB"

    let r = parseIt nfa "aab"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual  []


[<Test>]
let ``Simple Or - match string``() =
    let nfa = rgxToNFA <|  (RGP("A", [Token.``c-printable``]) ||| RGP("B", [Token.``c-printable``]))
    let r = parseIt nfa "A"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "A"

    let r = parseIt nfa "B"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "B"

    let r = parseIt nfa "C"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual  []


[<Test>]
let ``Simple Or with nested concat - match string``() =
    let nfa = rgxToNFA <|  (RGP("AC", [Token.``c-printable``]) ||| RGP("BC", [Token.``c-printable``]))
    let r = parseIt nfa "AC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AC"

    let r = parseIt nfa "BC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "BC"

    let r = parseIt nfa "AB"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "BD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "C"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

[<Test>]
let ``Simple Or with concat before - match string``() =
    let nfa = rgxToNFA <|  RGP("A", [Token.``c-printable``]) + (RGP("C", [Token.``c-printable``]) ||| RGP("B", [Token.``c-printable``]))
    let r = parseIt nfa "AC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AC"

    let r = parseIt nfa "AB"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AB"
    
    let r = parseIt nfa "B"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "AD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


[<Test>]
let ``Simple Or with concat after - match string``() =
    let nfa = rgxToNFA <|  (RGP("A", [Token.``c-printable``]) ||| RGP("B", [Token.``c-printable``])) + RGP("GH", [Token.``c-printable``])  
    let r = parseIt nfa "AGH"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AGH"

    let r = parseIt nfa "BGH"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "BGH"
    
    let r = parseIt nfa "C"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "BA"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


[<Test>]
let ``Complex Or with various nested concats - match string``() =
    let nfa = 
        rgxToNFA <| 
        RGP("XY", [Token.``c-printable``]) + 
        (RGP("A", [Token.``c-printable``]) ||| RGP("B", [Token.``c-printable``])) + 
        RGP("GH", [Token.``c-printable``]) + 
        (RGP("ABD", [Token.``c-printable``]) ||| RGP("ABDAC", [Token.``c-printable``]))

    let r = parseIt nfa "XYAGHABD"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "XYAGHABD"

    let r = parseIt nfa "XYBGHABD"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "XYBGHABD"
    
    let r = parseIt nfa "XYBGHABDAC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "XYBGHABDAC"

    let r = parseIt nfa "XYC"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "XYCABE"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


[<Test>]
let ``Complex Or with deep nested concats - match string``() =
    let nfa = 
        rgxToNFA <| 
        RGP("XY", [Token.``c-printable``]) + 
        (RGP("AB", [Token.``c-printable``]) ||| (RGP("BA", [Token.``c-printable``]) + 
            (RGP("CX", [Token.``c-printable``]) ||| RGP("DX", [Token.``c-printable``])))) + 
            RGP("GH", [Token.``c-printable``]) + 
            (RGP("ABD", [Token.``c-printable``]) ||| RGP("ABDAC", [Token.``c-printable``]))

    let r = parseIt nfa "XYABGHABD"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "XYABGHABD"

    let r = parseIt nfa "XYBACXGHABDAC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "XYBACXGHABDAC"
    
    let r = parseIt nfa "XYBADXGHABD"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "XYBADXGHABD"

    let r = parseIt nfa "XYC"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "XYBADY"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


let ``Simple Or with simple overlapping concat - match string``() =
    let nfa = rgxToNFA <|  (RGP("AB", [Token.``c-printable``]) ||| RGP("AC", [Token.``c-printable``]))
    let r = parseIt nfa "AB"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AB"

    let r = parseIt nfa "AC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AC"

    let r = parseIt nfa "AD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "B"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


let ``Simple Or with nested overlapping concat - match string``() =
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
    let r = parseIt nfa "AAB"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AAB"

    let r = parseIt nfa "AACA"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AACA"

    let r = parseIt nfa "AACB"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AACB"

    let r = parseIt nfa "AABA"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AABA"

    let r = parseIt nfa "AABD"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AAB"

    let r = parseIt nfa "BA"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "BA"

    let r = parseIt nfa "BC"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "BC"

    let r = parseIt nfa "CD"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "CD"

    let r = parseIt nfa "AACD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "AAD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "AD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "D"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


[<Test>]
let ``Conflicting Plain/OneOf within Or with simple concat - match string``() =
    let nfa = rgxToNFA <|  (
        (RGP("\n", [Token.NewLine]) + RGP("A", [Token.``c-printable``])) ||| 
        (RGO("B\n", [Token.``c-printable``;Token.NewLine]) + RGP("X", [Token.``c-printable``]))
    )

    let r = parseIt nfa "\nA"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "\nA"

    let r = parseIt nfa "\nX"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "\nX"

    let r = parseIt nfa "BX"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "BX"

    let r = parseIt nfa "?"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "\nB"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "BA"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


[<Test>]
let ``Conflicting Plain/OneOf within Or with canabalizing refactoring - match string``() =
    let nfa = 
        rgxToNFA <| (
            (RGP("\n", [Token.NewLine]) + RGP("A", [Token.``c-printable``])) ||| 
            (RGO("\t\n", [Token.``t-tab``;Token.NewLine]) + RGP("X", [Token.``c-printable``])) |||
            (RGP("\t", [Token.``t-tab``]) + RGP("Y", [Token.``c-printable``]))
        )

    let r = parseIt nfa "\nA"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "\nA"

    let r = parseIt nfa "\tX"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "\tX"

    let r = parseIt nfa "\nX"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "\nX"


    let r = parseIt nfa "\tY"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "\tY"


    let r = parseIt nfa "?"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "\nY"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "\nY"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "BA"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []



[<Test>]
let ``Conflicting double OneOf within Or - match string``() =
    let nfa = 
        rgxToNFA <| (
            (RGP("\n", [Token.NewLine]) + RGP("A", [Token.``c-printable``])) ||| 
            (RGO("\t\n", [Token.``t-tab``;Token.NewLine]) + RGP("X", [Token.``c-printable``])) |||
            (RGO("\t-", [Token.``t-tab``; Token.``t-hyphen``]) + RGP("Y", [Token.``c-printable``])) 
        )

    let r = parseIt nfa "\nA"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "\nA"

    let r = parseIt nfa "\tX"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "\tX"

    let r = parseIt nfa "\nX"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "\nX"


    let r = parseIt nfa "\tY"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "\tY"

    let r = parseIt nfa "-Y"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "-Y"

    let r = parseIt nfa "?"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "\nY"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "\nY"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "-X"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "BA"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

[<Test>]
let ``Simple optional at the end - match string``() =
    let nfa = 
        rgxToNFA <| RGP("A", [Token.``c-printable``]) + OPT(RGP("X", [Token.``c-printable``]))
    
    let r = parseIt nfa "AX"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AX"

    let r = parseIt nfa "A"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "A"

    let r = parseIt nfa "AY"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "A"

    let r = parseIt nfa "B"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


[<Test>]
let ``Simple optional at the beginnig - match string``() =
    let nfa = 
        rgxToNFA <| OPT(RGP("X", [Token.``c-printable``])) + RGP("A", [Token.``c-printable``]) 
    
    let r = parseIt nfa "XA"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "XA"

    let r = parseIt nfa "A"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "A"

    let r = parseIt nfa "AY"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "A"

    let r = parseIt nfa "B"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "XB"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []



[<Test>]
let ``Complex optional with conflicting enter-and-exit paths - match string``() =
    let nfa = 
        rgxToNFA <| 
            OPT(RGP("AAC", [Token.``c-printable``])) + 
                RGP("AAB", [Token.``c-printable``]) 
    
    let r = parseIt nfa "AAB"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AAB"

    let r = parseIt nfa "AACAAB"
    r |> ParseResult.IsMatch   |> shouldEqual true
    r |> ParseResult.FullMatch |> clts |> shouldEqual "AACAAB"

    let r = parseIt nfa "AAD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "B"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []

    let r = parseIt nfa "AACAD"
    r |> ParseResult.IsMatch   |> shouldEqual false
    r |> ParseResult.FullMatch |> shouldEqual []


