module Test.Legivel.ThompsonParser

open Legivel.Utilities.RegexDSL
open NUnit.Framework
open FsUnitTyped
open System.Collections.Generic
open Legivel.Tokenizer

let EndOfStream = TokenData.Create (Token.EOF) ""


let expected (str:string) =
    str.ToCharArray() 
    |> List.ofArray 
    |> List.map(fun c -> c.ToString())
    |> List.rev

let stripTokenData (tdl:TokenData list) = tdl |> List.map(fun i -> i.Source)

[<Test>]
let ``Parse Plain Character - sunny day``() =
    let rgxst = RGP("A", [Token.``c-printable``]) |> CreatePushParser
    let yaml = "A"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true 
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Plain String - sunny day``() =
    let rgxst = RGP("ABC", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABC"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    streamReader.Get().Token|> shouldEqual Token.EOF


[<Test>]
let ``Parse Plain String - rainy day``() =
    let rgxst = RGP("ABC", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABD"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual false
    streamReader.Get().Source |> shouldEqual "A" 


[<Test>]
let ``Parse Plain String concats - sunny day``() =
    let rgxst = RGP("AB", [Token.``c-printable``]) + RGP("CD", [Token.``c-printable``]) + RGP("EF", [Token.``c-printable``])|> CreatePushParser

    let yaml = "ABCDEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Plain String Ored - sunny day``() =
    let rgxst = RGP("ABCE", [Token.``c-printable``]) ||| RGP("ABD", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABDE"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    streamReader.Get().Source |> shouldEqual "E"

    let yaml = "ABCE"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst

    mr.IsMatch |> shouldEqual true
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse RGO Character - sunny day``() =
    let rgxst = RGO("A-", [Token.``c-printable``;Token.``t-hyphen``]) |> CreatePushParser

    let yaml = "-"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    streamReader.Get().Token |> shouldEqual Token.EOF

    let yaml = "A"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    ["A"] |> shouldEqual (stripTokenData mr.FullMatch)
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Plain String with optional end - nomatch option, with residu``() =
    let rgxst = RGP("ABC", [Token.``c-printable``])  + OPT(RGP("E", [Token.``c-printable``])) |> CreatePushParser

    let yaml = "ABCD"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABC")
    streamReader.Get().Source |> shouldEqual "D"


[<Test>]
let ``Parse Plain String with optional end - match option, with residu``() =
    let rgxst = RGP("ABC", [Token.``c-printable``])  + OPT(RGP("E", [Token.``c-printable``])) |> CreatePushParser
    let yaml = "ABCE"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABCE")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Plain String with optional middle - nomatch option``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + OPT(RGP("CDF", [Token.``c-printable``])) + RGP("CDEF", [Token.``c-printable``])  |> CreatePushParser

    let yaml = "ABCDEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABCDEF")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Plain String with optional middle - match option``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + OPT(RGP("CDF", [Token.``c-printable``])) + RGP("CDEF", [Token.``c-printable``])  |> CreatePushParser
    let yaml = "ABCDFCDEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABCDFCDEF")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Zero Or More in the middle - zero match``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + ZOM(RGP("E", [Token.``c-printable``])) + RGP("CD", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABCD"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABCD")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Zero Or More in the middle - zero match with residu``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + ZOM(RGP("E", [Token.``c-printable``])) + RGP("CD", [Token.``c-printable``]) |> CreatePushParser
    let yaml = "ABCDE"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABCD")
    streamReader.Get().Source |> shouldEqual "E"


[<Test>]
let ``Parse Zero Or More in the middle - one match``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + ZOM(RGP("E", [Token.``c-printable``])) + RGP("CD", [Token.``c-printable``]) |> CreatePushParser
    let yaml = "ABECD"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABECD")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Zero Or More in the middle - two match``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + ZOM(RGP("E", [Token.``c-printable``])) + RGP("CD", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABEECD"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEECD")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Zero Or More in the middle - three match``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + ZOM(RGP("E", [Token.``c-printable``])) + RGP("CD", [Token.``c-printable``]) |> CreatePushParser
    let yaml = "ABEEECD"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEEECD")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Zero Or More at the end - nomatch``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + ZOM(RGP("E", [Token.``c-printable``])) |> CreatePushParser

    let yaml = "AB"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "AB")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Zero Or More at the end - nomatch with residu``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + ZOM(RGP("E", [Token.``c-printable``])) |> CreatePushParser
    let yaml = "ABC"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "AB")
    streamReader.Get().Source |> shouldEqual "C"


[<Test>]
let ``Parse Zero Or More at the end - one match with residu``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + ZOM(RGP("E", [Token.``c-printable``])) |> CreatePushParser
    let yaml = "ABEC"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABE")
    streamReader.Get().Source |> shouldEqual "C"


[<Test>]
let ``Parse Zero Or More at the end - two match with residu``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + ZOM(RGP("E", [Token.``c-printable``])) |> CreatePushParser
    let yaml = "ABEEC"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEE")
    streamReader.Get().Source |> shouldEqual "C"


[<Test>]
let ``Parse Zero Or More at the end - two match``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + ZOM(RGP("E", [Token.``c-printable``])) |> CreatePushParser
    let yaml = "ABEE"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEE")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse One Or More in the middle - rainy day``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + OOM(RGP("E", [Token.``c-printable``])) + RGP("CD", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABCD"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual false
    streamReader.Get().Source |> shouldEqual "A"


[<Test>]
let ``Parse One Or More in the middle - one match with residu``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + OOM(RGP("E", [Token.``c-printable``])) + RGP("CD", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABECDF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABECD")
    streamReader.Get().Source |> shouldEqual "F"


[<Test>]
let ``Parse One Or More in the middle - two match``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + OOM(RGP("E", [Token.``c-printable``])) + RGP("CD", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABEECD"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEECD")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse One Or More in the middle, with digraph - one match``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + OOM(RGP("EF", [Token.``c-printable``])) + RGP("ED", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABEFED"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEFED")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse One Or More in the middle, with digraph - nomatch``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + OOM(RGP("EF", [Token.``c-printable``])) + RGP("ED", [Token.``c-printable``]) |> CreatePushParser
    let yaml = "ABED"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual false
    streamReader.Get().Source |> shouldEqual "A"


[<Test>]
let ``Parse Range at the end - match to minimum``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + Range(RGP("E", [Token.``c-printable``]), 2,3) |> CreatePushParser

    let yaml = "ABEE"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEE")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Range at the end - match to maximum``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + Range(RGP("E", [Token.``c-printable``]), 2,3) |> CreatePushParser
    let yaml = "ABEEE"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEEE")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Range at the end - match to maximum with residu``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + Range(RGP("E", [Token.``c-printable``]), 2,3) |> CreatePushParser
    let yaml = "ABEEEE"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEEE")
    streamReader.Get().Source |> shouldEqual "E"


[<Test>]
let ``Parse Range in the middle - match to minimum``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + Range(RGP("ED", [Token.``c-printable``]), 2,4) + RGP("EF", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABEDEDEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEDEDEF")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Range in the middle - match to middle``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + Range(RGP("ED", [Token.``c-printable``]), 2,4) + RGP("EF", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABEDEDEDEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEDEDEDEF")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Range in the middle - match to max``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + Range(RGP("ED", [Token.``c-printable``]), 2,4) + RGP("EF", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABEDEDEDEDEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEDEDEDEDEF")
    streamReader.Get().Token |> shouldEqual Token.EOF

[<Test>]
let ``Parse Group in Concat - middle match in group``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + GRP(RGP("CD", [Token.``c-printable``])) + RGP("EF", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABCDEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABCDEF")
    mr.GroupsResults.Length |> shouldEqual 1
    mr.GroupsResults.Head.Match |> stripTokenData |> shouldEqual (expected "CD")
    streamReader.Get().Token |> shouldEqual Token.EOF

    
[<Test>]
let ``Parse Group in Concat - end match in group``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + RGP("EF", [Token.``c-printable``]) + GRP(RGP("CD", [Token.``c-printable``]))  |> CreatePushParser

    let yaml = "ABEFCD"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEFCD")
    mr.GroupsResults.Length |> shouldEqual 1
    mr.GroupsResults.Head.Match |> stripTokenData |> shouldEqual (expected "CD")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Group in Or - match in group``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + (RGP("AB", [Token.``c-printable``]) ||| GRP(RGP("CD", [Token.``c-printable``])) ||| RGP("HK", [Token.``c-printable``]) )+ RGP("EF", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABCDEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABCDEF")
    mr.GroupsResults.Length |> shouldEqual 1
    mr.GroupsResults.Head.Match |> stripTokenData |> shouldEqual (expected "CD")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Group in Option - middle match in group``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + (OPT(GRP(RGP("CD", [Token.``c-printable``]))))+ RGP("EF", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABCDEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABCDEF")
    mr.GroupsResults.Length |> shouldEqual 1
    mr.GroupsResults.Head.Match |> stripTokenData |> shouldEqual (expected "CD")
    streamReader.Get().Token |> shouldEqual Token.EOF

[<Test>]
let ``Parse Group in Option - end match in group``() =
    let rgxst = RGP("AB", [Token.``c-printable``]) + RGP("EF", [Token.``c-printable``]) + (OPT(GRP(RGP("CD", [Token.``c-printable``])))) |> CreatePushParser

    let yaml = "ABEFCD"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEFCD")
    mr.GroupsResults.Length |> shouldEqual 1
    mr.GroupsResults.Head.Match |> stripTokenData |> shouldEqual (expected "CD")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Group in Option - middle nomatch in group``() =
    let rgxst = RGP("AB", [Token.``c-printable``])  + (OPT(GRP(RGP("CD", [Token.``c-printable``]))))+ RGP("EF", [Token.``c-printable``]) |> CreatePushParser

    let yaml = "ABEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEF")
    mr.GroupsResults.Length |> shouldEqual 1
    mr.GroupsResults.Head.Match |> stripTokenData |> shouldEqual (expected "")
    streamReader.Get().Token |> shouldEqual Token.EOF


[<Test>]
let ``Parse Group in Option - end nomatch in group``() =
    let rgxst = RGP("AB", [Token.``c-printable``]) + RGP("EF", [Token.``c-printable``]) + (OPT(GRP(RGP("CD", [Token.``c-printable``])))) |> CreatePushParser

    let yaml = "ABEF"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData |> shouldEqual (expected "ABEF")
    mr.GroupsResults.Length |> shouldEqual 1
    mr.GroupsResults.Head.Match |> stripTokenData |> shouldEqual (expected "")
    streamReader.Get().Token |> shouldEqual Token.EOF

