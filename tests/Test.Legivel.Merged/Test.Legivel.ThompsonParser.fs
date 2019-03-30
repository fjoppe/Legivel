module Test.Legivel.ThompsonParser

open Legivel.Utilities.RegexDSL
open NUnit.Framework
open FsUnitTyped
open System.Collections.Generic
open Legivel.Tokenizer
open System.Text

let EndOfStream = TokenData.Create (Token.EOF) ""


let expected (str:string) =
    str.ToCharArray() 
    |> List.ofArray 
    |> List.map(fun c -> c.ToString())
    //|> List.rev

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



//  Complex regex parse test - these caused issues during dev

let ``c-printable`` = 
        RGO (
            "\u0009\u000a\u000d\u0020-\u007e" +   // 8 - bit, #x9 | #xA | #xD | [#x20-#x7E]
            "\u0085\u00a0-\ud7ff\ue000-\ufffd",   // 16- bit, #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD]
                                                    //  32-bit -> currently not supported because .Net does not encode naturally. Yaml: [#x10000-#x10FFFF]
            [
            Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``c-printable``; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark`` 
            Token.``t-colon`` ; Token.``t-comma``; Token.``t-dot`` ; Token.``t-square-bracket-start`` ; Token.``t-square-bracket-end`` ; Token.``t-curly-bracket-start``
            Token.``t-curly-bracket-end`` ; Token.``t-hash`` ; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``
            Token.``t-gt``; Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``;Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``
            Token.``ns-dec-digit``; Token.``c-escape``
            ])

let ``s-space`` : string = "\u0020"
let ``s-tab`` = "\u0009"  
let ``nb-char``  = ``c-printable`` - RGO("\u000a\u000d", [Token.NewLine]) 
let ``s-white`` = RGO(``s-space`` + ``s-tab``, [Token.``t-space``; Token.``t-tab``])
let ``ns-char`` = ``nb-char`` - ``s-white``
let ``start-of-line`` = RGP ("^", [Token.NoToken])
let ``b-line-feed`` = RGP ("\u000a", [Token.NewLine])
let ``b-carriage-return`` = RGP ("\u000d", [Token.NewLine])
let ``b-break`` = 
        (``b-carriage-return`` + ``b-line-feed``) |||  //  DOS, Windows
        ``b-carriage-return``                          |||  //  MacOS upto 9.x
        ``b-line-feed``                                     //  UNIX, MacOS X
let ``b-non-content`` = ``b-break``
let ``b-comment`` = ``b-non-content`` ||| RGP("\\z", [Token.EOF]) // EOF..
let ``s-separate-in-line`` = OOM(``s-white``) ||| ``start-of-line``
let ``c-nb-comment-text`` = RGP("#", [Token.``t-hash``]) + ZOM(``nb-char``)
let ``s-b-comment`` = OPT(``s-separate-in-line`` + OPT(``c-nb-comment-text``)) + ``b-comment`` 
let ``l-comment`` = ``s-separate-in-line`` + OPT(``c-nb-comment-text``) + ``b-comment``
let ``s-l-comments`` = (``s-b-comment`` ||| ``start-of-line``) + ZOM(``l-comment``)
let ``s-indent(n)`` ps = Repeat(RGP (``s-space``, [Token.``t-space``]), ps)
let ``s-flow-line-prefix`` ps = (``s-indent(n)`` ps) + OPT(``s-separate-in-line``)


[<Test>]
let ``Parse Group and Once or More - should match``() =
    //let ``c-printable`` = 
    //        RGO (
    //            "\u0009\u000a\u000d\u0020-\u007e" +   // 8 - bit, #x9 | #xA | #xD | [#x20-#x7E]
    //            "\u0085\u00a0-\ud7ff\ue000-\ufffd",   // 16- bit, #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD]
    //                                                   //  32-bit -> currently not supported because .Net does not encode naturally. Yaml: [#x10000-#x10FFFF]
    //            [
    //            Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``c-printable``; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark`` 
    //            Token.``t-colon`` ; Token.``t-comma``; Token.``t-dot`` ; Token.``t-square-bracket-start`` ; Token.``t-square-bracket-end`` ; Token.``t-curly-bracket-start``
    //            Token.``t-curly-bracket-end`` ; Token.``t-hash`` ; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``
    //            Token.``t-gt``; Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``;Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``
    //            Token.``ns-dec-digit``; Token.``c-escape``
    //            ])

    //let ``s-space`` : string = "\u0020"
    //let ``s-tab`` = "\u0009"  
    //let ``nb-char``  = ``c-printable`` - RGO("\u000a\u000d", [Token.NewLine]) 
    //let ``s-white`` = RGO(``s-space`` + ``s-tab``, [Token.``t-space``; Token.``t-tab``])
    //let ``ns-char`` = ``nb-char`` - ``s-white``
    let icp = GRP(ZOM(RGP (``s-space``, [Token.``t-space``]))) + OOM(``ns-char``)
    let rgxst = icp |> CreatePushParser

    let yaml = "- value"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData  |> shouldEqual (expected "-")
    mr.GroupsResults |> List.length |> shouldEqual 1
    mr.GroupsResults.Head.Match |> stripTokenData |> shouldEqual (expected "")

    let yaml = " value"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData  |> shouldEqual (expected " value")


[<Test>]
let ``Parse s-separate - should match``() =
    //let ``start-of-line`` = RGP ("^", [Token.NoToken])
    //let ``c-printable`` = 
    //        RGO (
    //            "\u0009\u000a\u000d\u0020-\u007e" +   // 8 - bit, #x9 | #xA | #xD | [#x20-#x7E]
    //            "\u0085\u00a0-\ud7ff\ue000-\ufffd",   // 16- bit, #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD]
    //                                                   //  32-bit -> currently not supported because .Net does not encode naturally. Yaml: [#x10000-#x10FFFF]
    //            [
    //            Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``c-printable``; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark`` 
    //            Token.``t-colon`` ; Token.``t-comma``; Token.``t-dot`` ; Token.``t-square-bracket-start`` ; Token.``t-square-bracket-end`` ; Token.``t-curly-bracket-start``
    //            Token.``t-curly-bracket-end`` ; Token.``t-hash`` ; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``
    //            Token.``t-gt``; Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``;Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``
    //            Token.``ns-dec-digit``; Token.``c-escape``
    //            ])

    //let ``b-line-feed`` = RGP ("\u000a", [Token.NewLine])
    //let ``b-carriage-return`` = RGP ("\u000d", [Token.NewLine])

    //let ``b-break`` = 
    //        (``b-carriage-return`` + ``b-line-feed``) |||  //  DOS, Windows
    //        ``b-carriage-return``                          |||  //  MacOS upto 9.x
    //        ``b-line-feed``                                     //  UNIX, MacOS X
    //let ``b-non-content`` = ``b-break``
    //let ``s-space`` : string = "\u0020"
    //let ``s-tab`` = "\u0009"  
    //let ``b-comment`` = ``b-non-content`` ||| RGP("\\z", [Token.EOF]) // EOF..
    //let ``nb-char``  = ``c-printable`` - RGO("\u000a\u000d", [Token.NewLine]) 
    //let ``s-white`` = RGO(``s-space`` + ``s-tab``, [Token.``t-space``; Token.``t-tab``])
    //let ``s-separate-in-line`` = OOM(``s-white``) ||| ``start-of-line``
    //let ``c-nb-comment-text`` = RGP("#", [Token.``t-hash``]) + ZOM(``nb-char``)
    //let ``s-b-comment`` = OPT(``s-separate-in-line`` + OPT(``c-nb-comment-text``)) + ``b-comment`` 
    //let ``l-comment`` = ``s-separate-in-line`` + OPT(``c-nb-comment-text``) + ``b-comment``
    //let ``s-l-comments`` = (``s-b-comment`` ||| ``start-of-line``) + ZOM(``l-comment``)
    //let ``s-indent(n)`` ps = Repeat(RGP (``s-space``, [Token.``t-space``]), ps)
    //let ``s-flow-line-prefix`` ps = (``s-indent(n)`` ps) + OPT(``s-separate-in-line``)

    let rgx = (``s-l-comments`` + (``s-flow-line-prefix`` 0)) ||| ``s-separate-in-line``
    let rgxst = rgx |> CreatePushParser


    let yaml = "- value"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData  |> shouldEqual (expected "")


[<Test>]
let ``Parse s-l-comments - should match``() =

    let rgx = ``s-l-comments``
    let rgxst = rgx |> CreatePushParser

    let yaml = " \n  # C\n"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    streamReader.Position <- 1
    let mr = MatchRegexState streamReader rgxst
    mr.IsMatch |> shouldEqual true
    mr.FullMatch |> stripTokenData  |> shouldEqual (expected "\n  # C\n")

[<Test>]
let ``Parse s-l-comments oldway - should match``() =

    let rgx = ``s-l-comments``
    //let rgxst = rgx |> CreatePushParser

    let yaml = " \n  # C\n"
    let streamReader = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
    streamReader.Position <- 1
    let (mr,pr) = AssesInputPostParseCondition (fun _ -> true)  streamReader rgx
    mr |> shouldEqual true
    pr.Match 
    |>  List.map(fun i -> i.Source)
    |>  List.fold(fun (sb:StringBuilder) (i:string) -> sb.Append(i)) (new StringBuilder())
    |>  fun sb -> sb.ToString()
    |>  shouldEqual "\n  # C\n"

