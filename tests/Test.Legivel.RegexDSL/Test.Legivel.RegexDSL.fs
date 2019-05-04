module Test.Legivel.RegexDSL

open Legivel.Utilities.RegexDSL
open NUnit.Framework
open FsUnitTyped
open System.Collections.Generic
open Legivel.Tokenizer

let EndOfStream = TokenData.Create (Token.EOF) ""

let ReadStream (ip:TokenData list) =
    let q = new Queue<TokenData>(ip)
    (fun () -> if q.Count > 0 then q.Dequeue() else EndOfStream)


module ``Test Regex Constructs``=
    open System.Text

    [<Test>]
    let ``Parse Plain Character - sunny day``() =
        let testConstuct = RGP("A", [Token.``c-printable``])

        let yaml = "A"

        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens testConstuct

        b   |>  shouldEqual true
        tkl.Match 
        |>  List.map(fun td -> td.Token)
        |>  shouldEqual [
            Token.``c-printable``
        ]
        tokens.Stream |> Seq.head |> fun td -> td.Token |> shouldEqual Token.EOF    

    [<Test>]
    let ``Parse Plain Character - rainy day``() =
        let testConstuct = RGP("A", [Token.``c-printable``])

        let yaml = "-"

        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens testConstuct

        b   |>  shouldEqual false


    [<Test>]
    let ``Parse RGO Character - sunny day``() =
        let testConstuct = RGO("A-", [Token.``c-printable``;Token.``t-hyphen``])

        let yaml = "-"

        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens testConstuct

        b   |>  shouldEqual true
        tkl.Match
        |>  List.map(fun td -> td.Token)
        |>  shouldEqual [
            Token.``t-hyphen``
        ]
        tokens.Stream |> Seq.head |> fun td -> td.Token |> shouldEqual Token.EOF 


    [<Test>]
    let ``Parse RGO Character - rainy day``() =
        let testConstuct = RGO("A-", [Token.``c-printable``;Token.``t-hyphen``])

        let yaml = "9"

        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens testConstuct

        b   |>  shouldEqual false


    [<Test>]
    let ``Parse ZOM Character - sunny day``() =
        let testConstuct = ZOM(RGP("A", [Token.``c-printable``]))

        let yaml = "A"

        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens testConstuct

        b   |>  shouldEqual true
        tkl.Match 
        |>  List.map(fun td -> td.Token)
        |>  shouldEqual [
            Token.``c-printable``
        ]
        tokens.Stream |> Seq.head |> fun td -> td.Token |> shouldEqual Token.EOF 


    [<Test>]
    let ``Parse ZOM Character - rainy day``() =
        let testConstuct = ZOM(RGP("A", [Token.``c-printable``]))

        let yaml = "-"

        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens testConstuct

        b   |>  shouldEqual true
        tkl.Match |>  List.length |> shouldEqual 0
        tokens.Stream |> Seq.head |> fun td -> td.Token |> shouldEqual Token.``t-hyphen``

    [<Test>]
    let ``Parse ZOM infinite loop bug``() =
        let testConstuct = ZOM(OPT(RGP("A", [Token.``c-printable``])))

        let yaml = "-"

        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens testConstuct

        b   |>  shouldEqual true
        tkl.Match |>  List.length |> shouldEqual 0
        tokens.Stream |> Seq.head |> fun td -> td.Token |> shouldEqual Token.``t-hyphen``


    [<Test>]
    let ``Parse Group sub-match in match``() =
        let testRegex = ZOM(RGO("AB", [Token.``c-printable``])) + GRP(ZOM(RGO("12", [Token.``ns-dec-digit``]))) + ZOM(RGO("CD", [Token.``c-printable``]))

        let inputstring = "AA2121DDC"
        let tokens = RollingStream<_>.Create (tokenProcessor inputstring) EndOfStream
        let (b, tkl) = AssesInput tokens testRegex

        b   |>  shouldEqual true
        tkl.Match |>  List.length |> shouldEqual (inputstring.Length)
        tkl.Groups |>List.length |> shouldEqual 1
        tkl.Groups.Head 
            |> List.fold(fun (str:StringBuilder) i -> str.Append(i.Source)) (StringBuilder())
            |> fun sb -> sb.ToString()
            |>  shouldEqual "2121"
        tokens.Stream |> Seq.head |> fun td -> td.Token |> shouldEqual Token.EOF

    [<Test>]
    let ``Parse Group double sub-match in match``() =
        let testRegex = 
            ZOM(RGO("AB", [Token.``c-printable``])) 
            + GRP(ZOM(RGO("12", [Token.``ns-dec-digit``]))) 
            + GRP(ZOM(RGO("AB", [Token.``c-printable``]))) 
            + ZOM(RGO("34", [Token.``ns-dec-digit``]))

        let inputstring = "AA212BABBA34"
        let tokens = RollingStream<_>.Create (tokenProcessor inputstring) EndOfStream
        let (b, tkl) = AssesInput tokens testRegex

        b   |>  shouldEqual true
        tkl.Match |>  List.length |> shouldEqual (inputstring.Length)
        tkl.Groups |>List.length |> shouldEqual 2
        let ar = tkl.Groups |> List.rev |> List.toArray
        ar.[0]
            |> List.fold(fun (str:StringBuilder) i -> str.Append(i.Source)) (StringBuilder())
            |> fun sb -> sb.ToString()
            |>  shouldEqual "212"
        ar.[1]
            |> List.fold(fun (str:StringBuilder) i -> str.Append(i.Source)) (StringBuilder())
            |> fun sb -> sb.ToString()
            |>  shouldEqual "BABBA"

        tokens.Stream |> Seq.head |> fun td -> td.Token |> shouldEqual Token.EOF


module ``AssesInput for Block Sequence``=
    let ``l+block-sequence`` =
        let ``b-break`` = RGP("\n", [Token.NewLine])
        let ``c-l-block-seq-entry`` = RGP("-", [Token.``t-hyphen``]) + OOM(RGP(" ", [Token.``t-space``])) + OOM(RGO("\u0009\u0020-\uffff", [Token.``c-printable``; Token.``nb-json``; Token.``t-hyphen``; Token.``ns-dec-digit``]))
        OOM(``c-l-block-seq-entry`` + ``b-break``)


    [<Test>]
    let ``Parse Token Stream - full match``() =
        let yaml = "- 5\n- 10\n- -9\n"

        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens ``l+block-sequence``

        b   |>  shouldEqual true
        tkl.Match 
        |>  List.map(fun td -> td.Token)
        |>  shouldEqual [
            Token.``t-hyphen``; Token.``t-space``; Token.``ns-dec-digit``; Token.NewLine
            Token.``t-hyphen``; Token.``t-space``; Token.``ns-dec-digit``; Token.``ns-dec-digit``; Token.NewLine
            Token.``t-hyphen``; Token.``t-space``; Token.``t-hyphen``; Token.``ns-dec-digit``; Token.NewLine
        ]
        tokens.Stream |> Seq.head |> fun td -> td.Token |> shouldEqual Token.EOF
    


    [<Test>]
    let ``Parse Token Stream - partial match``() =
        let yaml = "- 5\n- 10\n- -9 # does not match\n"

        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens ``l+block-sequence``

        b   |>  shouldEqual true
        tkl.Match 
        |>  List.map(fun td -> td.Token)
        |>  shouldEqual [
            Token.``t-hyphen``; Token.``t-space``; Token.``ns-dec-digit``; Token.NewLine
            Token.``t-hyphen``; Token.``t-space``; Token.``ns-dec-digit``; Token.``ns-dec-digit``; Token.NewLine
        ]

        tokens.Stream |> Seq.head |> fun td -> td.Token |> shouldEqual Token.``t-hyphen``

    [<Test>]
    let ``Parse Token Stream - no match``() =
        let yaml = "[ 1, 2, 3 ]"

        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens ``l+block-sequence``

        b   |>  shouldEqual false
        tkl.Match |>  shouldEqual []
        tokens.Stream |> Seq.head |> fun td -> td.Token |> shouldEqual Token.``t-square-bracket-start``

    [<Test>]
    let ``Parse DQuoted string - match``() =
        let ``start-of-line`` = (* RGP "\n" ||| *) RGP ("^", [Token.NoToken])
        let ``nb-json`` = 
            RGO ("\u0009\u0020-\uffff",
                [
                Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``c-printable``; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark`` 
                Token.``t-colon`` ; Token.``t-comma``; Token.``t-dot`` ; Token.``t-square-bracket-start`` ; Token.``t-square-bracket-end`` ; Token.``t-curly-bracket-start``
                Token.``t-curly-bracket-end`` ; Token.``t-hash`` ; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``
                Token.``t-gt``; Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``;Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``
                Token.``ns-dec-digit``; Token.``c-escape``; Token.``nb-json``
                ])

        let ``s-space`` = "\u0020"
        let ``b-line-feed`` = RGP ("\u000a", [Token.NewLine])
        let ``b-carriage-return`` = RGP ("\u000d", [Token.NewLine])
        let ``b-break`` =  (``b-carriage-return`` + ``b-line-feed``)|||
                            ``b-carriage-return``                   |||
                            ``b-line-feed``                                   
        let ``b-non-content`` = ``b-break``
        let ``s-white`` = RGO("\u0020" + "\u0009", [Token.``t-space``; Token.``t-tab``])
        let ``ns-dec-digit`` = RGO ("\u0030-\u0039", [Token.``ns-dec-digit``])
        let ``ns-hex-digit`` =
            ``ns-dec-digit`` +
            RGO ("\u0041-\u0046", [Token.``c-printable``])  +  //  A-F
            RGO ("\u0061-\u0066", [Token.``c-printable``])   

        let ``c-escape`` = RGP ("\\\\", [Token.``c-escape``])
        let``ns-esc-null`` = RGP ("0", [Token.``ns-dec-digit``])
        let``ns-esc-bell`` = RGP ("a", [Token.``c-printable``])
        let``ns-esc-backspace`` = RGP( "b", [Token.``c-printable``])
        let``ns-esc-horizontal-tab`` = RGP ("t", [Token.``c-printable``])
        let``ns-esc-line-feed`` = RGP ("n", [Token.``c-printable``])
        let``ns-esc-vertical-tab`` = RGP ("v", [Token.``c-printable``])
        let``ns-esc-form-feed`` = RGP ("f", [Token.``c-printable``])
        let``ns-esc-carriage-return`` = RGP ("r", [Token.``c-printable``])
        let``ns-esc-escape`` = RGP ("e", [Token.``c-printable``])
        let``ns-esc-space`` = RGP ("\u0020", [Token.``t-space``])
        let``ns-esc-double-quote`` = RGP ("\"", [Token.``t-double-quote``])
        let``ns-esc-slash`` = RGP ("/", [Token.``c-printable``])
        let``ns-esc-backslash`` = RGP ("\\\\", [Token.``c-escape``])
        let``ns-esc-next-line`` = RGP ("N", [Token.``c-printable``])
        let``ns-esc-non-breaking-space`` = RGP ("_", [Token.``c-printable``])
        let``ns-esc-line-separator`` = RGP ("L", [Token.``c-printable``])
        let``ns-esc-paragraph-separator`` = RGP ("P", [Token.``c-printable``])
        let``ns-esc-8-bit`` = (RGP ("x", [Token.``c-printable``])) + Repeat(``ns-hex-digit``,2)
        let``ns-esc-16-bit`` = RGP ("u", [Token.``c-printable``]) + Repeat(``ns-hex-digit``,4)
        let``ns-esc-32-bit`` = RGP ("U", [Token.``c-printable``]) + Repeat(``ns-hex-digit``,8) // currently not supported
        let ``c-ns-esc-char`` = RGP ("\\\\", [Token.``c-escape``]) +
            (``ns-esc-null``             |||
             ``ns-esc-bell``             |||
             ``ns-esc-backspace``        |||
             ``ns-esc-horizontal-tab``   |||
             ``ns-esc-line-feed``        |||
             ``ns-esc-vertical-tab``     |||
             ``ns-esc-form-feed``        |||
             ``ns-esc-carriage-return``  |||
             ``ns-esc-escape``           |||
             ``ns-esc-space``            |||
             ``ns-esc-double-quote``     |||
             ``ns-esc-slash``            |||
             ``ns-esc-backslash``        |||
             ``ns-esc-next-line``        |||
             ``ns-esc-non-breaking-space``|||
             ``ns-esc-line-separator``   |||
             ``ns-esc-paragraph-separator``|||
             ``ns-esc-8-bit``            |||
             ``ns-esc-16-bit``           |||
             ``ns-esc-32-bit``)
        let ``ns-double-char`` = ``c-ns-esc-char`` |||  (``nb-json`` - RGO("\\\\\"", [Token.``c-escape``;Token.``t-double-quote``]) - ``s-white``)
        let ``nb-ns-double-in-line`` = ZOM(ZOM(``s-white``) + ``ns-double-char``)
        let ``s-separate-in-line`` = OOM(``s-white``) ||| ``start-of-line``
        let ``s-indent(n)`` = Repeat(RGP (``s-space``, [Token.``t-space``]), 0)
        let ``s-flow-line-prefix`` = (``s-indent(n)``) + OPT(``s-separate-in-line``)
        let ``s-line-prefix`` = ``s-flow-line-prefix`` 
        let ``s-indent(<n)`` = Range(RGP (``s-space``, [Token.``t-space``]), 0, -1) (* Where m < n *)
        let ``b-as-line-feed`` = ``b-break``
        let ``l-empty`` = ((``s-line-prefix``) ||| (``s-indent(<n)``)) + ``b-as-line-feed``
        let ``s-double-escaped`` = ZOM(``s-white``) + ``c-escape`` + ``b-non-content`` + ZOM(``l-empty``) + (``s-flow-line-prefix``)
        let ``b-l-trimmed`` = ``b-non-content`` + OOM(``l-empty``)
        let ``b-as-space`` = ``b-break``
        let ``b-l-folded`` = (``b-l-trimmed``) ||| ``b-as-space``
        let ``s-flow-folded`` = OPT(``s-separate-in-line``) + (``b-l-folded``) + (``s-flow-line-prefix``)
        let ``s-double-break`` = (``s-double-escaped``) ||| (``s-flow-folded``)
        let ``s-double-next-line`` =  
            ZOM((``s-double-break``) + ``ns-double-char`` + ``nb-ns-double-in-line``) + (``s-double-break``) |||
            OOM((``s-double-break``) + ``ns-double-char`` + ``nb-ns-double-in-line``) + ZOM(``s-white``)
        let ``nb-double-multi-line`` = ``nb-ns-double-in-line`` + ((``s-double-next-line``) ||| ZOM(``s-white``))
        let ``c-double-quote`` = RGP ("\"", [Token.``t-double-quote``])

        let patt = ``c-double-quote`` + GRP(``nb-double-multi-line``) + ``c-double-quote``

        let yaml = "\"Quoted \t\""

        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens patt

        b   |>  shouldEqual true


    [<Test>]
    let ``Parse Chomp indicator - match``() =
        let ``start-of-line`` = (* RGP "\n" ||| *) RGP ("^", [Token.NoToken])
        let ``c-printable`` = 
            RGO ("\u0009\u000a\u000d\u0020-\u007e" +   // 8 - bit, #x9 | #xA | #xD | [#x20-#x7E]
                 "\u0085\u00a0-\ud7ff\ue000-\ufffd",   // 16- bit, #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD]
                                                       //  32-bit -> currently not supported because .Net does not encode naturally. Yaml: [#x10000-#x10FFFF]
                [
                Token.``t-space``; Token.``t-tab``; Token.NewLine; Token.``c-printable``; Token.``t-hyphen``; Token.``t-plus``; Token.``t-questionmark`` 
                Token.``t-colon`` ; Token.``t-comma``; Token.``t-dot`` ; Token.``t-square-bracket-start`` ; Token.``t-square-bracket-end`` ; Token.``t-curly-bracket-start``
                Token.``t-curly-bracket-end`` ; Token.``t-hash`` ; Token.``t-ampersand``; Token.``t-asterisk``; Token.``t-quotationmark``; Token.``t-pipe``
                Token.``t-gt``; Token.``t-single-quote``; Token.``t-double-quote``; Token.``t-percent``; Token.``t-commat``;Token.``t-tick``; Token.``t-forward-slash``; Token.``t-equals``
                Token.``ns-dec-digit``; Token.``c-escape``
                ])
        let ``b-line-feed`` = RGP ("\u000a", [Token.NewLine])
        let ``b-carriage-return`` = RGP ("\u000d", [Token.NewLine])
        let ``b-break`` =  (``b-carriage-return`` + ``b-line-feed``)|||
                            ``b-carriage-return``                   |||
                            ``b-line-feed``                                   
        let ``b-non-content`` = ``b-break``
        let ``c-sequence-entry`` = RGP ("-", [Token.``t-hyphen``])
        let ``c-chomping-indicator`` = OPT(RGP("\\+", [Token.``t-plus``]) ||| ``c-sequence-entry``)
        let ``ns-dec-digit`` = RGO ("\u0030-\u0039", [Token.``ns-dec-digit``])      //  0-9
        let ``c-indentation-indicator`` = OPT(``ns-dec-digit``)
        let ``s-white`` = RGO("\u0020" + "\u0009", [Token.``t-space``; Token.``t-tab``])
        let ``s-separate-in-line`` = OOM(``s-white``) ||| ``start-of-line``
        let ``nb-char``  = ``c-printable`` - RGO("\u000a\u000d", [Token.NewLine]) // this.``b-char``
        let ``c-nb-comment-text`` = RGP("#", [Token.``t-hash``]) + ZOM(``nb-char``)
        let ``b-comment`` = ``b-non-content`` ||| RGP("\\z", [Token.EOF])
        let ``s-b-comment`` = OPT(``s-separate-in-line`` + OPT(``c-nb-comment-text``)) + ``b-comment`` 
        let pattern = GRP(``c-indentation-indicator``) + GRP(``c-chomping-indicator``) + ``s-b-comment``

        let yaml = "+

"

        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens pattern

        b   |>  shouldEqual true
        tokens.Stream |> Seq.head |> fun td -> td.Token |> shouldEqual Token.NewLine


        let yaml = "-

"
        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens pattern

        b   |>  shouldEqual true

        let yaml = "

"
        let tokens = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream
        let (b, tkl) = AssesInput tokens pattern

        b   |>  shouldEqual true
