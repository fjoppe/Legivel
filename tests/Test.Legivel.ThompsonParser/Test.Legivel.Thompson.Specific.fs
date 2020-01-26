module Test.Legivel.Thompson.Specific


open Legivel.Tokenizer
open NUnit.Framework
open FsUnitTyped
open Legivel.Utilities.RegexDSL
open Legivel.ThompsonParser


//let assertFullMatch nfa yaml =
//    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
//    let r = parseIt nfa stream
//    r |> ParseResult.IsMatch   |> shouldEqual true
//    r |> ParseResult.FullMatch |> clts |> shouldEqual yaml


//let assertGroupMatch nfa yaml gn mt =
//    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
//    let r = parseIt nfa stream
//    r |> ParseResult.IsMatch |> shouldEqual true
//    r.Groups |> List.item gn |> clts |> shouldEqual mt


//let assertPartialMatch nfa yaml strmatched =
//    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
//    let r = parseIt nfa stream
//    r |> ParseResult.IsMatch   |> shouldEqual true
//    r |> ParseResult.FullMatch |> clts |> shouldEqual strmatched

//    let rest = yaml.Substring(strmatched.Length)
//    let c = stream.Get()
//    c.Source.[0] |> shouldEqual rest.[0]

//let assertNoMatch nfa yaml =
//    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
//    let r = parseIt nfa stream
//    r |> ParseResult.IsMatch   |> shouldEqual false
//    r |> ParseResult.FullMatch |> shouldEqual []




[<Test>]
let ``Test l-folded-content``() =
    let ``s-indent(n)`` = Repeat(RGP (HardValues.``s-space``, [Token.``t-space``]), 1)
    let ``s-indent(<n)`` = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 0) (* Where m < n *)
    let ``s-indent(<=n)`` = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 1)  (* Where m ≤ n *)
    
    
    let ``l-trail-comments`` = (``s-indent(<n)``) + HardValues.``c-nb-comment-text`` + HardValues.``b-comment`` + ZOM(HardValues.``l-comment``)
    let ``l-strip-empty`` = ZOM((``s-indent(<=n)``) + HardValues.``b-non-content``) + OPT(``l-trail-comments``)
    let ``l-chomped-empty`` = ``l-strip-empty``
    let ``b-chomped-last`` = HardValues.``b-as-line-feed``   ||| RGP("\\z", [Token.EOF])
    let ``s-block-line-prefix`` = ``s-indent(n)``
    let ``s-line-prefix`` = ``s-block-line-prefix``
    let ``l-empty Block-in`` = ((``s-line-prefix``) ||| (``s-indent(<n)``)) + HardValues.``b-as-line-feed``
    let ``b-l-trimmed`` = HardValues.``b-non-content`` + OOM(``l-empty Block-in``)
    let ``s-nb-spaced-text`` = (``s-indent(n)``) + HardValues.``s-white`` + ZOM(HardValues.``nb-char``)
    let ``b-l-folded Block-in`` = (``b-l-trimmed``) ||| HardValues.``b-as-space``
    let ``s-nb-folded-text`` = (``s-indent(n)``) + ZOM(HardValues.``nb-char``)
    let ``l-nb-folded-lines`` = (``s-nb-folded-text``) + ZOM((``b-l-folded Block-in``) + ``s-nb-folded-text``)
    
    let ``b-l-spaced`` = HardValues.``b-as-line-feed`` + ZOM(``l-empty Block-in``)
    let ``l-nb-spaced-lines`` = (``s-nb-spaced-text``) + ZOM((``b-l-spaced``) + (``s-nb-spaced-text``))

    let ``l-nb-same-lines`` = 
        ZOM(``l-empty Block-in``) + ((``l-nb-folded-lines``) ||| (``l-nb-spaced-lines``))
    
    
    let ``l-nb-diff-lines`` = (``l-nb-same-lines``) + ZOM(HardValues.``b-as-line-feed`` + (``l-nb-same-lines``))
    let ``l-folded-content`` = GRP(OPT((``l-nb-diff-lines``) + (``b-chomped-last``))) + (``l-chomped-empty``)
    

    let nfa = ``l-folded-content`` |> rgxToNFA

    let yaml = "
>
 Sammy Sosa completed another
 fine season with great stats.

   63 Home Runs
   0.288 Batting Average

 What a year!"


    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
    stream.Position <- 3


    let res = parseIt nfa stream
    res.IsMatch |> shouldEqual true
    res.FullMatch |> clts |> shouldEqual " Sammy Sosa completed another\n fine season with great stats.\n\n   63 Home Runs\n   0.288 Batting Average\n\n What a year!"


[<Test>]
let ``Test c-single-quoted``() =
    let ``s-indent(n)`` = Repeat(RGP (HardValues.``s-space``, [Token.``t-space``]), 1)
    let ``s-indent(<n)`` = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, 0) (* Where m < n *)
    
    let ``s-flow-line-prefix`` = (``s-indent(n)``) + OPT(HardValues.``s-separate-in-line``)
    let ``s-line-prefix Flow-in`` = ``s-flow-line-prefix``
    let ``l-empty Flow-in`` = ((``s-line-prefix Flow-in``) ||| (``s-indent(<n)``)) + HardValues.``b-as-line-feed``
    let ``b-l-trimmed Flow-in`` = HardValues.``b-non-content`` + OOM(``l-empty Flow-in``)
    let ``b-l-folded Flow-in`` = ``b-l-trimmed Flow-in`` ||| HardValues.``b-as-space``

    let ``s-flow-folded`` =
        OPT(HardValues.``s-separate-in-line``) + (``b-l-folded Flow-in``) + ``s-line-prefix Flow-in``
    
    let ``s-single-next-line`` = 
        ZOM((``s-flow-folded``) + HardValues.``ns-single-char`` + HardValues.``nb-ns-single-in-line``) + (``s-flow-folded``) |||
        OOM((``s-flow-folded``) + HardValues.``ns-single-char`` + HardValues.``nb-ns-single-in-line``) + ZOM(HardValues.``s-white``)        
    
    let ``nb-single-multi-line`` = HardValues.``nb-ns-single-in-line`` + ((``s-single-next-line``) ||| ZOM(HardValues.``s-white``))
    let ``nb-single-text`` = ``nb-single-multi-line``
    let ``c-single-quoted`` = HardValues.``c-single-quote`` + GRP(``nb-single-text``) + HardValues.``c-single-quote``

    let nfa = ``c-single-quoted`` |> rgxToNFA

    let yaml = "'test single quote'"
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")


    let r = parseIt nfa stream

    r.Groups.[0] |> clts |> shouldEqual "test single quote"



[<Test>]
let ``c-double-quoted``() =
    let ``s-indent(n)`` = Repeat(RGP (HardValues.``s-space``, [Token.``t-space``]), 0)
    let ``s-indent(<n)`` = Range(RGP (HardValues.``s-space``, [Token.``t-space``]), 0, -1) (* Where m < n *)
    
    let ``s-flow-line-prefix`` = (``s-indent(n)``) + OPT(HardValues.``s-separate-in-line``)
    
    let ``s-line-prefix Flow-in`` = ``s-flow-line-prefix``
    
    let ``l-empty Flow-in`` = ((``s-line-prefix Flow-in``) ||| (``s-indent(<n)``)) + HardValues.``b-as-line-feed``
    
    let ``b-l-trimmed Flow-in`` = HardValues.``b-non-content`` + OOM(``l-empty Flow-in``)
    
    
    let ``b-l-folded Flow-in`` = ``b-l-trimmed Flow-in`` ||| HardValues.``b-as-space``
    
    let ``s-flow-folded`` =
        OPT(HardValues.``s-separate-in-line``) + (``b-l-folded Flow-in``) + ``s-line-prefix Flow-in``
    
    
    let ``s-double-escaped`` = ZOM(HardValues.``s-white``) + HardValues.``c-escape`` + HardValues.``b-non-content`` + ZOM(``l-empty Flow-in``) + (``s-flow-line-prefix``)
    
    let ``s-double-break`` = (``s-double-escaped``) ||| (``s-flow-folded``)
    
    let ``s-double-next-line`` =  
        ZOM((``s-double-break``) + HardValues.``ns-double-char`` + HardValues.``nb-ns-double-in-line``) + (``s-double-break``) |||
        OOM((``s-double-break``) + HardValues.``ns-double-char`` + HardValues.``nb-ns-double-in-line``) + ZOM(HardValues.``s-white``)
    
    
    let ``nb-double-multi-line`` = HardValues.``nb-ns-double-in-line`` + ((``s-double-next-line``) ||| ZOM(HardValues.``s-white``))
    
    let ``nb-double-text`` = ``nb-double-multi-line`` 
    
    let ``c-double-quoted`` = HardValues.``c-double-quote`` + GRP(``nb-double-text``) + HardValues.``c-double-quote``
    
    let nfa = ``c-double-quoted`` |> rgxToNFA

    let yaml = "
\"Fun with \\\\
\\\" \\a \\b \\e \\f \\
\\n \\r \\t \\v \\0 \\
\\  \\_ \\N \\L \\P \\
\\x41 \\u0041 \\U00000041\"
"

    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
    stream.Position <- 1


    let r = parseIt nfa stream

    r.IsMatch |> shouldEqual false
    r.Groups.[0] |> clts |> shouldEqual "Fun with \\\\\n\\\" \\a \\b \\e \\f \\\n\\n \\r \\t \\v \\0 \\\n\\  \\_ \\N \\L \\P \\\n\\x41 \\u0041 \\U00000041"


[<Test>]
let ``s-separate``() =
    let ``s-indent(n)`` = Repeat(RGP (HardValues.``s-space``, [Token.``t-space``]), 1)
    let ``s-flow-line-prefix`` = (``s-indent(n)``) + OPT(HardValues.``s-separate-in-line``)
    let ``s-separate-lines`` = (HardValues.``s-l-comments`` + (``s-flow-line-prefix``)) ||| HardValues.``s-separate-in-line``
    let ``s-separate`` = ``s-separate-lines``
    
    let nfa = ``s-separate`` |> rgxToNFA
    
    let yaml = "
key:    # Comment
        # lines
  value

"
    let stream = RollingStream<_>.Create (tokenProcessor yaml) (TokenData.Create (Token.EOF) "")
    stream.Position <- 5

    let r = parseIt nfa stream

    r.IsMatch |> shouldEqual true
    stream.Position |> shouldEqual 37
