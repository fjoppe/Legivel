module Chapter5Tests

(*
    Testing examples from chapter 5: http://www.yaml.org/spec/1.2/spec.html#Characters
*)

open NUnit.Framework
open FsUnitTyped
open TestUtils
open Legivel.Traverse

(*
    BOM / byte order mark => currently is assumed that .Net will take care of this
*)

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760118")>]
let ``Example 5.3. Block Structure Indicators``() =
    let yml = YamlParse "
sequence:
- one
- two
mapping:
  ? sky
  : blue
  sea : green
"

    let pth = YamlPath.Create (sprintf "//{#'sequence'}?")
    yml |> pth.Select |> ToSequence |> List.length |> shouldEqual 2

    let pth = YamlPath.Create (sprintf "//{#'sequence'}?/[]/#'one'")
    yml |> pth.Select |> ToScalar |> shouldEqual "one"

    let pth = YamlPath.Create (sprintf "//{#'mapping'}?/{#'sky'}?")
    yml |> pth.Select |> ToScalar |> shouldEqual "blue"

    let pth = YamlPath.Create (sprintf "//{#'mapping'}?/{#'sea'}?")
    yml |> pth.Select |> ToScalar |> shouldEqual "green"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2772813")>]
let ``Example 5.4. Flow Collection Indicators``() =
    let yml = YamlParse "
sequence: [ one, two, ]
mapping: { sky: blue, sea: green }
"

    let pth = YamlPath.Create (sprintf "//{#'sequence'}?")
    yml |> pth.Select |> ToSequence |> List.length |> shouldEqual 2

    let pth = YamlPath.Create (sprintf "//{#'sequence'}?/[]/#'one'")
    yml |> pth.Select |> ToScalar |> shouldEqual "one"

    let pth = YamlPath.Create (sprintf "//{#'mapping'}?/{#'sky'}?")
    yml |> pth.Select |> ToScalar |> shouldEqual "blue"

    let pth = YamlPath.Create (sprintf "//{#'mapping'}?/{#'sea'}?")
    yml |> pth.Select |> ToScalar |> shouldEqual "green"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2773032")>]
let ``Example 5.5. Comment Indicator``() =
    let yml = YamlParseEmpty "
# Comment only.
"
    yml |> shouldEqual true

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2773402")>]
let ``Example 5.6. Node Property Indicators``() =
    let yml = YamlParseList "
anchored: !local &anchor value
alias: *anchor
"
    yml.Length |> shouldEqual 1

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2773653")>]
let ``Example 5.7. Block Scalar Indicators``() =
    let yml = YamlParseList "
literal: |
  some
  text
folded: >
  some
  text
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//{#'literal'}?")
    yml |> pth.Select |> ToScalar |> shouldEqual "some\ntext\n"

    let pth = YamlPath.Create (sprintf "//{#'folded'}?")
    yml |> pth.Select |> ToScalar |> shouldEqual "some text\n"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2773890")>]
let ``Example 5.8. Quoted Scalar Indicators``() =
    let yml = YamlParseList "
single: 'text'
double: \"text\"
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//{#'single'}?")
    yml |> pth.Select |> ToScalar |> shouldEqual "text"

    let pth = YamlPath.Create (sprintf "//{#'double'}?")
    yml |> pth.Select |> ToScalar |> shouldEqual "text"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2774058")>]
let ``Example 5.9. Directive Indicator``() =
    let yml = YamlParseList "
%YAML 1.2
--- text
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#'text'")
    yml |> pth.Select |> ToScalar |> shouldEqual "text"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2774228")>]
let ``Example 5.10. Invalid use of Reserved Indicators``() =
    let err = YamlParseWithErrors "
commercial-at: @text
"
    err.Error |> List.map(fun e -> e.Message) |> List.head |> shouldEqual "Reserved indicators can't start a plain scalar."

    let err = YamlParseWithErrors "
grave-accent: `text
"
    err.Error |> List.map(fun e -> e.Message) |> List.head |> shouldEqual "Reserved indicators can't start a plain scalar."


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2775100")>]
let ``Example 5.11. Line Break Characters``() =
    let yml = YamlParseList "
|
  Line break (no glyph)
  Line break (glyphed)
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#")
    yml |> pth.Select |> ToScalar |> shouldEqual "Line break (no glyph)\nLine break (glyphed)\n"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2775350")>]
let ``Example 5.12. Tabs and Spaces``() =
    let yml = YamlParseList "
# Tabs and spaces
quoted: \"Quoted \t\"
block:\t|
  void main() {
  \tprintf(\"Hello, world!\\n\");
  }
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("quoted", "Quoted \t")
        ("block", "void main() {\n\tprintf(\"Hello, world!\\n\");\n}\n")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2775100")>]
let ``Example 5.13. Escaped Characters``() =
    let yml = YamlParseList "
\"Fun with \\\\
\\\" \\a \\b \\e \\f \\
\\n \\r \\t \\v \\0 \\
\\  \\_ \\N \\L \\P \\
\\x41 \\u0041 \\U00000041\"
"
    yml.Length |> shouldEqual 1

    yml |> Some |> ToScalar |> shouldEqual "Fun with \x5C \x22 \x07 \x08 \x1B \x0C \x0A \x0D \x09 \x0B \x00 \x20 \xA0 \x85 \u2028 \u2029 A A A"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2777449")>]
let ``Example 5.14. Invalid Escaped Characters``() =
    let err = YamlParseWithErrors "
Bad escapes:
  \"\\c
  \\xq-\"
"
    err.Error.Length |> shouldBeGreaterThan 0
    err.Error |> List.map(fun e -> e.Message) |> List.exists(fun s -> s = "Literal string contains illegal characters.") |> shouldEqual true

    let err = YamlParseWithErrors "
Bad escapes:
  \"
  \\xq-\"
"
    err.Error.Length |> shouldBeGreaterThan 0
    err.Error |> List.map(fun e -> e.Message) |> List.exists(fun s -> s = "Literal string contains illegal characters.") |> shouldEqual true
