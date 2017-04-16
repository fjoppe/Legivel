module Chapter6Tests

(*
    Testing examples from chapter 6: http://www.yaml.org/spec/1.2/spec.html#Basic
*)

open NUnit.Framework
open FsUnit
open TestUtils
open YamlParser


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2777865")>]
let ``Example 6.1. Indentation Spaces``() =
    let yml = YamlParseList "
  # Leading comment line spaces are
   # neither content nor indentation.
    
Not indented:
 By one space: |
    By four
      spaces
 Flow style: [    # Leading spaces
   By two,        # in flow style
  Also by two,    # are neither
    Still by two   # content nor
    ]             # indentation.
"
    yml.Length |> should equal 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//{#'Not indented'}?/{#'By one space'}?")
    yml |> pth.Select |> ToScalar |> should equal "By four\n  spaces\n"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2778101")>]
let ``Example 6.2. Indentation Indicators``() =
    let yml = YamlParseList "
? a
: - b
  -  -  c
     - d"
    yml.Length |> should equal 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//{#'a'}?/[]/[]/#'d'")
    yml |> pth.Select |> ToScalar |> should equal "d"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2778394")>]
let ``Example 6.3. Separation Spaces``() =
    let yml = YamlParseList "
- foo:   bar
- - baz
  - baz
"
    yml.Length |> should equal 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//[]/{#'foo'}?")
    yml |> pth.Select |> ToScalar |> should equal "bar"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2778720")>]
let ``Example 6.4. Line Prefixes``() =
    let yml = YamlParseList "
plain: text
  lines
quoted: \"text
  \tlines\"
block: |
  text
   \tlines
"
    yml.Length |> should equal 1
    let yml = yml.Head

    [
        ("plain", "text lines")
        ("quoted", "text lines")
        ("block", "text\n \tlines\n")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2778971")>]
let ``Example 6.5. Empty Lines``() =
    let yml = YamlParseList "
Folding:
  \"Empty line
   \t
  as a line feed\"
Chomping: |
  Clipped empty lines
 
"
    yml.Length |> should equal 1
    let yml = yml.Head

    [
        ("Folding", "Empty line\nas a line feed")
        ("Chomping", "Clipped empty lines\n")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2779289")>]
let ``Example 6.6. Line Folding``() =
    let yml = YamlParseList "
>-
  trimmed
  
 

  as
  space
"
    yml.Length |> should equal 1

    yml |> Some |> ToScalar |> should equal "trimmed\n\n\nas space"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2779603")>]
let ``Example 6.7. Block Folding``() =
    let yml = YamlParseList ">\n  foo \n \n  \t bar\n\n  baz\n"
    yml.Length |> should equal 1

    yml |> Some |> ToScalar |> should equal "foo \n\n\t bar\n\nbaz\n"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2779950")>]
let ``Example 6.8. Flow Folding``() =
    let yml = YamlParseList "\"\n  foo \n \n  \t bar\n\n  baz\n\""
    yml.Length |> should equal 1

    yml |> Some |> ToScalar |> should equal " foo\nbar\nbaz "


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2780342")>]
let ``Example 6.9. Separated Comment``() =
    let yml = YamlParseList "
key:    # Comment
  value"
    yml.Length |> should equal 1
    let yml = yml.Head

    [("key", "value")]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2780544")>]
let ``Example 6.10. Comment Lines``() =
    let yml = YamlParseList "
  # Comment
   

"
    yml.Length |> should equal 0


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2780696")>]
let ``Example 6.11. Multi-Line Comments``() =
    let yml = YamlParseList "
key:    # Comment
        # lines
  value

"
    yml.Length |> should equal 1
    let yml = yml.Head

    [("key", "value")]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2780696")>]
let ``Example 6.12. Separation Spaces``() =
    let yml = YamlParseList "{ first: Sammy, last: Sosa }:\n# Statistics:\n  hr:  # Home runs\n     65\n  avg: # Average\n   0.278"

    yml.Length |> should equal 1
    let yml = yml.Head

    [("first", "Sammy"); ("last", "Sosa")]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{}/{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )

    [("hr", "65");("avg", "0.278")]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{}?/{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )

[<Ignore "Check warning">]
[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2781445")>]
let ``Example 6.13. Reserved Directives``() =
    let yml = YamlParseList "
%FOO  bar baz # Should be ignored
              # with a warning.
--- \"foo\"
"
    yml.Length |> should equal 1

    yml |> Some |> ToScalar |> should equal "foo"


