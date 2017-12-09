module Chapter6Tests

(*
    Testing examples from chapter 6: http://www.yaml.org/spec/1.2/spec.html#Basic
*)

open NUnit.Framework
open FsUnitTyped
open TestUtils
open Legivel.Traverse


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
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//{#'Not indented'}?/{#'By one space'}?")
    yml |> pth.Select |> ToScalar |> shouldEqual "By four\n  spaces\n"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2778101")>]
let ``Example 6.2. Indentation Indicators``() =
    let yml = YamlParseList "
? a
: - b
  -  -  c
     - d"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//{#'a'}?/[]/[]/#'d'")
    yml |> pth.Select |> ToScalar |> shouldEqual "d"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2778394")>]
let ``Example 6.3. Separation Spaces``() =
    let yml = YamlParseList "
- foo:   bar
- - baz
  - baz
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//[]/{#'foo'}?")
    yml |> pth.Select |> ToScalar |> shouldEqual "bar"


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
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("plain", "text lines")
        ("quoted", "text lines")
        ("block", "text\n \tlines\n")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
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
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("Folding", "Empty line\nas a line feed")
        ("Chomping", "Clipped empty lines\n")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2779289")>]
let ``Example 6.6. Line Folding``() =
    let yml = YamlParseList "
>-
  trimmed
  
 

  as
  space
"
    yml.Length |> shouldEqual 1

    yml |> Some |> ToScalar |> shouldEqual "trimmed\n\n\nas space"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2779603")>]
let ``Example 6.7. Block Folding``() =
    let yml = YamlParseList ">\n  foo \n \n  \t bar\n\n  baz\n"
    yml.Length |> shouldEqual 1

    yml |> Some |> ToScalar |> shouldEqual "foo \n\n\t bar\n\nbaz\n"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2779950")>]
let ``Example 6.8. Flow Folding``() =
    let yml = YamlParseList "\"\n  foo \n \n  \t bar\n\n  baz\n\""
    yml.Length |> shouldEqual 1

    yml |> Some |> ToScalar |> shouldEqual " foo\nbar\nbaz "


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2780342")>]
let ``Example 6.9. Separated Comment``() =
    let yml = YamlParseList "
key:    # Comment
  value"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [("key", "value")]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2780544")>]
let ``Example 6.10. Comment Lines``() =
    let yml = YamlParseEmpty "
  # Comment
   

"
    yml |> shouldEqual true


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2780696")>]
let ``Example 6.11. Multi-Line Comments``() =
    let yml = YamlParseList "
key:    # Comment
        # lines
  value

"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [("key", "value")]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2780696")>]
let ``Example 6.12. Separation Spaces``() =
    let yml = YamlParseList "{ first: Sammy, last: Sosa }:\n# Statistics:\n  hr:  # Home runs\n     65\n  avg: # Average\n   0.278"

    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [("first", "Sammy"); ("last", "Sosa")]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{}/{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )

    [("hr", "65");("avg", "0.278")]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{}?/{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2781445")>]
let ``Example 6.13. Reserved Directives``() =
    let wrr = YamlParseWithWarning "
%FOO bar baz # Should be ignored
              # with a warning.
--- \"foo\"
"
    wrr.Warn.Length |> shouldEqual 1
    wrr.Warn |> List.filter(fun m -> m.Message = "Reserved directive will ignored: %FOO bar baz") |> List.length |> shouldEqual 1
    [wrr.Document] |> Some |> ToScalar |> shouldEqual "foo"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2781929")>]
let ``Example 6.14. “YAML” directive``() =
    let yml = YamlParseWithWarning "
%YAML 1.3 # Attempt parsing
           # with a warning
---
\"foo\""
    yml.Warn.Length |> shouldEqual 1
    yml.Warn |> List.filter(fun m -> m.Message = "YAML 1.3 document will be parsed as YAML 1.2") |> List.length |> shouldEqual 1

    [yml.Document] |> Some |> ToScalar |> shouldEqual "foo"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2782032")>]
let ``Example 6.15. Invalid Repeated YAML directive``() =
    let err = YamlParseWithErrors "
%YAML 1.2
%YAML 1.1
foo"
    err.Error.Length |> shouldEqual 1
    err.Error |> List.filter(fun m -> m.Message = "The YAML directive must only be given at most once per document.") |> List.length |> shouldEqual 1


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2782252")>]
let ``Example 6.16. “TAG” directive``() =
    let yml = YamlParseList "
%TAG !yaml! tag:yaml.org,2002:
---
!yaml!str \"foo\"
"
    yml.Length |> shouldEqual 1
    yml |> Some |> ToScalar |> shouldEqual "foo"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2782400")>]
let ``Example 6.17. Invalid Repeated TAG directive``() =
    let err = YamlParseWithErrors "
%TAG ! !foo
%TAG ! !foo
bar"
    err.Error.Length |> shouldBeGreaterThan 0
    err.Error |> List.filter(fun m -> m.Message.StartsWith("The TAG directive must only be given at most once per handle in the same document")) |> List.length |> shouldEqual 1


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2782728")>]
let ``Example 6.18. Primary Tag Handle``() =
    let ymllst = YamlParseList "
# Private
!foo \"bar\"
...
# Global
%TAG ! tag:example.com,2000:app/
---
!foo \"bar\"
"
    ymllst.Length |> shouldEqual 2
    let yml = ymllst |> List.head
    let p = YamlPath.Create (sprintf "//#'bar'")
    yml |> p.Select |> ToScalar |> shouldEqual "bar"
    yml |> p.Select |> ExtractTag |> shouldEqual "!foo"

    let yml = ymllst |> List.last 
    let p = YamlPath.Create (sprintf "//#'bar'")
    yml |> p.Select |> ToScalar |> shouldEqual "bar"
    yml |> p.Select |> ExtractTag |> shouldEqual "tag:example.com,2000:app/foo"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2782940")>]
let ``Example 6.19. Secondary Tag Handle``() =
    let yml = YamlParseList "
%TAG !! tag:example.com,2000:app/
---
!!int 1 - 3 # Interval, not integer
"
    yml.Length |> shouldEqual 1
    yml |> Some |> ToScalar |> shouldEqual "1 - 3"
    yml |> Some |> ExtractTag |> shouldEqual "tag:example.com,2000:app/int"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2783195")>]
let ``Example 6.20. Tag Handles``() =
    let yml = YamlParseList "
%TAG !e! tag:example.com,2000:app/
---
!e!foo \"bar\"
"
    yml.Length |> shouldEqual 1
    yml |> Some |> ToScalar |> shouldEqual "bar"
    yml |> Some |> ExtractTag |> shouldEqual "tag:example.com,2000:app/foo"


//[<Ignore "Should support local tags">]
[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2783499")>]
let ``Example 6.21. Local Tag Prefix``() =
    let yml = YamlParseList "
%TAG !m! !my-
--- # Bulb here
!m!light fluorescent
...
%TAG !m! !my-
--- # Color here
!m!light green
"
    yml.Length |> shouldEqual 2

    let yml1, yml2 = List.head yml, List.last yml 


    [yml1] |> Some |> ToScalar |> shouldEqual "fluorescent"
    [yml1] |> Some |> ExtractTag |> shouldEqual "!my-light"

    [yml2] |> Some |> ToScalar |> shouldEqual "green"
    [yml2] |> Some |> ExtractTag |> shouldEqual "!my-light"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2783726")>]
let ``Example 6.22. Global Tag Prefix``() =
    let yml = YamlParseList "

%TAG !e! tag:example.com,2000:app/
---
- !e!foo \"bar\"
"
    yml.Length |> shouldEqual 1

    let p = YamlPath.Create (sprintf "//[]/#'bar'")

    yml.Head |> p.Select |> ToScalar |> shouldEqual "bar"
    yml.Head |> p.Select |> ExtractTag |> shouldEqual "tag:example.com,2000:app/foo"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2783940")>]
let ``Example 6.23. Node Properties``() =
    let yml = YamlParseList "
!!str &a1 \"foo\":
  !!str bar
&a2 baz : *a1
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [("foo", "bar"); ("baz", "foo")]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )

//[<Ignore "Check tags">]
[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2784370")>]
let ``Example 6.24. Verbatim Tags``() =
    let yml = YamlParseList "
!<tag:yaml.org,2002:str> foo :
  !<!bar> baz
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let p1 = YamlPath.Create (sprintf "//{#'foo'}")
    yml |> p1.Select |> ToScalar |> shouldEqual "foo"
    yml |> p1.Select |> ExtractTag |> shouldEqual "tag:yaml.org,2002:str"

    let p1 = YamlPath.Create (sprintf "//{#'foo'}?")
    yml |> p1.Select |> ToScalar |> shouldEqual "baz"
    yml |> p1.Select |> ExtractTag |> shouldEqual "!bar"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2784444")>]
let ``Example 6.25. Invalid Verbatim Tags``() =
    let err = YamlParseWithErrors "- !<!> foo"

    err.Error.Length |> shouldBeGreaterThan 0
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Verbatim tags aren't resolved, so ! is invalid.")) |> List.length |> shouldEqual 1

    let err = YamlParseWithErrors "- !<$:?> bar"
    err.Error.Length |> shouldBeGreaterThan 0
    err.Error |> List.filter(fun m -> m.Message.StartsWith("Verbatim tag is neither a local or global tag.")) |> List.length |> shouldEqual 1


//[<Ignore "Check tags">]
[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2785009")>]
let ``Example 6.26. Tag Shorthands``() =
    let yml = YamlParseList "
%TAG !e! tag:example.com,2000:app/
---
- !local foo
- !!str bar
- !e!tag%21 baz
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [("!local","foo"); ("tag:yaml.org,2002:str", "bar"); ("tag:example.com,2000:app/tag!","baz")]
    |> List.iter(fun (t,v) ->
        let p = YamlPath.Create (sprintf "//[]/#'%s'" v)
        yml |> p.Select |> ToScalar |> shouldEqual v
        yml |> p.Select |> ExtractTag |> shouldEqual t
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2785092")>]
let ``Example 6.27. Invalid Tag Shorthands``() =
    let err = YamlParseWithErrors "
%TAG !e! tag:example,2000:app/
---
- !e! foo
"
    err.Error.Length |> shouldBeGreaterThan 0
    err.Error |> List.exists(fun m -> m.Message = "The !e! handle has no suffix.") |> shouldEqual true

    let err = YamlParseWithErrors "
%TAG !e! tag:example,2000:app/
---
- !h!bar baz
"
    err.Error.Length |> shouldBeGreaterThan 0
    err.Error |> List.exists(fun m -> m.Message = "The !h! handle wasn't declared.") |> shouldEqual true


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2785512")>]
let ``Example 6.28. Non-Specific Tags``() =
    // values have been changed for test-evaluation purposes
    let yml = YamlParseList "
# Assuming conventional resolution:
- \"1\"
- 12
- ! 123
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("tag:yaml.org,2002:str", "1")
        ("tag:yaml.org,2002:int", "12")
        ("tag:yaml.org,2002:str", "123")
    ]
    |> List.iter(fun (t,v) ->
        let p = YamlPath.Create (sprintf "//[]/#'%s'" v)
        yml |> p.Select |> ExtractTag |> shouldEqual t
        yml |> p.Select |> ToScalar |> shouldEqual v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2785977")>]
let ``Example 6.29. Node Anchors``() =
    let yml = YamlParseList "
First occurrence: &anchor Value
Second occurrence: *anchor
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [("First occurrence", "Value"); ("Second occurrence", "Value")]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )


