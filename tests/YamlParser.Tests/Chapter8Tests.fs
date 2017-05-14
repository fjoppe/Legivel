module Chapter8Tests

(*
    Testing examples from chapter 7: http://www.yaml.org/spec/1.2/spec.html#Flow
*)

open NUnit.Framework
open FsUnit
open TestUtils
open YamlParser


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2793888")>]
let ``Example 8.1. Block Scalar Header``() =
    let yml = YamlParseList "
- | # Empty header
 literal
- >1 # Indentation indicator
  folded
- |+ # Chomping indicator
 keep

- >1- # Both indicators
  strip
"
    yml.Length |> should equal 1
    let yml = yml.Head

    ["literal\n";" folded\n"; "keep\n\n"; " strip"]
    |> List.iter(fun (v) ->
        let p = YamlPath.Create (sprintf "//[]/#'%s'" v)
        yml |> p.Select |> ToScalar |> should equal v
    )



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2794311")>]
let ``Example 8.2. Block Indentation Indicator``() =
    let yml = YamlParseList "
- |
 detected
- >
 
  
  # detected
- |1
  explicit
- >
 \t
 detected
"
    yml.Length |> should equal 1
    let yml = yml.Head

    ["detected\n";"\n\n# detected\n"; " explicit\n"; "\t detected\n"]
    |> List.iter(fun (v) ->
        let p = YamlPath.Create (sprintf "//[]/#'%s'" v)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Ignore "Chec errors">]
[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2794450")>]
let ``Example 8.3. Invalid Block Scalar Indentation Indicators``() =
    let yml = YamlParseList "
- |
  
 text
- >
  text
 text
- |2
 text
"
    yml.Length |> should equal 0



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2795034")>]
let ``Example 8.4. Chomping Final Line Break``() =
    let yml = YamlParseList "
strip: |-
  text
clip: |
  text
keep: |+
  text
"
    yml.Length |> should equal 1
    let yml = yml.Head

    [
        ("strip","text")
        ("clip","text\n")
        ("keep","text\n")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2795435")>]
let ``Example 8.5. Chomping Trailing Lines``() =
    let yml = YamlParseList "
 # Strip
  # Comments:
strip: |-
  # text
  
 # Clip
  # comments:

clip: |
  # text
 
 # Keep
  # comments:

keep: |+
  # text

 # Trail
  # comments.
"
    yml.Length |> should equal 1
    let yml = yml.Head

    [
        ("strip","# text")
        ("clip","# text\n")
        ("keep","# text\n")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2795596")>]
let ``Example 8.6. Empty Scalar Chomping``() =
    let yml = YamlParseList "
strip: >-

clip: >

keep: |+

"
    yml.Length |> should equal 1
    let yml = yml.Head

    [
        ("strip","")
        ("clip","")
        ("keep","\n")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2795789")>]
let ``Example 8.7. Literal Scalar``() =
    let yml = YamlParseList "
|
 literal
 \ttext

"
    yml.Length |> should equal 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#")
    yml |> pth.Select |> ToScalar |> should equal "literal\n\ttext\n"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2796118")>]
let ``Example 8.8. Literal Content``() =
    let yml = YamlParseList "
|
 
  
  literal
   
  
  text

 # Comment
"
    yml.Length |> should equal 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#")
    yml |> pth.Select |> ToScalar |> should equal "\n\nliteral\n \n\ntext\n"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2796371")>]
let ``Example 8.9. Folded Scalar``() =
    let yml = YamlParseList "
>
 folded
 text

"
    yml.Length |> should equal 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#")
    yml |> pth.Select |> ToScalar |> should equal "folded text\n"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2796543")>]
let ``Example 8.10. Folded Lines``() =
    let yml = YamlParseList "
>

 folded
 line

 next
 line
   * bullet

   * list
   * lines

 last
 line

# Comment"

    yml.Length |> should equal 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#")
    yml |> pth.Select |> ToScalar |> should equal "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n" 



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2796800")>]
let ``Example 8.11. More Indented Lines``() =
    let yml = YamlParseList "
>

 folded
 line

 next
 line
   * bullet

   * list
   * lines

 last
 line

# Comment
"

    yml.Length |> should equal 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#")
    yml |> pth.Select |> ToScalar |> should equal "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2797006")>]
let ``Example 8.12. Empty Separation Lines``() =
    let yml = YamlParseList "
>

 folded
 line

 next
 line
   * bullet

   * list
   * lines

 last
 line

# Comment
"

    yml.Length |> should equal 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#")
    yml |> pth.Select |> ToScalar |> should equal "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2797205")>]
let ``Example 8.13. Final Empty Lines``() =
    let yml = YamlParseList "
>
 folded
 line

 next
 line
   * bullet

   * list
   * lines

 last
 line

# Comment
"

    yml.Length |> should equal 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#")
    // note that the canonical formt in the example starts with \n, which is incorrect.
    yml |> pth.Select |> ToScalar |> should equal "folded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2797596")>]
let ``Example 8.14. Block Sequence``() =
    let yml = YamlParseList "
block sequence:
  - one
  - two : three
"
    yml.Length |> should equal 1
    let yml = yml.Head

    let p = YamlPath.Create (sprintf "//{#'block sequence'}?/[]/#" )
    yml |> p.Select |> ToScalar |> should equal "one"

    let p = YamlPath.Create (sprintf "//{#'block sequence'}?/[]/{#'two'}?" )
    yml |> p.Select |> ToScalar |> should equal "three"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2797944")>]
let ``Example 8.15. Block Sequence Entry Types``() =
    let yml = YamlParseList "
- # Empty
- |
 block node
- - one # Compact
  - two # sequence
- one: two # Compact mapping
"
    yml.Length |> should equal 1
    let yml = yml.Head

    let p = YamlPath.Create (sprintf "//[]/#'block node\n'" )
    yml |> p.Select |> ToScalar |> should equal "block node\n"

    let p = YamlPath.Create (sprintf "//[]/{#'one'}?" )
    yml |> p.Select |> ToScalar |> should equal "two"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2798147")>]
let ``Example 8.16. Block Mappings``() =
    let yml = YamlParseList "
block mapping:
 key: value
"
    yml.Length |> should equal 1
    let yml = yml.Head

    let p = YamlPath.Create (sprintf "//{#'block mapping'}?/{#'key'}?" )
    yml |> p.Select |> ToScalar |> should equal "value"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2798425")>]
let ``Example 8.17. Explicit Block Mapping Entries``() =
    let yml = YamlParseList "
? explicit key # Empty value
? |
  block key
: - one # Explicit compact
  - two # block value
"
    yml.Length |> should equal 1
    let yml = yml.Head

    let p = YamlPath.Create (sprintf "//{#'explicit key'}?" )
    yml |> p.Select |> ToScalar |> should equal ""

    let p = YamlPath.Create (sprintf "//{#'block key\n'}?/[]/#'two'" )
    yml |> p.Select |> ToScalar |> should equal "two"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2798896")>]
let ``Example 8.18. Implicit Block Mapping Entries``() =
    let yml = YamlParseList "
plain key: in-line value
: # Both empty
\"quoted key\":
- entry
"
    yml.Length |> should equal 1
    let yml = yml.Head

    let p = YamlPath.Create (sprintf "//{#'plain key'}?" )
    yml |> p.Select |> ToScalar |> should equal "in-line value"

    let p = YamlPath.Create (sprintf "//{#''}?" )
    yml |> p.Select |> ToScalar |> should equal ""

    let p = YamlPath.Create (sprintf "//{#'quoted key'}?/[]/#'entry'" )
    yml |> p.Select |> ToScalar |> should equal "entry"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2799091")>]
let ``Example 8.19. Compact Block Mappings``() =
    let yml = YamlParseList "
- sun: yellow
- ? earth: blue
  : moon: white
"
    yml.Length |> should equal 1
    let yml = yml.Head

    let p = YamlPath.Create (sprintf "//[]/{#'sun'}?" )
    yml |> p.Select |> ToScalar |> should equal "yellow"

    let p = YamlPath.Create (sprintf "//[]/{}/{#'earth'}?" )
    yml |> p.Select |> ToScalar |> should equal "blue"

    let p = YamlPath.Create (sprintf "//[]/{}?/{#'moon'}?" )
    yml |> p.Select |> ToScalar |> should equal "white"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2799426")>]
let ``Example 8.20. Block Node Types``() =
    let yml = YamlParseList "
-
  \"flow in block\"
- >
 Block scalar
- !!map # Block collection
  foo : bar
"
    yml.Length |> should equal 1
    let yml = yml.Head

    let p = YamlPath.Create (sprintf "//[]/#'flow in block'" )
    yml |> p.Select |> ToScalar |> should equal "flow in block"

    let p = YamlPath.Create (sprintf "//[]/#'Block scalar\n'" )
    yml |> p.Select |> ToScalar |> should equal "Block scalar\n"

    let p = YamlPath.Create (sprintf "//[]/{#'foo'}?" )
    yml |> p.Select |> ToScalar |> should equal "bar"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2799693")>]
let ``Example 8.21. Block Scalar Nodes``() =
    let yml = YamlParseList "
literal: |2
  value
folded:
   !foo
  >1
 value
"

    yml.Length |> should equal 1
    let yml = yml.Head

    [
        ("literal","value\n")   // in the example, the '\n' is missing 
        ("folded","value\n")    // in the example, the '\n' is missing 
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2799693")>]
let ``Example 8.22. Block Collection Nodes``() =
    let yml = YamlParseList "
sequence: !!seq
- entry
- !!seq
 - nested
mapping: !!map
 foo: bar
"
    yml.Length |> should equal 1
    let yml = yml.Head

    let p = YamlPath.Create (sprintf "//{#'sequence'}?/[]/#'entry'" )
    yml |> p.Select |> ToScalar |> should equal "entry"

    let p = YamlPath.Create (sprintf "//{#'sequence'}?/[]/[]/#'nested'" )
    yml |> p.Select |> ToScalar |> should equal "nested"

    let p = YamlPath.Create (sprintf "//{#'mapping'}?/{#'foo'}?" )
    yml |> p.Select |> ToScalar |> should equal "bar"

