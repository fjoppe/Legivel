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



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2793888")>]
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
        let p = YamlPath.Create (sprintf "//[]/{#'%s'}?" k)
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
        let p = YamlPath.Create (sprintf "//[]/{#'%s'}?" k)
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
        let p = YamlPath.Create (sprintf "//[]/{#'%s'}?" k)
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

