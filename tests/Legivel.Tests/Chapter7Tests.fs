module Chapter7Tests

(*
    Testing examples from chapter 7: http://www.yaml.org/spec/1.2/spec.html#Flow
*)

open NUnit.Framework
open FsUnitTyped
open TestUtils
open Legivel.Traverse


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2778101")>]
let ``Example 7.1. Alias Nodes``() =
    let yml = YamlParseList "
First occurrence: &anchor Foo
Second occurrence: *anchor
Override anchor: &anchor Bar
Reuse anchor: *anchor
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("First occurrence", "Foo")
        ("Second occurrence", "Foo")
        ("Override anchor", "Bar")
        ("Reuse anchor", "Bar")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2786720")>]
let ``Example 7.2. Empty Content``() =
    let yml = YamlParseList "
{
  foo : !!str,
  !!str : bar,
}
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("foo", "")
        ("", "bar")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2786868")>]
let ``Example 7.3. Completely Empty Flow Nodes``() =
    let yml = YamlParseList "
{
  ? foo :,
  : bar,
}
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("foo", "")
        ("", "bar")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2787420")>]
let ``Example 7.4. Double Quoted Implicit Keys``() =
    let yml = YamlParseList "
\"implicit block key\" : [
  \"implicit flow key\" : value,
 ]
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//{#'implicit block key'}?/[]/{#'implicit flow key'}?")
    yml |> pth.Select |> ToScalar |> shouldEqual "value"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2787745")>]
let ``Example 7.5. Double Quoted Line Breaks``() =
    let yml = YamlParseList "
\"folded 
to a space,\t
 
to a line feed, or \t\\
 \\ \tnon-content\"
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#")
    yml |> pth.Select |> ToScalar |> shouldEqual "folded to a space,\nto a line feed, or \t \tnon-content"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2787994")>]
let ``Example 7.6. Double Quoted Lines``() =
    let yml = YamlParseList "
\" 1st non-empty

 2nd non-empty 
 \t3rd non-empty \"
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#")
    yml |> pth.Select |> ToScalar |> shouldEqual " 1st non-empty\n2nd non-empty 3rd non-empty "


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2788307")>]
let ``Example 7.7. Single Quoted Characters``() =
    let yml = YamlParseList "
'here''s to \"quotes\"'
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#")
    yml |> pth.Select |> ToScalar |> shouldEqual "here's to \"quotes\""


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2787420")>]
let ``Example 7.8. Single Quoted Implicit Keys``() =
    let yml = YamlParseList "
'implicit block key' : [
  'implicit flow key' : value,
 ]
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//{#'implicit block key'}?/[]/{#'implicit flow key'}?")
    yml |> pth.Select |> ToScalar |> shouldEqual "value"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2788756")>]
let ``Example 7.9. Single Quoted Lines``() =
    let yml = YamlParseList "
' 1st non-empty

 2nd non-empty 
 \t3rd non-empty '
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#")
    yml |> pth.Select |> ToScalar |> shouldEqual " 1st non-empty\n2nd non-empty 3rd non-empty "



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2789510")>]
let ``Example 7.10. Plain Characters``() =
    let yml = YamlParseList "
# Outside flow collection:
- ::vector
- \": - ()\"
- Up, up, and away!
- -123
- http://example.com/foo#bar
# Inside flow collection:
- [ ::vector,
  \": - ()\",
  \"Up, up and away!\",
  -123,
  http://example.com/foo#bar ]
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    ["::vector" ; ": - ()"; "-123"; "http://example.com/foo#bar"]
    |> List.iter(fun (v) ->
        let p1 = YamlPath.Create (sprintf "//[]/#'%s'" v)
        let p2 = YamlPath.Create (sprintf "//[]/[]/#'%s'" v)
        yml |> p1.Select |> ToScalar |> shouldEqual v
        yml |> p2.Select |> ToScalar |> shouldEqual v
    )

    //  trip over a single comma..
    let p1 = YamlPath.Create (sprintf "//[]/#'%s'" "Up, up, and away!")
    let p2 = YamlPath.Create (sprintf "//[]/[]/#'%s'" "Up, up and away!")
    yml |> p1.Select |> ToScalar |> shouldEqual "Up, up, and away!"
    yml |> p2.Select |> ToScalar |> shouldEqual "Up, up and away!"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2789794")>]
let ``Example 7.11. Plain Implicit Keys``() =
    let yml = YamlParseList "
implicit block key : [
  implicit flow key : value,
 ]
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//{#'implicit block key'}?/[]/{#'implicit flow key'}?")
    yml |> pth.Select |> ToScalar |> shouldEqual "value"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2789986")>]
let ``Example 7.12. Plain Lines``() =
    let yml = YamlParseList "
1st non-empty

 2nd non-empty 
\t3rd non-empty
"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    let pth = YamlPath.Create (sprintf "//#")
    yml |> pth.Select |> ToScalar |> shouldEqual "1st non-empty\n2nd non-empty 3rd non-empty"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2790506")>]
let ``Example 7.13. Flow Sequence``() =
    let yml = YamlParseList "
- [ one, two, ]
- [three ,four]
 "
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        "one";"two"; "three"; "four"
    ]
    |> List.iter(fun v ->
        let p = YamlPath.Create (sprintf "//[]/[]/#'%s'" v)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2790726")>]
let ``Example 7.14. Flow Sequence Entries``() =
    let yml = YamlParseList "
[
\"double
 quoted\", 'single
           quoted',
plain
 text, [ nested ],
single: pair,
]
 "
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        "double quoted";"single quoted"; "plain text"
    ]
    |> List.iter(fun v ->
        let p = YamlPath.Create (sprintf "//[]/#'%s'" v)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )

    let p = YamlPath.Create (sprintf "//[]/[]/#'nested'")
    yml |> p.Select |> ToScalar |> shouldEqual "nested"

    let p = YamlPath.Create (sprintf "//[]/{#'single'}?")
    yml |> p.Select |> ToScalar |> shouldEqual "pair"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2791018")>]
let ``Example 7.15. Flow Mappings``() =
    let yml = YamlParseList "
- { one : two , three: four , }
- {five: six,seven : eight}
 "
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("one","two"); ("three", "four")
        ("five","six"); ("seven", "eight")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//[]/{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2791260")>]
let ``Example 7.16. Flow Mapping Entries``() =
    let yml = YamlParseList "
{
? explicit: entry,
implicit: entry,
?
}
 "
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("explicit","entry")
        ("implicit", "entry")
        ("","")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2791704")>]
let ``Example 7.17. Flow Mapping Separate Values``() =
    let yml = YamlParseList "
{
unquoted : \"separate\",
http://foo.com,
omitted value:,
: omitted key,
}
 "
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("unquoted","separate")
        ("http://foo.com", "")
        ("omitted value","")
        ("","omitted key")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2792073")>]
let ``Example 7.18. Flow Mapping Adjacent Values``() =
    let yml = YamlParseList "
{
\"adjacent\":value,
\"readable\":value,
\"empty\":
}
 "
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("adjacent","value")
        ("readable","value")
        ("empty","")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2792291")>]
let ``Example 7.19. Single Pair Flow Mappings``() =
    let yml = YamlParseList "
[
foo: bar
]
 "
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("foo","bar")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//[]/{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2792424")>]
let ``Example 7.20. Single Pair Explicit Entry``() =
    let yml = YamlParseList "
[
? foo
 bar : baz
]
 "
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("foo bar","baz")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//[]/{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2792785")>]
let ``Example 7.21. Single Pair Implicit Entries``() =
    let yml = YamlParseList "
- [ YAML : separate ]
- [ : empty key entry ]
- [ {JSON: like}:adjacent ]
 "
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("YAML","separate")
        ("","empty key entry")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//[]/[]/{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )
    let p = YamlPath.Create (sprintf "//[]/[]/{}?/#'adjacent'" )
    yml |> p.Select |> ToScalar |> shouldEqual "adjacent"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2792902")>]
let ``Example 7.22. Invalid Implicit Keys``() =
    failwith "Implement continuations for this test"

    let plus1Kchars = [0 .. 1025] |> List.fold(fun s e -> s+"A") ""

    let input = "[ foo\n bar: invalid ]" 
    let err = YamlParseWithErrors input
    err.Error.Length |> shouldBeGreaterThan 0
    err.Error |> List.filter(fun m ->m.Message.StartsWith("This plain scalar cannot span multiple lines")) |> List.length  |> shouldEqual 1

    let input = sprintf "[ \"foo...%s...bar\": invalid ]" plus1Kchars
    let err = YamlParseWithErrors input
    err.Error.Length |> shouldBeGreaterThan 0
    err.Error |> List.filter(fun m ->m.Message.StartsWith("The mapping key is too long.")) |> List.length  |> shouldEqual 1


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2793163")>]
let ``Example 7.23. Flow Content``() =
    let yml = YamlParseList "
- [ a, b ]
- { a: b }
- \"a\"
- 'b'
- c
 "
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("[]/#'b'","b")
        ("{#'a'}?","b")
        ("#'a'","a")
        ("#'b'","b")
        ("#'c'","c")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//[]/%s" k)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2793490")>]
let ``Example 7.24. Flow Nodes``() =
    let yml = YamlParseList "
- !!str \"a\"
- 'b'
- &anchor \"c\"
- *anchor
- !!str"
    yml.Length |> shouldEqual 1
    let yml = yml.Head

    [
        ("a")
        ("b")
    ]
    |> List.iter(fun (v) ->
        let p = YamlPath.Create (sprintf "//[]/#'%s'" v)
        yml |> p.Select |> ToScalar |> shouldEqual v
    )

    let p = YamlPath.Create (sprintf "//[]/#'c'")
    yml |> p.Select |> Option.map(fun v -> v.Length |>shouldEqual 2) |> Option.isSome |> shouldEqual true
