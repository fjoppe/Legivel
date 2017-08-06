module Chapter2Tests

(*
    Testing examples from chapter 2: http://www.yaml.org/spec/1.2/spec.html#Preview
*)

open NUnit.Framework
open FsUnit
open TestUtils
open YamlParser
open TagResolution

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760118")>]
let ``Example 2.1.  Sequence of Scalars``() =
    let yml = YamlParse "
- Mark McGwire
- Sammy Sosa
- Ken Griffey"
    ["Mark McGwire";"Sammy Sosa"; "Ken Griffey"]
    |> List.iter(fun s-> 
        let pth = YamlPath.Create (sprintf "//[]/#'%s'" s)
        yml |> pth.Select |> ToScalar |> should equal s
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760142")>]
let ``Example 2.2.  Mapping Scalars to Scalars``() =
    let yml = YamlParse "
hr:  65    # Home runs
avg: 0.278 # Batting average
rbi: 147   # Runs Batted In"

    [("hr", "65");("avg","0.278"); ("rbi","147")]
    |> List.iter(fun (k,v) -> 
        let pth = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> pth.Select |> ToScalar |> should equal v
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760167")>]
let ``Example 2.3.  Mapping Scalars to Sequences``() =
    let yml = YamlParse "
american:
  - Boston Red Sox
  - Detroit Tigers
  - New York Yankees
national:
  - New York Mets
  - Chicago Cubs
  - Atlanta Braves"

    [
    ("american","Boston Red Sox");("american","Detroit Tigers"); ("american","New York Yankees")
    ("national","New York Mets");("national","Chicago Cubs"); ("national","Atlanta Braves")
    ]
    |> List.iter(fun (k,v) -> 
        let pth = YamlPath.Create (sprintf "//{#'%s'}?/[]/#'%s'" k v)
        yml |> pth.Select |> ToScalar |> should equal v
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760193")>]
let ``Example 2.4.  Sequence of Mappings``() =
    let yml = YamlParse "
-
  name: Mark McGwire
  hr:   65
  avg:  0.278
-
  name: Sammy Sosa
  hr:   63
  avg:  0.288"

    [
    ("name","Mark McGwire"); ("hr","65"); ("avg","0.278")
    ("name","Sammy Sosa");("hr","63"); ("avg","0.288")
    ]
    |> List.iter(fun (k,v) -> 
        let ypath = (sprintf "//[]/{#'%s'}?/#'%s'" k v)
        let pth = YamlPath.Create ypath
        yml |> pth.Select |> ToScalar |> should equal v
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760351")>]
let ``Example 2.5. Sequence of Sequences``() =
    let yml = YamlParse "
- [name        , hr, avg  ]
- [Mark McGwire, 65, 0.278]
- [Sammy Sosa  , 63, 0.288]"

    [
    "name"; "hr"; "avg"
    "Mark McGwire"; "65"; "0.278"
    "Sammy Sosa"; "63"; "0.288"
    ]
    |> List.iter(fun s -> 
        let ypath = (sprintf "//[]/[]/#'%s'" s)
        let pth = YamlPath.Create ypath
        yml |> pth.Select |> ToScalar |> should equal s
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760372")>]
let ``Example 2.6. Mapping of Mappings``() =
    let yml = YamlParse "
Mark McGwire: {hr: 65, avg: 0.278}
Sammy Sosa: {
    hr: 63,
    avg: 0.288
  }"

    [
    ("Mark McGwire", "hr", "65");   ("Mark McGwire", "avg", "0.278")
    ("Sammy Sosa", "hr", "63");     ("Sammy Sosa", "avg", "0.288")
    ]
    |> List.iter(fun (n,p,v) -> 
        let ypath = (sprintf "//{#'%s'}?/{#'%s'}?" n p)
        let pth = YamlPath.Create ypath
        yml |> pth.Select |> ToScalar |> should equal v
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760493")>]
let ``Example 2.7. Two Documents in a Stream (each with a leading comment)``() =
    let yml = YamlParseList "
# Ranking of 1998 home runs
---
- Mark McGwire
- Sammy Sosa
- Ken Griffey

# Team ranking
---
- Chicago Cubs
- St Louis Cardinals
"
    yml.Length |> should equal 2

    let yml1, yml2 = List.head yml, List.last yml 

    ["Mark McGwire"; "Sammy Sosa"; "Ken Griffey"]
    |> List.iter(fun e -> 
        let ypath = (sprintf "//[]/#'%s'" e)
        let pth = YamlPath.Create ypath
        yml1 |> pth.Select |> ToScalar |> should equal e
    )
    
    ["Chicago Cubs"; "St Louis Cardinals"]
    |> List.iter(fun e -> 
        let ypath = (sprintf "//[]/#'%s'" e)
        let pth = YamlPath.Create ypath
        yml2 |> pth.Select |> ToScalar |> should equal e
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760519")>]
let ``Example 2.8.  Play by Play Feed from a Game``() =
    let yml = YamlParseList "
---
time: 20:03:20
player: Sammy Sosa
action: strike (miss)
...
---
time: 20:03:47
player: Sammy Sosa
action: grand slam
...
"
    yml.Length |> should equal 2

    let yml1, yml2 = List.head yml, List.last yml 


    [("time", "20:03:20"); ("player", "Sammy Sosa");("action", "strike (miss)")]
    |> List.iter(fun (k,v) -> 
        let ypath = (sprintf "//{#'%s'}?" k)
        let pth = YamlPath.Create ypath
        yml1 |> pth.Select |> ToScalar |> should equal v
    )

    [("time", "20:03:47"); ("player", "Sammy Sosa");("action", "grand slam")]
    |> List.iter(fun (k,v) -> 
        let ypath = (sprintf "//{#'%s'}?" k)
        let pth = YamlPath.Create ypath
        yml2 |> pth.Select |> ToScalar |> should equal v
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760633")>]
let ``Example 2.9.  Single Document with Two Comments``() =
    let yml = YamlParseList "
---
hr: # 1998 hr ranking
  - Mark McGwire
  - Sammy Sosa
rbi:
  # 1998 rbi ranking
  - Sammy Sosa
  - Ken Griffey
"
    yml.Length |> should equal 1

    [("hr", "Mark McGwire", 2); ("rbi", "Sammy Sosa", 2)]
    |> List.iter(fun (k,v,c) -> 
        let p1 = YamlPath.Create (sprintf "//{#'%s'}?" k)
        let p2 = YamlPath.Create (sprintf "//{#'%s'}?/[]/#'%s'" k v)

        yml.Head |> p1.Select |> ToSequence |> fun s -> s.Length |> should equal c
        yml.Head |> p2.Select |> ToScalar |> should equal v
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760658")>]
let ``Example 2.10.  Node for “Sammy Sosa” appears twice in this document``() =
    let yml = YamlParseList "
---
hr:
  - Mark McGwire
  # Following node labeled SS
  - &SS Sammy Sosa
rbi:
  - *SS # Subsequent occurrence
  - Ken Griffey
"
    yml.Length |> should equal 1

    [("hr", "Sammy Sosa", 2); ("rbi", "Sammy Sosa", 2)]
    |> List.iter(fun (k,v,c) -> 
        let p1 = YamlPath.Create (sprintf "//{#'%s'}?" k)
        let p2 = YamlPath.Create (sprintf "//{#'%s'}?/[]/#'%s'" k v)

        yml.Head |> p1.Select |> ToSequence |> fun s -> s.Length |> should equal c
        yml.Head |> p2.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2760799")>]
let ``Example 2.11. Mapping between Sequences``() =
    let yml = YamlParseList "
? - Detroit Tigers
  - Chicago cubs
:
  - 2001-07-23

? [ New York Yankees,
    Atlanta Braves ]
: [ 2001-07-02, 2001-08-12,
    2001-08-14 ]
"
    yml.Length |> should equal 1

    //  TODO: extend YamlPath to query above structure
    let p1 = YamlPath.Create "//{}/[]/#'Detroit Tigers'"
    let p2 = YamlPath.Create "//{}?/[]/#'2001-07-02'"

    yml.Head |> p1.Select |> ToScalar |> should equal "Detroit Tigers"
    yml.Head |> p2.Select |> ToScalar |> should equal "2001-07-02"

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761008")>]
let ``Example 2.13.  In literals, newlines are preserved``() =
    let yml = YamlParse "
# ASCII Art
--- |
  \//||\/||
  // ||  ||__"

    Some([yml]) |> ExtractTag |> should equal TagResolution.Failsafe.StringGlobalTag.Uri

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761032")>]
let ``Example 2.14.  In the folded scalars, newlines become spaces``() =
    let yml = YamlParse "
--- >
  Mark McGwire's
  year was crippled
  by a knee injury."

    Some([yml]) |> ExtractTag |> should equal TagResolution.Failsafe.StringGlobalTag.Uri
    Some([yml]) |> ToScalar |> should equal "Mark McGwire's year was crippled by a knee injury."


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761056")>]
let ``Example 2.15.  Folded newlines are preserved for "more indented" and blank lines``() =
    let yml = YamlParse "
>
 Sammy Sosa completed another
 fine season with great stats.

   63 Home Runs
   0.288 Batting Average

 What a year!"

    Some([yml]) |> ExtractTag |> should equal TagResolution.Failsafe.StringGlobalTag.Uri
    Some([yml]) |> ToScalar |> should equal "Sammy Sosa completed another fine season with great stats.\n\n  63 Home Runs\n  0.288 Batting Average\n\nWhat a year!"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761056")>]
let ``Example 2.16.  Indentation determines scope``() =
    let yml = YamlParse "
name: Mark McGwire
accomplishment: >
  Mark set a major league
  home run record in 1998.
stats: |
  65 Home Runs
  0.278 Batting Average"

    [
        ("name", "Mark McGwire")
        ("accomplishment", "Mark set a major league home run record in 1998.\n") // TODO: not sure about this last \n
        ("stats", "65 Home Runs\n0.278 Batting Average")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761245")>]
let ``Example 2.17. Quoted Scalars``() =
    let yml = YamlParse "
unicode: \"Sosa did fine.\\u263A\"
control: \"\\b1998\\t1999\\t2000\\n\"
hex esc: \"\\x0d\\x0a is \\r\\n\"

single: '\"Howdy!\" he cried.'
quoted: ' # Not a ''comment''.'
tie-fighter: '|\\-*-/|'
"
    [
        ("unicode", "Sosa did fine.\u263A")
        ("control", "\b1998\t1999\t2000\n") 
        ("hex esc", "\n is \n")
        ("single", "\"Howdy!\" he cried.")
        ("quoted", " # Not a 'comment'.")
        ("tie-fighter", "|\\-*-/|")
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761268")>]
let ``Example 2.18. Multi-line Flow Scalars``() =
    let yml = YamlParse "
plain:
  This unquoted scalar
  spans many lines.

quoted: \"So does this
  quoted scalar.\\n\"
"
    [
        ("plain", "This unquoted scalar spans many lines.")
        ("quoted", "So does this quoted scalar.\n") 
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761509")>]
let ``Example 2.19. Integers``() =
    let yml = YamlParse "
canonical: 12345
decimal: +12345
octal: 0o14
hexadecimal: 0xC
"
    [
        ("canonical", "12345")
        ("decimal", "+12345") 
        ("octal", "0o14") 
        ("hexadecimal", "0xC") 
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761530")>]
let ``Example 2.20. Floating Point``() =
    let yml = YamlParse "
canonical: 1.23015e+3
exponential: 12.3015e+02
fixed: 1230.15
negative infinity: -.inf
not a number: .NaN
"
    [
        ("canonical", "1.23015e+3")
        ("exponential", "12.3015e+02") 
        ("fixed", "1230.15") 
        ("negative infinity", "-.inf") 
        ("not a number", ".NaN") 
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761552")>]
let ``Example 2.21. Miscellaneous``() =
    let yml = YamlParse "
null:
booleans: [ true, false ]
string: '012345'
"
    [
        ("null", "")
        ("string", "012345") 
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )
    let p = YamlPath.Create "//{#'booleans'}?/[]/#'true'"
    yml |> p.Select |> ToScalar |> should equal "true"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761573")>]
let ``Example 2.22. Timestamps``() =
    let yml = YamlParse "
canonical: 2001-12-15T02:59:43.1Z
iso8601: 2001-12-14t21:59:43.10-05:00
spaced: 2001-12-14 21:59:43.10 -5
date: 2002-12-14
"
    [
        ("canonical", "2001-12-15T02:59:43.1Z")
        ("iso8601", "2001-12-14t21:59:43.10-05:00") 
        ("spaced", "2001-12-14 21:59:43.10 -5") 
        ("date", "2002-12-14") 
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )

[<Ignore "TODO: Implement real binary">]
[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761694")>]
let ``Example 2.23. Various Explicit Tags``() =
    let yml = YamlParse "
---
not-date: !!str 2002-04-28

picture: !!binary |
 R0lGODlhDAAMAIQAAP//9/X
 17unp5WZmZgAAAOfn515eXv
 Pz7Y6OjuDg4J+fn5OTk6enp
 56enmleECcgggoBADs=

application specific tag: !something |
 The semantics of the tag
 above may be different for
 different documents.
"
    [
        ("not-date", "2002-04-28", "tag:yaml.org,2002:str")
        // TODO: "picture" needs to become real binary
        ("picture", "R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5OTk6enp56enmleECcgggoBADs=\n", "tag:yaml.org,2002:binary")
        ("application specific tag", "The semantics of the tag\nabove may be different for\ndifferent documents.\n","!something") 
    ]
    |> List.iter(fun (k,v,t) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
        yml |> p.Select |> ExtractTag |> should equal t
    )

[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761719")>]
let ``Example 2.24. Global Tags``() =
    let yml = YamlParse "
%TAG ! tag:clarkevans.com,2002:
--- !shape
  # Use the ! handle for presenting
  # tag:clarkevans.com,2002:circle
- !circle
  center: &ORIGIN {x: 73, y: 129}
  radius: 7
- !line
  start: *ORIGIN
  finish: { x: 89, y: 102 }
- !label
  start: *ORIGIN
  color: 0xFFEEBB
  text: Pretty vector drawing.
"
    [
        ("//[]/<tag:clarkevans.com,2002:line>/{#'start'}?/{#'x'}?","73")
        ("//[]/<tag:clarkevans.com,2002:line>/{#'start'}?/{#'y'}?","129")
        ("//[]/<tag:clarkevans.com,2002:line>/{#'finish'}?/{#'x'}?","89")
        ("//[]/<tag:clarkevans.com,2002:line>/{#'finish'}?/{#'y'}?","102")

        ("//[]/<tag:clarkevans.com,2002:label>/{#'start'}?/{#'x'}?","73")
        ("//[]/<tag:clarkevans.com,2002:label>/{#'start'}?/{#'y'}?","129")
        ("//[]/<tag:clarkevans.com,2002:label>/{#'color'}?","0xFFEEBB")
        ("//[]/<tag:clarkevans.com,2002:label>/{#'text'}?","Pretty vector drawing.")

        ("//[]/<tag:clarkevans.com,2002:circle>/{#'center'}?/{#'x'}?","73")
        ("//[]/<tag:clarkevans.com,2002:circle>/{#'center'}?/{#'y'}?","129")
        ("//[]/<tag:clarkevans.com,2002:circle>/{#'radius'}?","7")
    ]
    |> List.iter(fun (pt,v) ->
        let p = YamlPath.Create (pt)
        yml |> p.Select |> ToScalar |> should equal v
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761758")>]
let ``Example 2.25. Unordered Sets``() =
    let yml = YamlParseForSchema YamlExtendedSchema "
# Sets are represented as a
# Mapping where each key is
# associated with a null value
--- !!set
? Mark McGwire
? Sammy Sosa
? Ken Griff
"
    [
        ("Mark McGwire", "")
        ("Sammy Sosa", "")
        ("Ken Griff", "") 
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )
    Some([yml]) |> ExtractTag |> should equal "tag:yaml.org,2002:set"



[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761780")>]
let ``Example 2.26. Ordered Mappings``() =
    let yml = YamlParseForSchema YamlExtendedSchema  "
# Ordered maps are represented as
# A sequence of mappings, with
# each mapping having one key
--- !!omap
- Mark McGwire: 65
- Sammy Sosa: 63
- Ken Griffy: 58
"
    [
        ("Mark McGwire", "65")
        ("Sammy Sosa", "63")
        ("Ken Griffy", "58") 
    ]
    |> List.iter(fun (k,v) ->
        let p = YamlPath.Create (sprintf "//[]/{#'%s'}?" k)
        yml |> p.Select |> ToScalar |> should equal v
    )
    Some([yml]) |> ExtractTag |> should equal "tag:yaml.org,2002:omap"


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761823")>]
let ``Example 2.27. Invoice``() =
    let yml = YamlParse "
--- !<tag:clarkevans.com,2002:invoice>
invoice: 34843
date   : 2001-01-23
bill-to: &id001
    given  : Chris
    family : Dumars
    address:
        lines: |
            458 Walkman Dr.
            Suite #292
        city    : Royal Oak
        state   : MI
        postal  : 48046
ship-to: *id001
product:
    - sku         : BL394D
      quantity    : 4
      description : Basketball
      price       : 450.00
    - sku         : BL4438H
      quantity    : 1
      description : Super Hoop
      price       : 2392.00
tax  : 251.42
total: 4443.52
comments:
    Late afternoon is best.
    Backup contact is Nancy
    Billsmer @ 338-4338.
"
    [
        ("invoice", "34843")
        ("tax", "251.42")
        ("total", "4443.52")
        ("comments", "Late afternoon is best. Backup contact is Nancy Billsmer @ 338-4338.")
        ("bill-to/given", "Chris")
        ("bill-to/family", "Dumars")
        ("bill-to/address/lines", "458 Walkman Dr.\nSuite #292\n")
        ("bill-to/address/city", "Royal Oak")
        ("bill-to/address/state", "MI")
        ("bill-to/address/postal", "48046")
        ("ship-to/given", "Chris")
        ("ship-to/family", "Dumars")
        ("ship-to/address/lines", "458 Walkman Dr.\nSuite #292\n")
        ("ship-to/address/city", "Royal Oak")
        ("ship-to/address/state", "MI")
        ("ship-to/address/postal", "48046")
    ]
    |> List.iter(fun (k,v) ->
        let kp = k.Split([|'/'|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
        let p = kp |> List.fold(fun s i -> sprintf "%s/{#'%s'}?" s i) "/" |> YamlPath.Create
        yml |> p.Select |> ToScalar |> should equal v
    )

    let prdpt =  YamlPath.Create "//{#'product'}?/[]"
    let prdNodes = yml |> prdpt.Select |> Option.get

    [
        [
            ("sku", "BL394D")
            ("quantity", "4")
            ("description", "Basketball")
            ("price", "450.00")
        ]
        [
            ("sku", "BL4438H")
            ("quantity", "1")
            ("description", "Super Hoop")
            ("price", "2392.00")
        ]
    ]
    |>  List.zip prdNodes
    |>  List.iter(fun (e,a) -> 
        a
        |> List.iter(fun (k,v) ->
            let p = (sprintf "//{#'%s'}?" k) |> YamlPath.Create
            e |> p.Select |> ToScalar |> should equal v
        )
    )


[<Test(Description="http://www.yaml.org/spec/1.2/spec.html#id2761866")>]
let ``Example 2.28. Log File``() =
    let yml = YamlParseList "
---
Time: 2001-11-23 15:01:42 -5
User: ed
Warning:
  This is an error message
  for the log file
---
Time: 2001-11-23 15:02:31 -5
User: ed
Warning:
  A slightly different error
  message.
---
Date: 2001-11-23 15:03:17 -5
User: ed
Fatal:
  Unknown variable \"bar\"
Stack:
  - file: TopClass.py
    line: 23
    code: |
      x = MoreObject(\"345\\n\")
  - file: MoreClass.py
    line: 58
    code: |-
      foo = bar
"
    yml.Length |> should equal 3

    yml
    |> List.iter(fun (y) ->
        let p = YamlPath.Create (sprintf "//{#'User'}?")
        y |> p.Select |> ToScalar |> should equal "ed"
    )

    yml
    |> List.zip [("Time","2001-11-23 15:01:42 -5"); ("Time","2001-11-23 15:02:31 -5");("Date","2001-11-23 15:03:17 -5")]
    |> List.iter(fun ((k,v),y) ->
        let p = YamlPath.Create (sprintf "//{#'%s'}?" k)
        y |> p.Select |> ToScalar |> should equal v
    )

