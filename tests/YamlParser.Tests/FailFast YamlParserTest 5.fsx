#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#r @"bin/Debug/YamlParser.dll"
#r @"NLog/lib/net45/NLog.dll"

open YamlParse
open TagResolution
open Deserialization
open NLog

#load "nlog.fsx"
NlogInit.With __SOURCE_DIRECTORY__ __SOURCE_FILE__

let logger = LogManager.GetLogger("*")

let engine = Yaml12Parser(fun s -> logger.Trace(s))


let YamlParse s =
    try
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Value
        let (nodes, ps) = pr
        let node = nodes.Head
        printfn "%s" (Deserialize node (ps.TagShorthands))
        ps.Messages |> List.iter(fun i -> 
            match i with
            |   Error t -> printfn "ERROR: %s" t
            |   Warn  t -> printfn "Warning: %s" t
        )
    with
    | e -> printfn "%A" e

let YamlParseList s =
    try
        let pr = (engine.``l-yaml-stream`` YamlCoreSchema s).Value
        let (nodes, ps) = pr
        nodes |> List.iter(fun node -> printfn "%s\n---" (Deserialize node (ps.TagShorthands)))
        nodes
    with
    | e -> printfn "%A" e; raise e


YamlParse "a:\n key: value\n"

YamlParse "plain key: in-line value\n: # Both empty\n\"quoted key\":- entry"

YamlParseList "
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


YamlParse "
---
hr:
  - Mark McGwire
  # Following node labeled SS
  - &SS Sammy Sosa
rbi:
  - *SS # Subsequent occurrence
  - Ken Griffey
"

YamlParse  "block sequence:\n  - one\n  - two : three\n"

YamlParse "
? - Detroit Tigers
  - Chicago cubs
:
  - 2001-07-23
? [ New York Yankees,
    Atlanta Braves ]
: [ 2001-07-02, 2001-08-12,
    2001-08-14 ]
"

YamlParse "
# ASCII Art
--- |
  \//||\/||
  // ||  ||__"

YamlParse "
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

YamlParse "
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

YamlParse "
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

YamlParse "
sequence:
- one
- two
mapping:
  ? sky
  : blue
  sea : green
"

//YamlParse "
//anchored: !local &anchor value
//alias: *anchor
//"
YamlParse "
commercial-at: @text
grave-accent: `text
"

YamlParse "
!!str &a1 \"foo\":
  !!str bar
&a2 baz : *a1
"

YamlParse "
{
  foo : !!str,
  !!str : bar,
}
"

YamlParse "
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


YamlParse "
- { one : two , three: four , }
- {five: six,seven : eight}
"
  

YamlParse "
{
unquoted : \"separate\",
http://foo.com,
omitted value:,
: omitted key,
}
"

