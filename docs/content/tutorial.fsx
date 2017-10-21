(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I __SOURCE_DIRECTORY__
#I "../../bin/Legivel.Parser"
#I "../../bin/Legivel.Mapper"
#r "Legivel.Parser.dll"
#r "Legivel.Mapper.dll"
open Legivel.Serialization

(**
Legivel Tutorial
================

Yaml poses a technical challenge, in that it supports structures which are
unsuported in F# and C# - ie in Yaml you can create a list where each element has a different type.

Legivel provides an F#-idiomatic Yaml to Native conversion, simply meaning that
it does not support all Yaml structures. This tutorial demonstrates all supported 
mappings. Note that prerequisite statements as #I/#r/open are left out of these examples.

# Primitive mapping
Consider the folowing examples, which map a Yaml scalar to a primitive:
*)
Deserialize<int> "1"

Deserialize<float> "3.14"

Deserialize<bool> "True"


(**
# List mapping

In the examples below, an integer list is parsed. However you can use any supported type as list element.
*)
(*** define-output: l1 ***)
Deserialize<int list> "[ 1, 1, 2, 3, 5, 8, 13 ]"
(**
Which results in:
*)
(*** include-it: l1 ***)
(**
Yaml flow style:
*)
(*** define-output: l2 ***)
Deserialize<int list> "
- 1
- 2
- 3
"
(**
Which results in:
*)
(*** include-it: l2 ***)

(**
# Record mapping

In the example below, Yaml from [example 2.4](http://www.yaml.org/spec/1.2/spec.html#id2760193) is mapped to a record type. 
You can use attributes in the type definition, if the field name in Yaml is different than the record field name.
*)
(*** define-output: recordexample ***)
open Legivel.Attributes

type PlayerStats = {
    name    : string
    hr      : int
    [<YamlField("avg")>] average : float
}

let yaml = "
-
  name: Mark McGwire
  hr:   65
  avg:  0.278
-
  name: Sammy Sosa
  hr:   63
  avg:  0.288"

Deserialize<PlayerStats list> yaml

(**
Which results in:
*)
(*** include-it: recordexample ***)
