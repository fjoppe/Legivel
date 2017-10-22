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

Yaml comes with a technical challenge; it supports structures which are
not suported in F# and C#. For example in Yaml you can create a list where each element is of a different type.
``List<obj>`` is seen as an undesired type - we wish static types, which are the main target for this library.

Legivel provides an F#-idiomatic Yaml to Native conversion. This simply means that
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

(**


# Option mapping

In the example below, an option is parsed. When a value is available, it is mapped to Some(data). 
If the value is absent, it is mapped to None.
*)
(*** define-output: optionexample ***)
type OptionExample = {
  opt1 : int option
  opt2 : int option
}

let yaml = "
opt1: 31
"

Deserialize<OptionExample> yaml
(**
Which results in:
*)
(*** include-it: optionexample ***)
(**


# Discriminated Union mapping

Discriminated unions can be compiled to a C# enum, or to an ordinary DU.
They can also be appear as a value in Yaml (plain style), or one key/value pair in a mapping
determines both Union Case and contained data (embedded style). You can also use an attribute
to customize the yaml-to-union-case mapping.

Below an example of plain-style yaml which maps to a enum-DU:
*)
(*** define-output: duenumexample ***)
type UnionCaseEnum =
    |   One=1
    |   [<YamlValue("two")>] Two=2
let yml = "two # alias"
Deserialize<UnionCaseEnum> yaml
(**
Which results in:
*)
(*** include-it: duenumexample ***)
(**

The following example demonstrates embedded style yaml which maps 
to a Union Case with record data:
*)
(*** define-output: duembeddedexample ***)
type SomeData = {
    Name : string
    Age  : int
}

[<YamlField("TypeOf")>]
type UnionCaseWithData =
    |   One of SomeData
    |   [<YamlValue("two")>] Two of SomeData

let yaml = "
    Name: 'Frank'
    Age:  43
    TypeOf : One
"

Deserialize<UnionCaseWithData> yaml
(**
Which results in:
*)
(*** include-it: duembeddedexample ***)
