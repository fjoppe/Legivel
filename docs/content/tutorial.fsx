(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#I "../../bin/Legivel.Parser"
#I "../../bin/Legivel.Mapper"


(**
Legivel 
============

Legivel, parses Yaml strings into a generic ```Node``` structure.
The ```Node``` structure is a binary representation of the oiginal Yaml document,
however is generic enough to support all possible Yaml constructs.

Yaml structures are incompatible with type constraints in F# and C#. So a ```Node```
 structure cannot be easily translated into native types.

See the following example:
*)
#r "Legivel.Parser.dll"
open Legivel.Parser
open Legivel.TagResolution

let engine = Yaml12Parser()
let YamlParse s = (engine.``l-yaml-stream`` YamlExtended.Schema s)

let yaml = "{ a: 2, b: [9, 6, 3] }" 

YamlParse yaml

(**
This Yaml structure cannot be translated easily into an F# or C# type, 
because the mapping values are of heterogenious types. Yaml offers even
more complex structures.

As shown in the Yaml [spec](http://www.yaml.org/spec/1.2/spec.html#id2767381), this parser covers all 
but the last step, "Construct Native Data" (if we don't count the generic as native).

To make the final step, and translate the generic Node structure into a record, or ```int list```
an "opiniated" Yaml to Native Constructor is required, which makes non-generic choices.

Non-generic choices are for example, to not support all Yaml constructs, to make special translations
for specific cases. In that respect, everyone should be able to write their own
Yaml To Native Constructor, based on the output of the FsYamlParser, and make their own choices
when no sufficient solution exists. With this thought, these concerns were seperated in the design.

*)
