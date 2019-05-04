(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I __SOURCE_DIRECTORY__
#I "../../bin/Legivel.Mapper/netstandard2.0"
#r "Legivel.Parser.dll"
#r "Legivel.Mapper.dll"

(**
Legivel Features
================

Legivel is split into two parts:

*  ``Legivel.Parser``: parses any Yaml-1.2, into a generic native structure (representation graph)
*  ``Legivel.Mapper``: maps the output of ``Legivel.Parser`` to specific native types.

#   Legivel.Parser

The ``Legivel.Parser`` can parse any Yaml1.2 string to a native format - this includes any Yaml construct which
may not be supported by the F# or C# languages. The yaml string may contain any number of yaml documents, 
as demonstrated in [example 2.8](http://www.yaml.org/spec/1.2/spec.html#id2761866).

The parser implements all [recommended schema's](http://www.yaml.org/spec/1.2/spec.html#Schema). 
The (not official name) ``YamlExtended`` schema implements all tags from the [Yaml Tag Repository](http://yaml.org/type/)
except [!!yaml](http://yaml.org/type/yaml.html).

The parser is found in ``Legivel.Parser``, and you can invoke it with any of the given Yaml schema's, found in ``Legivel.TagResolution``, or a customized one.
*)

open Legivel.Parser
open Legivel.TagResolution

let YamlParse s =
    let parser = Yaml12Parser(YamlExtended.Schema)
    let repr = (parser.``l-yaml-stream`` s)
    repr.Head

(**
Note, even though it is trivial to inject your own schema, it is not trivial to create your own schema. 
Please refer to the TagResolution source code to find your example. Also read the Yaml specification
[here](http://www.yaml.org/spec/1.2/spec.html#id2764295) and [here](http://www.yaml.org/spec/1.2/spec.html#id2768011),
as the Legivel parser follows these specifications.

The source code in the ``Legivel.Parser`` is directly related to the Yaml rules from the specification. 
The function name of each rule implementation has the same name as in the specification. So you can copy/paste and 
use a Find function in the code, or on the spec-page. All spec-examples have been implemented as a unit test.


#   Legivel.Mapper

The ``Legivel.Mapper`` takes the ``Legivel.Parser``'s output and maps it to native types, as sofar is supported.
As Yaml has no contract or metadata facility, the mapper requires a target native type, which is considered as
contract for the yaml conversion. 

Conversion happens in a contract-first fashion; ie the mapper tries to search the input yaml for contract requirements.
Any extra yaml, foreign to the contract (target type), is ignored.

The Legivel mapper supports the following F# native types, which may be nested without any limit:

*   Primitive types: ``int``, ``float``, ``bool``, ``string``
*   F# list type
*   F# record type
*   F# option type
*   F# discriminated union type
*   F# Map type

Please refer to the [Tutorials](tutorial.html), for mapping specifications.

The ``Legivel.Mapper`` has been designed with customization in mind. You can add your own
yaml to native mappers, discard current mappers. You can even decide not to use this component at all.

*)