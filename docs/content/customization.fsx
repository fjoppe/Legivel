(**
Legivel Customization
=====================

Legivel was designed with tweaking and customization in mind. Maybe you want to parse with
your own yaml-schema, in which you add, or remove a yaml-tag (data type). Maybe you want a custom
yaml to native mapping, perhaps non-generic, but specific to your application.

Legivel is spit into two parts:

*  ``Legivel.Parser``: parses any Yaml-1.2, into a generic native structure (representation graph)
*  ``Legivel.Mapper``: maps the output of ``Legivel.Parser`` to specific native types.


#   Legivel.Parser

This is the generic Yaml parser, which parses any Yaml - complieing to the 1.2 specification.


#   Legivel.Mapper

*)