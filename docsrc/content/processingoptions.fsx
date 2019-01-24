(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I __SOURCE_DIRECTORY__
#I "../../bin/Legivel.Mapper/netstandard2.0"
#r "Legivel.Parser.dll"
#r "Legivel.Mapper.dll"
open Legivel.Serialization
open System

(**
Legivel.Mapper Processing Options
=================================

Calling ``Deserialize`` will run the yaml-to-model conversion with default options.

With the ``DeserializeWithOptions``, you can customize your yaml processing.

# Cross Check
By default, the model is leading, which means that any extra yaml in the source document will be ignored
as long as it minimally includes all content required for the target model. With the processing option ``MappingMode``
you can require cross-check between content and model.
*)
(*** define-output: cc1 ***)
type ProcessingOptionsType = {
    Name  : string
    EMail : string option
}

let yml = "
Name: Frank
#   below must cause a warning bc they're not part of the target type
Should Give A Warning : Morning
"

DeserializeWithOptions<ProcessingOptionsType> [MappingMode(MapYaml.WithCrossCheck)] yml
(**
Which results in:
*)
(*** include-it: cc1 ***)
(**
Cross checking has the following options:
*)
//  (default) The model is leading, any extra yaml content is ignored
MapYaml.ToModelOnly     

//  Cross-Check the yaml to the model (in certain cases), extra yaml causes warnings
MapYaml.WithCrossCheck  

//  Cross-Check the yaml to the model (in certain cases), extra yaml causes errors
MapYaml.AndRequireFullProjection

(**
certain cases: Record types.
*)
