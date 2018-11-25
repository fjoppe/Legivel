(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I __SOURCE_DIRECTORY__
#I "../../bin/Legivel.Mapper"


(**
Legivel
======================

A Yaml to Native processor in F#, producing F# types.

"Legivel" is the Portugese word for ["readable"](https://translate.google.com/?hl=nl#en/pt/readable); ie Yaml was invented to create a
human readable data-format.

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The Legivel library can be <a href="https://nuget.org/packages/Legivel">installed from NuGet</a>:
      <pre>PM> Install-Package Legivel</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

This example demonstrates Yaml to Native conversion using this library:
*)
#r "Legivel.Parser.dll"
#r "Legivel.Mapper.dll"
open Legivel.Serialization

type PlayEvent = {
  time   : string
  player : string
  action : string
}

//  example : http://www.yaml.org/spec/1.2/spec.html#id2760519
let yaml = "
---
time: 20:03:20
player: Sammy Sosa
action: strike (miss)
...
---
time: 20:03:47
player: Sammy Sosa
action: grand slam
..." 

(*** define-output:test ***)
Deserialize<PlayEvent> yaml
(**
Which results in:
*)
(*** include-it: test  ***)

(**

Examples & documentation
-----------------------

Legivel comes with comprehensible documentation. 

 * [Tutorial](tutorial.html) contains more examples how to use this library.
 * [Processing Options](processingoptions.html) customize the yaml processing
 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library.
*  [Customization](customization.html) contains information to customize yaml-processing.
*  [Features](features.html) documents the details of implemented features.
*  [Wish List](WishList.html) contains pending ideas, which may be added in future.

Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fjoppe/Legivel/tree/master/docs/content
  [gh]: https://github.com/fjoppe/Legivel
  [issues]: https://github.com/fjoppe/Legivel/issues
  [readme]: https://github.com/fjoppe/Legivel/blob/master/README.md
  [license]: https://github.com/fjoppe/Legivel/blob/master/LICENSE.txt
*)
