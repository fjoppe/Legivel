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

The Yaml parser is called with the injection of a Yaml Schema (and the Yaml itself). The Yaml schema
defines the types applied to the document which is to be parsed. These types are resolved either 
via [Explicit or Implicit](http://www.yaml.org/spec/1.2/spec.html#id2768011) Global Tags. You can
also use Local Tags, but these do not depend on Yaml Schema and must be processed after Legivel.Parser (successful) output.

A Yaml schema defines all supported data-types, which are implemented as Yaml Global Tags.

A schema is defined as follows (in ``Legivel.TagResolution``):
```fsharp
type GlobalTagSchema = {
    GlobalTags              : GlobalTag list
    TagResolution           : TagResolutionFunc
    UnresolvedResolution    : UnresolvedTagResolutionFunc
    LocalTags               : LocalTagsFuncs
}
```

| Field | description |
| :--- | :--- |
| ``GlobalTags`` | All global tags supported by the schema|
| ``TagResolution`` | The function that is called when a tag requires resolution, this usually is the case when a node received an implicit tag|
| ``UnresolvedResolution`` | The function that is called when a tag cannot be resolved, should return a global tag|
| ``LocalTags`` | A set of function which are applicable for local tags, ie the 'AreEqual' function for a node with local tag|

---

A GlobalTag is defined in ``Legivel.RepresentationGraph`` as:
```fsharp
type GlobalTag = {
        Uri'    : string
        Kind'   : NodeKind
        Regex'  : string
        canonFn : string -> string option
        TagFunctions' : TagFunctions
    }
```    

| Field | description |
| :--- | :--- |
| ``Uri'`` | The tag uri, ie "tag:yaml.org,2002:str" for strings |
| ``Kind'`` | The kind of node which may match this tag - ie NodeKind scalar for strings |
| ``Regex'`` | If Yaml content string matches this regex pattern then the node may be resolved to this tag, in the tag resolution process |
| ``canonFn'`` | Convert the input (yaml content) string to the canonical format of this tag. This may fail if the input string does not comply. In case of an explicit tag, this was not detected earlier |
| ``TagFunctions'`` | A set of functions, which apply for this tag |

---


The ``TagFunctions`` is defined in ``Legivel.RepresentationGraph`` as: 
 ```fsharp
 type TagFunctions = {
        AreEqual    : Node -> Node -> bool
        GetHash     : Node -> Lazy<NodeHash>
        PostProcessAndValidateNode     : Node -> FallibleOption<Node, ErrorMessage>
        IsMatch     : Node -> GlobalTag -> bool
    }
```

| Field | description |
| :--- | :--- |
| ``AreEqual`` | Should return true if both input nodes [are equal](http://www.yaml.org/spec/1.2/spec.html#id2764652) |
| ``GetHash`` | Returns the hash of a node - this value is used in AreEqual functions |
| ``PostProcessAndValidateNode`` | Is called after node construction and tag-resolution. This function may do some extra checks or transformations. |
| ``IsMatch`` | true if the Node matches the specified tag |

---

The above mentioned ``TagResolutionFunc``, is the function which is called for tag resolution, and is defined as:
```fsharp
type TagResolutionFunc = (TagResolutionInfo -> GlobalTag option)
```

A ``GlobalTag`` is returned when it was resolved, otherwise ``None``

---


The ``TagResolutionInfo`` is defined as follows:
```fsharp
type TagResolutionInfo = {
        NonSpecificTag  : string
        Path            : Node list
        Content         : Node
    }
```
| Field | description |
| :--- | :--- |
| ``NonSpecificTag`` | The non-specific tag, "!" or "?", see [tag resolution](http://www.yaml.org/spec/1.2/spec.html#id2768011) | 
| ``Path`` | The parent path to the node, only mentioning keys from mapping nodes, the List.Head is the most direct parent | 
| ``Content`` | The Node for which a tag needs to be resolved | 

---

This covers the most important aspects from Schema and tag resolution.

#   Legivel.Mapper

The ``Legivel.Mapper`` takes output from the ``Legivel.Parser``, and tries to map it to a given type.
You can decide not to use this component at all, ie when you require a C# mapper.

The mapping proces can be tweaked at different levels:
*   Parsing level: the mapper can define which Yaml Schema to use in the Parser; 
*   Mapping level: you can choose your own set of mappers

*)