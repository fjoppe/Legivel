#   Legivel Wish List

##  Processing options

The processing of the Legivel Mapper should be configurable with attributes and processing options.

The following options are desired:

*   Use default value if missing. ie a missing mandatory int should get value 0, a missing string should get an empty string (!)

The following attributes are desired:

*   Attribute in which one can define a default value on a field, in case of a missing value


##  Wrapped style Discriminated Union

The following Yaml must be mapped to a DU's Union Cases, with containing data:
```yaml
post: data1     #   'post' is DU-case, 'data1' is contained data
get:  data2     #   'post' and 'get' are in a sequence in this example
```

Implementation is tricky; probably requires a dedicated list-of-DU mapper.


##  More FSharp types support

Support for:

*   FSharp Map
*   FSharp Set (using YamlExtended tag "!!set"?)
*   Tuples? (not sure about this one)


##  Multi-platform support

This library currently supports net461, obviously it should also support netstandard16 or something higher.


##  Readable Serialization

Currently, Legivel only supports Yaml parsing. The Legivel.Parser also has a ToCanonical serializer, but this
is in machine format, not in human-readable format. This feature should be taken into two steps,
in the Legivel.Mapper a mapper must convert native to the generic format, and the Legivel.Parser
should get the generic to readable string function. Adding this feature contradicts "Parser" in the name Legivel.Parser,
but let's solve that naming problem later.
