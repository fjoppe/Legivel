#   Legivel Wish List


##  More build-in primitive data types 

The Legivel Mapper should support more build-in primitive data types. Some consideration is required when approaching this. 
You cannot use the YamlExtended schema as-is, because the non-scalar tags distort the suitabilityy of the required output.
Either use the pure set of the YamlCore schema, or do some cherry-picking in the YamlExtended schema.


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

