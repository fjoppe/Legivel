module TagResolution

open RepresentationGraph

[<NoEquality; NoComparison>]
type TagResolutionInfo = {
        NonSpecificTag  : string
        Path            : Node list
        Content         : Node
        NodeKind        : NodeKind
    }
    with
        static member Create : string -> Node list -> Node -> NodeKind -> TagResolutionInfo


type TagResolutionFunc = (TagResolutionInfo -> GlobalTag option)
type UnresolvedTagResolutionFunc = (NodeKind -> string -> GlobalTag)


[<NoEquality; NoComparison>]
type GlobalTagSchema = {
    GlobalTags              : GlobalTag list
    TagResolution           : TagResolutionFunc
    UnresolvedResolution    : UnresolvedTagResolutionFunc
    LocalTags               : LocalTagsFuncs
}

module internal NonSpecific =
    val NonSpecificTagQT : TagKind
    val NonSpecificTagQM : TagKind
    val UnresolvedTag    : TagKind

module Failsafe =
    val MappingGlobalTag : GlobalTag
    val SequenceGlobalTag : GlobalTag
    val StringGlobalTag : GlobalTag
    ///    Failsafe schema:  http://www.yaml.org/spec/1.2/spec.html#id2802346
    val Schema : GlobalTagSchema

module JSON =
    val MappingGlobalTag : GlobalTag
    val SequenceGlobalTag : GlobalTag
    val StringGlobalTag : GlobalTag
    val NullGlobalTag : GlobalTag
    val BooleanGlobalTag : GlobalTag
    val IntegerGlobalTag : GlobalTag
    val FloatGlobalTag : GlobalTag
    ///    Json schema:  http://www.yaml.org/spec/1.2/spec.html#id2803231
    val Schema : GlobalTagSchema

module YamlCore =
    val MappingGlobalTag : GlobalTag
    val SequenceGlobalTag : GlobalTag
    val StringGlobalTag : GlobalTag
    val NullGlobalTag : GlobalTag
    val BooleanGlobalTag : GlobalTag
    val IntegerGlobalTag : GlobalTag
    val FloatGlobalTag : GlobalTag
    ///  Core Schema:    http://www.yaml.org/spec/1.2/spec.html#id2804923
    val Schema : GlobalTagSchema

module YamlExtended =
    val MappingGlobalTag : GlobalTag
    val SequenceGlobalTag : GlobalTag
    val StringGlobalTag : GlobalTag
    val NullGlobalTag : GlobalTag
    val BooleanGlobalTag : GlobalTag
    val IntegerGlobalTag : GlobalTag
    val FloatGlobalTag : GlobalTag
    val TimestampGlobalTag : GlobalTag
    val ValueGlobalTag : GlobalTag
    val BinaryGlobalTag : GlobalTag
    val OrderedMappingGlobalTag : GlobalTag
    val OrderedPairsGlobalTag : GlobalTag
    val UnOrderedSetGlobalTag : GlobalTag
    ///  'Extended' schema (not official name):  http://yaml.org/type/
    val Schema : GlobalTagSchema

