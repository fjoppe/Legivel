module Legivel.TagResolution

open Legivel.RepresentationGraph


/// Info required for tag resolution
[<NoEquality; NoComparison>]
type TagResolutionInfo = {
        /// either "!" or "?"
        NonSpecificTag  : string

        /// Unsupported. Target feature: path from root-to-current via mapping keys
        Path            : Node list

        /// The Node for which its tag needs t be resolved
        Content         : Node
    }
    with
        static member Create : string -> Node list -> Node -> TagResolutionInfo


/// Function signature that is to resolve the tag of a Node
type TagResolutionFunc = (TagResolutionInfo -> GlobalTag option)


/// Function signature that is to return a GlobalTag for an unresolved tag
type UnresolvedTagResolutionFunc = (NodeKind -> string -> GlobalTag)


/// A schema for tags and tag-resolution functions
[<NoEquality; NoComparison>]
type GlobalTagSchema = {
    GlobalTags              : GlobalTag list
    TagResolution           : TagResolutionFunc
    UnresolvedResolution    : UnresolvedTagResolutionFunc
    LocalTags               : LocalTagsFuncs
}


module SchemaUtils =
    //  Retrieve the data from a Map Node; throws exception if NodeKind is incorrect
    val getMapNode : Node -> (Node*Node) list

    //  Retrieve the data from a Seq Node; throws exception if NodeKind is incorrect
    val getSeqNode : Node -> Node list

    //  Retrieve the data from a Scalar Node; throws exception if NodeKind is incorrect
    val getScalarNode : Node -> string

    //  Retrieve the NodeData<_> from a Map Node; throws exception if NodeKind is incorrect
    val getMapNodeData : Node -> NodeData<(Node*Node) list>

    //  Retrieve the NodeData<_> from a Seq Node; throws exception if NodeKind is incorrect
    val getSeqNodeData : Node -> NodeData<Node list>

    //  Retrieve the NodeData<_> from a Scalar Node; throws exception if NodeKind is incorrect
    val getScalarNodeData : Node -> NodeData<string>

    //  Called to resolve an unresolved tag, ie "!" and "?"
    val tagResolution : (TagResolutionInfo->GlobalTag option) -> (GlobalTag*GlobalTag*GlobalTag) -> GlobalTag list -> GlobalTag list -> GlobalTag list -> TagResolutionFunc


/// Non specific tags, "!", "?" and unresolved
module internal NonSpecific =
    val NonSpecificTagQT : TagKind
    val NonSpecificTagQM : TagKind
    val UnresolvedTag    : TagKind


///    Failsafe schema:  http://www.yaml.org/spec/1.2/spec.html#id2802346
module Failsafe =
    val MappingGlobalTag : GlobalTag
    val SequenceGlobalTag : GlobalTag
    val StringGlobalTag : GlobalTag
    val Schema : GlobalTagSchema


///    Json schema:  http://www.yaml.org/spec/1.2/spec.html#id2803231
module JSON =
    val MappingGlobalTag : GlobalTag
    val SequenceGlobalTag : GlobalTag
    val StringGlobalTag : GlobalTag
    val NullGlobalTag : GlobalTag
    val BooleanGlobalTag : GlobalTag
    val IntegerGlobalTag : GlobalTag
    val FloatGlobalTag : GlobalTag
    val Schema : GlobalTagSchema


///  Core Schema:    http://www.yaml.org/spec/1.2/spec.html#id2804923
module YamlCore =
    val MappingGlobalTag : GlobalTag
    val SequenceGlobalTag : GlobalTag
    val StringGlobalTag : GlobalTag
    val NullGlobalTag : GlobalTag
    val BooleanGlobalTag : GlobalTag
    val IntegerGlobalTag : GlobalTag
    val FloatGlobalTag : GlobalTag
    val Schema : GlobalTagSchema


///  'Extended' schema (not official name):  http://yaml.org/type/
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
    val MergeGlobalTag : GlobalTag
    val BinaryGlobalTag : GlobalTag
    val OrderedMappingGlobalTag : GlobalTag
    val OrderedPairsGlobalTag : GlobalTag
    val UnOrderedSetGlobalTag : GlobalTag
    val Schema : GlobalTagSchema

