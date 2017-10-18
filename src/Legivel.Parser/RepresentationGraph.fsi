module Legivel.RepresentationGraph

open Legivel.Internals
open Legivel.Common


/// Registers parsing information per Node, related to the source document
type ParseInfo = {
        /// Marks the start location in the source document
        Start : DocumentLocation

        /// Marks the end location in the source document
        End   : DocumentLocation
    }
    with
        /// Create a ParseInfo instance
        static member internal Create : DocumentLocation -> DocumentLocation -> ParseInfo


/// A Node may be of Maping, Sequence or Scalar type
type NodeKind = 
    | Mapping
    | Sequence
    | Scalar


/// Each Global Tag must implement the functions in this type
[<NoEquality; NoComparison>]
type internal TagFunctions = {
        /// A function which returs true if the two given nodes are equal (according to yaml equality).
        AreEqual    : Node -> Node -> bool

        /// A function which returs the hash of the given node
        GetHash     : Node -> Lazy<NodeHash>

        /// A function which is called after Node construction and tag resolution.
        /// This function is to post-process Node construction and validate its tag.
        /// an incorrect explicitly specified tag should return a Parse Error.
        PostProcessAndValidateNode     : Node -> FallibleOption<Node, ErrorMessage>

        /// A function which returs true if the given Node is a match for the given tag. 
        /// This is the case when the Node complies to all tag's rules.
        IsMatch     : Node -> GlobalTag -> bool
    }
    with
        static member Create : (Node -> Node -> bool) -> (Node -> Lazy<NodeHash>) -> (Node -> FallibleOption<Node, ErrorMessage>) -> (Node -> GlobalTag -> bool) -> TagFunctions
and
    /// Local Tags should use these functions when appropriate
    [<NoEquality; NoComparison>]
    LocalTagsFuncs = {
        /// A function which returs true if the two given nodes are equal (according to yaml equality).
        AreEqual : Node -> Node -> bool

        /// A function which returs the hash of the given node
        GetHash  : Node -> Lazy<NodeHash>
    }
and
    /// Defines a Global Tag, its properties and functions.
    [<Sealed>]
    GlobalTag =
    interface System.IComparable
    with
        /// Retrieves the tag's uri, example: "tag:yaml.org,2002:str"
        member Uri  : string with get
        
        /// Applicable to which NodeKind
        member Kind : NodeKind with get 

        /// Pattern used to detect whether string-data complies for this Global Tag
        member Regex : string with get 

        /// Convert the given string to its (yaml) canonical form
        member ToCanonical : string -> string option

        /// Create a Global Tag based on uri, nodekind, regex, to Canonical func, tagfunctions
        static member internal Create : uri:string * nk:NodeKind * rgx:string * toCanon:(string -> string option) * tgfn:TagFunctions -> GlobalTag

        /// Create a Global Tag based on uri, nodekind, regex, tagfunctions
        static member internal Create : string * NodeKind * string * TagFunctions -> GlobalTag

        /// Create a Global Tag based on uri, nodekind, tagfunctions
        static member internal Create : string * NodeKind * TagFunctions -> GlobalTag

        /// Retrieve tagfunctions
        member internal TagFunctions : TagFunctions with get

        /// Set Tagfunctions and return new GlobalTag
        member internal SetTagFunctions : TagFunctions -> GlobalTag

        /// Returns true if the given Node complies to the rules o this tag 
        member internal IsMatch : Node -> bool
and
    /// Defines a Local Tag, its properties and functions.
    [<Sealed>]
    LocalTag =
    interface System.IComparable
    with
        /// Retrieves the tag's handle, ie !foo -> foo
        member Handle  : string with get 

        /// Create a Local Tag
        static member Create : string -> LocalTagsFuncs -> LocalTag
and
    /// A Tag can be Local, Global, Unrecognized or Non Specific
    TagKind =
    |   Global       of GlobalTag
    |   Unrecognized of GlobalTag
    |   Local        of LocalTag
    |   NonSpecific  of LocalTag
        with
            /// For dev/debugging
            member internal ToPrettyString : unit -> string

            /// Retrieves the function which converts a string to its tag-Canonical form
            member internal CanonFn : (string -> string option)

            /// Retrieves the function which determines whether two Nodes are yaml-wise equal
            member internal AreEqual : Node -> Node -> bool

            /// Retrieves the function which is called after Node construction and tag resolution.
            member internal PostProcessAndValidateNode : Node -> FallibleOption<Node, ErrorMessage>

            /// Special case: returns handle equality if the tag is non-specific. Used in sentinel pattern.
            member internal EqualIfNonSpecific : TagKind -> bool

            /// Retrieves the Uri for Global Tags and the handle for Local Tags
            member Uri : string with get
and
    /// Contains the actual data of a node, used for all types, Map, Seq, Scalar
    [<CustomEquality; CustomComparison>]
    NodeData<'T when 'T : equality and 'T :> System.IComparable> = {
        /// The Node's tag
        Tag  : TagKind

        /// The Node's data; Scalar:string, Sequence: Node list, Mapping: (key:Node * value:Node) list
        Data : 'T

        /// Parsing information for this Node, related to the source yaml Document
        ParseInfo : ParseInfo
    }
    with
        static member internal Create<'a when 'a : equality and 'a :> System.IComparable> : TagKind -> 'a -> ParseInfo -> NodeData<'a>
        interface System.IComparable

and
    /// The main/root structure of a Yaml Node
    Node =
    | SeqNode of NodeData<Node list>
    | MapNode of NodeData<(Node*Node) list>
    | ScalarNode of NodeData<string>
    with
        member ParseInfo : ParseInfo with get
        member NodeTag : TagKind with get
        member Kind : NodeKind with get

        /// For dev/debugging
        member internal ToPrettyString : unit -> string

        //  Retrieve the Spooky Hash of this Node, used for node-equality comparison.
        member internal Hash : NodeHash with get

        /// To set the tag of this Nod, ie when a non-specific tag is resolved to a specific tag
        member internal SetTag : TagKind -> Node


/// Contains a warning or error message
type ParseMessageAtLine = {
        Location: DocumentLocation
        Message : string
    }
    with
        static member internal Create : DocumentLocation -> string -> ParseMessageAtLine


/// Yaml Error Result Info
type ErrorResult = {
        /// All warning messages
        Warn  : ParseMessageAtLine list

        /// All error messages
        Error : ParseMessageAtLine list

        /// Document location which could not be parsed
        StopLocation : DocumentLocation

        /// The rest string, which was not parsed
        RestString  : string
    }
    with
        static member internal Create : ParseMessageAtLine list -> ParseMessageAtLine list -> DocumentLocation -> string -> ErrorResult


/// Statistics for unrecognized tags
type Unrecognized =  {
        Scalar      : int
        Collection  : int
    }
    with
        static member internal Create : int -> int -> Unrecognized


/// Statistics of problematic tags used in the source yaml
type TagReport = {
        Unresolved   : int
        Unrecognized : Unrecognized
        Unavailable  : int
    }
    with
        static member internal Create : Unrecognized -> int -> int -> TagReport

/// Mapping of tag shorthands, with the TAG directive
type TagShorthand = {
        ShortHand : string
        MappedTagBase : string
    }
    with
        static member Create : string * string -> TagShorthand
        static member DefaultSecondaryTagHandler : TagShorthand

/// Fully parsed document result info
type ParsedDocumentResult = {
        /// All warning messages
        Warn        : ParseMessageAtLine list

        /// Statistics of problematic tags used in the source yaml
        TagReport   : TagReport

        /// The location where parsing finished
        StopLocation : DocumentLocation

        /// List of tag shorthands, set with the TAG directive
        TagShorthands: TagShorthand list

        /// The root Node of the parsed document
        Document    : Node
    }
    with
        static member internal Create : ParseMessageAtLine list -> TagReport -> DocumentLocation ->  TagShorthand list -> Node -> ParsedDocumentResult


/// Empty Result Info (possible candidate to discard)
type EmptyDocumentResult = {
        Warn        : ParseMessageAtLine list
        StopLocation : DocumentLocation
    }
    with
            static member Create : ParseMessageAtLine list -> DocumentLocation -> EmptyDocumentResult

/// Representation of the parsed document
type Representation =
    /// Errors occurred
    |   NoRepresentation of ErrorResult

    /// Not all tags were resolved
    |   PartialRepresentaton of ParsedDocumentResult

    /// Completely parsed succesfully
    |   CompleteRepresentaton of ParsedDocumentResult

    /// There was no result (possible candidate to discard; this never occurred when testing)
    |   EmptyRepresentation of EmptyDocumentResult

