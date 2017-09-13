module RepresentationGraph

open YamlParser.Internals

type ParseInfo = {
        Start : DocumentLocation
        End   : DocumentLocation
    }
    with
        static member internal Create : DocumentLocation -> DocumentLocation -> ParseInfo


type NodeKind = 
    | Mapping
    | Sequence
    | Scalar

[<NoEquality; NoComparison>]
type internal TagFunctions = {
        /// true if two nodes are equal
        AreEqual    : Node -> Node -> bool

        /// Retrieve the hash of the specified node
        GetHash     : Node -> Lazy<NodeHash>

        /// Called after node construction and tag resolution
        PostProcessAndValidateNode     : Node -> FallibleOption<Node, ErrorMessage>

        /// true if the Node is a match for the specified tag
        IsMatch     : Node -> GlobalTag -> bool
    }
    with
        static member Create : (Node -> Node -> bool) -> (Node -> Lazy<NodeHash>) -> (Node -> FallibleOption<Node, ErrorMessage>) -> (Node -> GlobalTag -> bool) -> TagFunctions
and
    [<NoEquality; NoComparison>]
    LocalTagsFuncs = {
        AreEqual : Node -> Node -> bool
        GetHash  : Node -> Lazy<NodeHash>
    }
and
    [<Sealed>]
    GlobalTag =
    interface System.IComparable
    with
        member Uri  : string with get 
        member Kind : NodeKind with get 
        member Regex : string with get 
        member ToCanonical : string -> string option
        static member internal Create : string * NodeKind * string * (string -> string option) * TagFunctions -> GlobalTag
        static member internal Create : string * NodeKind * string * TagFunctions -> GlobalTag
        static member internal Create : string * NodeKind * TagFunctions -> GlobalTag
        member internal TagFunctions : TagFunctions with get
        member internal SetTagFunctions : TagFunctions -> GlobalTag
        member internal IsMatch : Node -> bool
and
    [<Sealed>]
    LocalTag =
    interface System.IComparable
    with
        member Handle  : string with get 
        static member Create : string -> LocalTagsFuncs -> LocalTag
and
    TagKind =
    |   Global       of GlobalTag
    |   Unrecognized of GlobalTag
    |   Local        of LocalTag
    |   NonSpecific  of LocalTag
        with
            member internal ToPrettyString : unit -> string
            member internal CanonFn : (string -> string option)
            member internal AreEqual : Node -> Node -> bool
            member internal PostProcessAndValidateNode : Node -> FallibleOption<Node, ErrorMessage>
            member internal EqualIfNonSpecific : TagKind -> bool
            member Uri : string with get
and
    [<CustomEquality; CustomComparison>]
    NodeData<'T when 'T : equality and 'T :> System.IComparable> = {
        Tag  : TagKind
        Data : 'T
        ParseInfo : ParseInfo
    }
    with
        static member internal Create<'a when 'a : equality and 'a :> System.IComparable> : TagKind -> 'a -> ParseInfo -> NodeData<'a>
        interface System.IComparable

and
    Node =
    | SeqNode of NodeData<Node list>
    | MapNode of NodeData<(Node*Node) list>
    | ScalarNode of NodeData<string>
    with
        member ParseInfo : ParseInfo with get
        member NodeTag : TagKind with get
        member Kind : NodeKind with get
        member internal ToPrettyString : unit -> string
        member internal Hash : NodeHash with get
        member internal SetTag : TagKind -> Node

type ParseMessageAtLine = {
        Location: DocumentLocation
        Message : string
    }
    with
        static member internal Create : DocumentLocation -> string -> ParseMessageAtLine

type ErrorResult = {
        Warn  : ParseMessageAtLine list 
        Error : ParseMessageAtLine list
        StopLocation : DocumentLocation
        RestString  : string
    }
    with
        static member internal Create : ParseMessageAtLine list -> ParseMessageAtLine list -> DocumentLocation -> string -> ErrorResult

type Unrecognized =  {
        Scalar      : int
        Collection  : int
    }
    with
        static member internal Create : int -> int -> Unrecognized

type TagReport = {
        Unresolved   : int
        Unrecognized : Unrecognized
        Unavailable  : int
    }
    with
        static member internal Create : Unrecognized -> int -> int -> TagReport

type internal TagShorthand = {
        ShortHand : string
        MappedTagBase : string
    }
    with
        static member Create : string * string -> TagShorthand
        static member DefaultSecondaryTagHandler : TagShorthand


type ParsedDocumentResult = {
        Warn        : ParseMessageAtLine list
        TagReport   : TagReport
        StopLocation : DocumentLocation
        TagShorthands: TagShorthand list
        Document    : Node
    }
    with
        static member internal Create : ParseMessageAtLine list -> TagReport -> DocumentLocation ->  TagShorthand list -> Node -> ParsedDocumentResult

type EmptyDocumentResult = {
        Warn        : ParseMessageAtLine list
        StopLocation : DocumentLocation
    }
    with
            static member Create : ParseMessageAtLine list -> DocumentLocation -> EmptyDocumentResult

type Representation =
    |   NoRepresentation of ErrorResult
    |   PartialRepresentaton of ParsedDocumentResult
    |   CompleteRepresentaton of ParsedDocumentResult
    |   EmptyRepresentation of EmptyDocumentResult