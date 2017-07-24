module RepresentationGraph

open System.Diagnostics
open YamlParser.Internals





type ParseInfo = {
        Start : DocumentLocation
        End   : DocumentLocation
    }
    with
        static member Create s e = { Start = s; End = e}


type NodeKind = 
    | Mapping
    | Sequence
    | Scalar

[<NoEquality; NoComparison>]
type TagFunctions = {
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
        static member Create eq hs vl ismt = { AreEqual = eq; GetHash = hs; PostProcessAndValidateNode = vl; IsMatch = ismt}


and 
    [<NoEquality; NoComparison>]
    LocalTagsFuncs = {
        AreEqual : Node -> Node -> bool
        GetHash  : Node -> Lazy<NodeHash>
    }

and
    [<CustomEquality; CustomComparison>]
    [<StructuredFormatDisplay("{AsString}")>]
    GlobalTag = {
        Uri     : string
        Kind    : NodeKind
        Regex   : string
        canonFn : string -> string
        TagFunctions : TagFunctions
    }
    with
        static member Create (uri, nk, rgx, canon, tgfn) =
            { 
                Uri = uri; 
                Kind = nk;
                Regex = sprintf "\\A(%s)\\z" rgx
                canonFn = canon
                TagFunctions = tgfn
            }

        static member Create (uri, nk, rgx, tgfn) = GlobalTag.Create (uri, nk, rgx, (fun s -> s), tgfn)

        static member Create (uri, nk, tgfn) = GlobalTag.Create (uri, nk, ".*", (fun s -> s), tgfn)

        member this.AreEqual n1 n2 = this.TagFunctions.AreEqual n1 n2
        member this.GetHash n = this.TagFunctions.GetHash n
        member this.PostProcessAndValidateNode n = this.TagFunctions.PostProcessAndValidateNode n
        member this.IsMatch n = this.TagFunctions.IsMatch n this

        member this.Canonical s = this.canonFn s

        override this.ToString() = sprintf "<%A::%s>" this.Kind this.Uri
        member m.AsString = m.ToString()

        override this.Equals(other) = other |> InternalUtil.equalsOn(fun that -> this.Uri = that.Uri && this.Kind = that.Kind)

        override this.GetHashCode() = this.Uri.GetHashCode() ^^^ this.Kind.GetHashCode()

        interface System.IComparable with
            member this.CompareTo other = other |> InternalUtil.compareOn(fun that -> this.Uri.CompareTo(that.Uri))

and
    [<CustomEquality; CustomComparison>]
    LocalTag = {
        Handle      : string
        LocalTag    : LocalTagsFuncs
    }
    with
        static member Create h f = { Handle = h; LocalTag = f}

        override this.Equals(other) = other |> InternalUtil.equalsOn(fun that -> this.Handle = that.Handle)

        override this.GetHashCode() = this.Handle.GetHashCode() 

        interface System.IComparable with
            member this.CompareTo other = other |> InternalUtil.compareOn(fun that -> this.Handle.CompareTo(that.Handle))
        
and
    [<StructuredFormatDisplay("{AsString}")>]
    TagKind =
    |   Global       of GlobalTag
    |   Unrecognized of GlobalTag
    |   Local        of LocalTag
    |   NonSpecific  of LocalTag
    with
        override this.ToString() =
            match this with
            |   Global       gt -> sprintf "Global:%O" gt
            |   Unrecognized gt -> sprintf "Unrecognized:%O" gt
            |   Local        ls -> sprintf "Local:%O" (ls.Handle)
            |   NonSpecific  ls -> sprintf "NonSpecific:%O" (ls.Handle)

        member this.ToPrettyString() =
            match this with
            |   Global       gt -> sprintf "%s" gt.Uri
            |   Unrecognized gt -> sprintf "%s" gt.Uri
            |   Local        ls -> sprintf "%s" ls.Handle
            |   NonSpecific  ls -> sprintf "%s" ls.Handle

        member this.EqualIfNonSpecific otherTag =
            match (this, otherTag) with
            |   (NonSpecific a, NonSpecific b)  -> a.Handle=b.Handle
            |   _   -> false

        member this.AreEqual n1 n2 =
            match this with
            |   Global       gt -> gt.AreEqual n1 n2
            |   Unrecognized gt -> gt.AreEqual n1 n2
            |   Local        lt -> lt.LocalTag.AreEqual n1 n2
            |   NonSpecific  lt -> lt.LocalTag.AreEqual n1 n2

        member this.GetHash n =
            match this with
            |   Global       gt -> gt.GetHash n
            |   Unrecognized gt -> gt.GetHash n
            |   Local        lt -> lt.LocalTag.GetHash n
            |   NonSpecific  lt -> lt.LocalTag.GetHash n

        member this.PostProcessAndValidateNode n =
            match this with
            |   Global       gt -> gt.PostProcessAndValidateNode n
            |   Unrecognized gt -> gt.PostProcessAndValidateNode n
            // local tags are checked by the application, so always valid here
            |   Local        _  -> Value(n) 
            |   NonSpecific  _  -> Value(n)

        member this.Uri 
            with get() =
                match this with
                |   Global       gt -> sprintf "%s" gt.Uri
                |   Unrecognized gt -> sprintf "%s" gt.Uri
                |   Local        ls -> sprintf "%s" ls.Handle
                |   NonSpecific  ls -> sprintf "%s" ls.Handle

        member this.CanonFn =
            match this with
            |   Global       gt -> gt.canonFn 
            |   Unrecognized gt -> gt.canonFn 
            |   Local        _  -> id
            |   NonSpecific  _  -> id

        member m.AsString = m.ToString()


and
    [<CustomEquality; CustomComparison>]
    [<StructuredFormatDisplay("{AsString}")>]
    NodeData<'T when 'T : equality and 'T :> System.IComparable> = {
        Tag  : TagKind
        Data : 'T
        ParseInfo : ParseInfo
    }
    with
        static member Create t d pi =
            { Tag = t; Data = d; ParseInfo = pi}

        member this.SetTag t = 
            { this with Tag = t}

        override this.ToString() = sprintf "%O %A" (this.Tag) (this.Data)
        member m.AsString = m.ToString()

        member this.ToPrettyString() = sprintf "%O %A" (this.Tag) (this.Data)

        override this.Equals(other) = 
            other 
            |> InternalUtil.equalsOn(fun that ->
                this.Tag = that.Tag && 
                this.Data = that.Data
                )

        override this.GetHashCode() = this.Data.GetHashCode() ^^^ this.ParseInfo.GetHashCode()

        interface System.IComparable with
            member this.CompareTo other = other |> InternalUtil.compareOn(fun (that:NodeData<'T>) -> this.Data.CompareTo(that.Data))

and
    [<DebuggerDisplay("{this.DebuggerInfo}")>]
    Node =
    | SeqNode of NodeData<Node list>
    | MapNode of NodeData<(Node*Node) list>
    | ScalarNode of NodeData<string>
    with
        member private this.tagString t =
            match t with
            |   Global gt -> gt.Uri
            |   Local  s  -> s.Handle
            |   NonSpecific s -> s.Handle
            |   Unrecognized gt -> gt.Uri

        member this.Hash 
            with get() =
                match this with
                |   SeqNode n       -> n.Tag.GetHash this
                |   MapNode n       -> n.Tag.GetHash this
                |   ScalarNode n    -> n.Tag.GetHash this
                |> fun h -> h.Force()
        
        member this.SetTag t = 
            match this with
            |   SeqNode n       -> SeqNode(n.SetTag t)
            |   MapNode n       -> MapNode(n.SetTag t)
            |   ScalarNode n    -> ScalarNode(n.SetTag t)

        member this.NodeTag 
            with get() =
                match this with
                |   SeqNode n       -> n.Tag
                |   MapNode n       -> n.Tag
                |   ScalarNode n    -> n.Tag

        member this.ParseInfo 
            with get() =
                match this with
                |   SeqNode n       -> n.ParseInfo
                |   MapNode n       -> n.ParseInfo
                |   ScalarNode n    -> n.ParseInfo

        member this.ToPrettyString() =
            match this with
            |   SeqNode n       -> n.ToPrettyString()
            |   MapNode n       -> n.ToPrettyString()
            |   ScalarNode n    -> n.ToPrettyString()

        member this.Kind
            with get() =
                match this with
                |   SeqNode _       -> Sequence
                |   MapNode _       -> Mapping
                |   ScalarNode _    -> Scalar

        member this.DebuggerInfo 
            with get() =
                match this with
                |   SeqNode d       -> sprintf "<%s>[..], length=%d" (this.tagString d.Tag) d.Data.Length
                |   MapNode d       -> sprintf "<%s>{..}, length=%d" (this.tagString d.Tag) d.Data.Length
                |   ScalarNode d    -> sprintf "<%s>\"%s\"" (this.tagString d.Tag) d.Data


type Legend = {
        YamlVersion : string
    }


type ParseMessageAtLine = {
        Location: DocumentLocation
        Message : string
    }
    with
        static member Create dl s = {Location = dl; Message = s}


type ErrorResult = {
        Warn  : ParseMessageAtLine list 
        Error : ParseMessageAtLine list
        StopLocation : DocumentLocation
        RestString  : string
    }
    with
        static member Create w e sl rs = { Warn = w; Error = e ; StopLocation = sl; RestString = rs }


type Unrecognized =  {
        Scalar      : int
        Collection  : int
    }
    with
        static member Create s c = { Scalar = s; Collection = c}

type TagReport = {
        Unresolved   : int
        Unrecognized : Unrecognized
        Unavailable  : int
    }
    with
        static member Create unrc unrs unav = { Unrecognized = unrc; Unresolved = unrs; Unavailable = unav}

type TagShorthand = {
        ShortHand : string
        MappedTagBase : string
    }

type ParsedDocumentResult = {
        Warn        : ParseMessageAtLine list
        TagReport   : TagReport
        StopLocation : DocumentLocation
        TagShorthands: TagShorthand list
        Document    : Node
    }
    with
        static member Create wm tr sl tsl d = {Warn = wm; TagReport = tr; StopLocation = sl; TagShorthands = tsl; Document = d}

type EmptyDocumentResult = {
        Warn        : ParseMessageAtLine list
        StopLocation : DocumentLocation
    }
    with
            static member Create wm sl = {Warn = wm; StopLocation = sl}

//  http://www.yaml.org/spec/1.2/spec.html#id2767381
type Representation =
    |   NoRepresentation of ErrorResult
    |   PartialRepresentaton of ParsedDocumentResult
    |   CompleteRepresentaton of ParsedDocumentResult
    |   EmptyRepresentation of EmptyDocumentResult

