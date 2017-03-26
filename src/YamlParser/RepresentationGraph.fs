module RepresentationGraph

type NodeKind = 
    | Mapping
    | Sequence
    | Scalar

type Tag = {
        Uri     : string
        Regex   : string
        canonFn : string -> string
    }
    with
        static member Create (uri, rgx, canon) =
            { 
                Uri = uri; 
                Regex = sprintf "\\A(%s)\\z" rgx
                canonFn = canon
            }

        static member Create (uri, rgx) = Tag.Create (uri, rgx, fun s -> s)

        static member Create (uri) = Tag.Create (uri, ".*", fun s -> s)

        member this.Canonical s = this.canonFn s


open YamlParser.Internals


type NodeData<'T> = {
        Tag  : Tag
        Data : 'T
        Hash : Lazy<NodeHash>
    }
    with
        static member Create t d h =
            { Tag = t; Data = d; Hash = h}

        member this.SetTag t = 
            { this with Tag = t}

type Node =
    | SeqNode of NodeData<Node list>
    | MapNode of NodeData<(Node*Node) list>
    | ScalarNode of NodeData<string>
    with
        member this.Hash 
            with get() =
                match this with
                |   SeqNode n       -> n.Hash
                |   MapNode n       -> n.Hash
                |   ScalarNode n    -> n.Hash
        
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

        member this.Kind
            with get() =
                match this with
                |   SeqNode _       -> Sequence
                |   MapNode _       -> Mapping
                |   ScalarNode _    -> Scalar

type Legend = {
        YamlVersion : string
    }




