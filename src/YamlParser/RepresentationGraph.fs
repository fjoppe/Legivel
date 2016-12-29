module RepresentationGraph

type TagScope =
    | Global
    | Local

type TagKind = 
    | Mapping
    | Sequence
    | Scalar

type Tag = {
        Scope   : TagScope
        Kind    : TagKind
        Uri     : string
        Short   : string
        Regex   : string
        canonFn : string -> string
    }
    with
        static member Create (scope, kind, uri, short, rgx, canon) =
            { 
                Scope = scope; 
                Kind = kind; 
                Uri = uri; 
                Short = short; 
                Regex = sprintf "^(%s)$" rgx
                canonFn = canon
            }

        static member Create (scope, kind, uri, short, rgx) =
            Tag.Create (scope, kind, uri, short, rgx, fun s -> s)

        static member Create (scope, kind, uri, short) =
            Tag.Create (scope, kind, uri, short, ".*", fun s -> s)

        static member Create (scope, kind, uri) =
            Tag.Create (scope, kind, uri, uri, ".*", fun s -> s)

        member this.Canonical s = this.canonFn s
        

type NodeData<'T> = {
        Tag  : Tag
        Data : 'T
    }
    with
        static member Create t d =
            { Tag = t; Data = d}

        member this.SetTag t = 
            { this with Tag = t}

type Node =
    | SeqNode of NodeData<Node list>
    | MapNode of NodeData<(Node*Node) list>
    | ScalarNode of NodeData<string>
    with
        member this.Indent l =
            [1 .. l] |> List.fold(fun s _ -> s + "  ") ""

        member this.ToCanonical l =
            match this with
            |   SeqNode n ->
                let ind0 = this.Indent l
                let head = sprintf "%s%s [\n" (ind0) (n.Tag.Short)
                let content = n.Data |> List.fold(fun s ni -> s + (sprintf "%s,\n" (ni.ToCanonical(l+1)))) ""
                let tail = sprintf "%s]\n" ind0
                sprintf "%s%s%s" head content tail
            |   MapNode n -> 
                let ind0 = this.Indent l
                let ind1 = this.Indent (l+1)
                let head = sprintf "%s%s {\n" (ind0) (n.Tag.Short)
                let content = 
                    n.Data 
                    |> List.fold(
                        fun s (k,v) -> 
                            let kc = k.ToCanonical(l+1)
                            let vc = v.ToCanonical(l+1)
                            match (k,v) with
                            |   (ScalarNode(_),ScalarNode(_))   -> s + sprintf "%s? %s\t: %s,\n" ind1 kc vc
                            |   _ -> s + sprintf "%s? %s\n%s: %s,\n" ind1 kc ind1 vc
                        ) ""
                let tail = sprintf "%s}\n" ind0
                sprintf "%s%s%s" head content tail
            |   ScalarNode n ->
                let ind0 = this.Indent l
                sprintf "%s%s \"%s\"" ind0 (n.Tag.Short) (n.Data)
        
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


type Legend = {
        YamlVersion : string
    }




