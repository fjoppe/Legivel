namespace YamlParser

open RegexDSL
open RepresentationGraph
open YamlParse

exception YamlPathException of string

(*
    Spec by example:
        //#                        -> if root node is scalar, retrieve it (scalar node)
        //#'scalar value'          -> if root node is scalar with value "scalar value", retrieve it (scalar node)
        //{}                       -> if root node is mapping, retrieve all its keys (Yaml Node list)
        //{}?                      -> if root node is mapping, retrieve all its values (Yaml Node list)
        //{#'key1'}                -> if root node is mapping, with key "key1", retrieve this key (Yaml Node)
        //{#'key1'}?               -> if root node is mapping, with key "key1", retrieve its value (Yaml Node)
        //{#'key1'}?/#             -> if root node is mapping, with key "key1", and its value is a scalar, retrieve its value (scalar)
        //{#'key1'}?/#'scalar value'-> if root node is mapping, with key "key1", and its value is scalar "scalar value", retrieve it (scalar)
        //{#'key1'}?/[]             -> if root node is mapping, with key "key1", and its value is a seq, retrieve its value (Yaml Node)
        //[]                       -> if root node is seq, then retrieve its value (Yaml seq Node list)
        //[]/#                     -> if root node is seq, then retrieve its scalar values (Yaml Node list)
        //[]/#'scalar value'       -> if root node is seq, which contains scalar "scalar value", then retrieve all "scalar value" (Yaml Node list)
        //[]/{}                    -> if root node is seq, then retrieve its mapping nodes (Yaml seq Node list)
        //[]/{key1}                -> if root node is seq, then retrieve its mapping nodes which have key "key1" (Yaml seq Node list)
        //[]/[]                    -> if root node is seq, then retrieve seq nodes from its values (Yaml seq Node list)
        //[]/[]/#'scalar value'    -> if root node is seq, then retrieve seq nodes from its values, of which all scalar nodes with scalar "scalar value" are retrieved (scalar list)

        //[:#]                     -> if root node is seq, then retrieve its scalar nodes (Yaml seq Node list)
        //[:#'scalar value']       -> if root node is seq, then retrieve its scalar nodes with value "scalar value" (Yaml seq Node list) (may be extended with wildcard search)
        //[:{}]                    -> if root node is seq, then retrieve its mapping nodes (Yaml seq Node list)
        //[:{key1}]                -> if root node is seq, then retrieve its mapping nodes which have key "key1" (Yaml seq Node list)
        //[:[]]                    -> if root node is seq, then retrieve its seq nodes, which have at least 1 element (Yaml Node list)
        //[:[:#'scalar value']]    -> if root node is seq, then retrieve its seq nodes, which contain scalar value "scalar value" (Yaml Node list)
        //<tag:sometag>            -> filter root node on tag (ie returns None if not found)
        //[]/<tag:sometag>         -> if root node is seq, then retrieve its seq nodes, and filter nodes with tag "tag:sometag"

    //      =>  root node
    #       => match any scalar
    #'val'  => match scalar with value "val"
    {}      => mapping keys
    {}/     => mapping value(s)
    []      => seq
    []/     => seq values (in this case, same as [])
    <!abc>  => match any node with tag "!abc" (you can use global and local tags)

    So
    //a/b/c => 
        c works on the selection of b
        b works on the selection of a
        / on mapping works on selected key or value
        / on filtered seq, works on filtered subset
*)

type StartingPoint =
    |   RootNode

type ScalarEvaluation = 
    |   AnyScalar               //  #
    |   LiteralScalar of string //  #'literal value'
    with
        member this.filter nodes  =
            nodes 
            |> List.filter (fun node ->
                match (this, node) with
                |   (AnyScalar, ScalarNode(_))  -> true
                |   ((LiteralScalar l1), ScalarNode(n1)) -> l1 = n1.Data
                |   _ -> false
            )
            |> fun lst ->
                if lst.Length = 0 then None
                else (Some lst)


type MappingEvaluation = 
    |   AllKeys
    |   GivenKey of string    //  should be any node type, so this is just for start
    |   AllValues
    |   MappedValueForKey of string
    with
        member private this.ScalarSelect node sel =
            match node with
            |    ScalarNode data -> data.Data = sel
            |   _ -> false

        member this.filter nodes =
            nodes 
            |> List.filter (fun node ->
                match (this, node) with
                |   (AllKeys,               (MapNode _)) -> true
                |   ((GivenKey sel),        (MapNode data)) ->  data.Data |> List.exists (fun (k,_) -> this.ScalarSelect k sel)
                |   (AllValues,             (MapNode _)) -> true
                |   (MappedValueForKey sel, (MapNode data)) ->  data.Data |> List.exists (fun (k,_) -> this.ScalarSelect k sel)
                |   _ -> false
            )
            |> List.map (fun node ->
                match (this, node) with
                |   (AllKeys,               (MapNode data)) -> data.Data |> List.map(fun (k,_) -> k) 
                |   ((GivenKey sel),        (MapNode data)) -> data.Data |> List.map(fun (k,_) -> k) |> List.filter (fun k -> this.ScalarSelect k sel)
                |   (AllValues,             (MapNode data)) -> data.Data |> List.map(fun (_,v) -> v)
                |   (MappedValueForKey sel, (MapNode data)) -> data.Data |> List.filter (fun (k, _) -> this.ScalarSelect k sel) |> List.map(fun (_,v) -> v)
                |   _ -> []
            )
            |> List.collect (fun v -> v)
            |> fun lst ->
                if lst.Length = 0 then None
                else (Some lst)

type SeqEvaluation =
    |   SeqValues
    with
        member this.filter nodes =
            nodes 
            |> List.filter (fun node ->
                match (this, node) with
                |   (SeqValues,  (SeqNode _)) -> true
                |   _ -> false
            )
            |> List.map (fun node -> 
                match (this, node) with
                |   (SeqValues,  (SeqNode data)) -> data.Data   
                |   _ -> []
            )
            |> List.collect (fun v -> v)
            |> fun lst ->
                if lst.Length = 0 then None
                else (Some lst)

type TagEvaluation =
    |   TagValue of string
    with
        member this.extractTag n = 
            let extract nd =
                match nd.Tag with
                |   Global gt   -> gt.Uri
                |   Unrecognized gt -> gt.Uri
                |   Local  s    -> s
                |   NonSpecific s -> s
            match n with
            |   ScalarNode nd -> extract nd
            |   SeqNode nd -> extract nd
            |   MapNode nd -> extract nd

        member this.filter nodes =
            nodes 
            |> List.filter (fun node ->
                let nodeTag = this.extractTag node
                let (TagValue filterTag) = this
                nodeTag = filterTag
            )
            |> fun lst ->
                if lst.Length = 0 then None
                else (Some lst)

type EvaluationExpression =
    |   Map of MappingEvaluation
    |   Seq of SeqEvaluation
    |   Val of ScalarEvaluation
    |   Tag of TagEvaluation
    with
        member this.filter nodes =
            match (this) with
            |   Map d   -> d.filter nodes
            |   Seq d   -> d.filter nodes
            |   Val d   -> d.filter nodes
            |   Tag d   -> d.filter nodes

type YamlPath = private {
        Start      : StartingPoint
        Evaluation : EvaluationExpression list
    }
    with
        static member ``single quote regex`` =
            let y = Yaml12Parser()
            let ps = ParseState.Create "" (TagResolution.FailsafeSchema)
            y.``nb-single-multi-line`` ps

        static member tagrx =
            let y = Yaml12Parser()
//            let ps = ParseState.Create "" (TagResolution.FailsafeSchema)
            OOM(y.``ns-uri-char``) ||| y.``c-primary-tag-handle`` + OOM(y.``ns-tag-char``)


        static member Create (s:string) =
            let start (s:string) = 
                if s.StartsWith("//") then (RootNode, s.Substring(1))
                else raise (YamlPathException "Yaml path must start with //")
            let (root, rs) = start s

            let parseEvals s =
                let plainscalar = RGP("#'") + GRP(YamlPath.``single quote regex``)+ RGP("'") 
                let mapkeyscalar = RGP(@"\{#'") + GRP(YamlPath.``single quote regex``)+ RGP(@"'\}") 
                let mapvaluescalar = RGP(@"\{#'") + GRP(YamlPath.``single quote regex``)+ RGP(@"'\}\?")
                let tagvalue = (RGP "<") + GRP(YamlPath.tagrx) + (RGP ">")
                match s with
                |   Regex2(plainscalar)     mt -> Val(LiteralScalar (mt.ge1)),mt.Rest
                |   Regex2(RGP("#"))        mt -> Val(AnyScalar),mt.Rest
                |   Regex2(RGP(@"\{\}\?"))  mt -> Map(AllValues),mt.Rest
                |   Regex2(RGP(@"\{\}"))    mt -> Map(AllKeys),mt.Rest
                |   Regex2(mapvaluescalar)  mt -> Map(MappedValueForKey mt.ge1),mt.Rest
                |   Regex2(mapkeyscalar)    mt -> Map(GivenKey mt.ge1),mt.Rest
                |   Regex2(RGP(@"\[\]"))    mt -> Seq(SeqValues),mt.Rest
                |   Regex2(tagvalue)        mt -> Tag(TagValue mt.ge1),mt.Rest
                | _  -> raise (YamlPathException (sprintf "Unsupported construct: %s" s))

            let evals = 
                let rec parseIt str evs =
                    if str = "" then
                        evs |> List.rev
                    else
                        if not(str.StartsWith "/") then raise (YamlPathException (sprintf "Invalid construct, expecting '/' but was: %s" str))
                        let str  = str.Substring(1)
                        let (e,r) = parseEvals str
                        parseIt (r) (e :: evs)
                parseIt rs []

            //  Validate: scalars may only appear at the end of an Ypath string
            evals |> List.rev |> List.tail |> List.iter(fun e ->
                match e with
                |   Val _ -> raise (YamlPathException "Scalars may only appear as last operation")
                |   _   -> ()
            )

            { Start = root; Evaluation = evals}
            
        member this.Select (nd: Node) : Node list option =
            this.Evaluation
            |>  List.fold(fun n e ->
                match n with
                |   Some (nodeList) -> 
                    e.filter nodeList
                |   None -> None
            ) (Some [nd])

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module YamlPath =
    let Create s = YamlPath.Create s

