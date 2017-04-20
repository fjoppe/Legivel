namespace YamlParser

open System
open System.Text
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

    //      =>  root node
    #       => match any scalar
    #'val'  => match scalar with value "val"
    {}      => mapping keys
    {}/     => mapping value(s)
    []      => seq
    []/     => seq values (in this case, same as [])

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

type EvaluationExpression =
    |   Map of MappingEvaluation
    |   Seq of SeqEvaluation
    |   Val of ScalarEvaluation
    with
        member this.filter nodes =
            match (this) with
            |   Map d   -> d.filter nodes
            |   Seq d   -> d.filter nodes
            |   Val d   -> d.filter nodes

type YamlPath = private {
        Start      : StartingPoint
        Evaluation : EvaluationExpression list
    }
    with
        static member sqrx =
            let y = Yaml12Parser()
            y.``nb-single-one-line``

        static member Create (s:string) =
            let start (s:string) = 
                if s.StartsWith("//") then (RootNode, s.Substring(2))
                else raise (YamlPathException "Yaml path must start with //")
            let (root, rs) = start s

            let parseEvals s =
                let plainscalar = RGS( RGP("#'") + GRP(YamlPath.sqrx)+ RGP("'")) + "$" //   "^#'(%s)'$" (YamlPath.sqrx)
                let mapkeyscalar = RGS( RGP(@"\{#'") + GRP(YamlPath.sqrx)+ RGP(@"'\}")) + "$" // sprintf "^\{#'(%s)'\}$" (YamlPath.sqrx)
                let mapvaluescalar = RGS( RGP(@"\{#'") + GRP(YamlPath.sqrx)+ RGP(@"'\}\?")) + "$" // sprintf "^\{#'(%s)'\}\?$" (YamlPath.sqrx)
                match s with
                | Regex(plainscalar)      [_; scalarValue]   -> Val(LiteralScalar scalarValue)
                | Regex(@"^#$")                      _               -> Val(AnyScalar)
                | Regex(@"^\{\}$")                   _               -> Map(AllKeys)
                | Regex(@"^\{\}\?$")                 _               -> Map(AllValues)
                | Regex(mapkeyscalar)   [_; scalarValue]   -> Map(GivenKey scalarValue)
                | Regex(mapvaluescalar) [_; scalarValue]   -> Map(MappedValueForKey scalarValue)
                | Regex(@"^\[\]$")               _                   -> Seq(SeqValues)
                | _  -> raise (YamlPathException (sprintf "Unsupported construct: %s" s))

            let evals = 
                rs.Split([|"/"|], StringSplitOptions.RemoveEmptyEntries)
                |> List.ofArray
                |> List.map(parseEvals)

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

