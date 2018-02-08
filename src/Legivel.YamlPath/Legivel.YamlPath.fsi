namespace Legivel.Traverse

open Legivel.RepresentationGraph

/// Thrown when there are YamlPath related issues
exception YamlPathException of string

/// Contains a data-query, which may be applied to a Node
type YamlPath
type YamlPath
    with
        /// <summary>Applies the query to the given Yaml Node, and optionally returns a list of Nodes which comply</summary>
        /// <param name="node">Root node to apply the YamlPath to</param>
        /// <returns>Optionally a list of Yaml nodes which comply to the query</returns>
        member Select : Node -> Node list option

/// Contains functions applied to the YamlPath type
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module YamlPath =

    /// <summary>Creates a new YamlPath, based on the given query</summary>
    /// <param name="query">Defines a YamlPath query, pointing at the location of interest in the Yaml structure</param>
    /// <returns>YamlPath instance with the compiled qurey</returns>
    /// <exception cref="YamlPathException">Thrown when the given query is invalid</exception>
    val Create : query: string -> YamlPath
