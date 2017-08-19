namespace YamlParser

open RepresentationGraph

exception YamlPathException of string

type YamlPath
type YamlPath
    with
        member Select : Node -> Node list option

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module YamlPath =
    val Create : string -> YamlPath
