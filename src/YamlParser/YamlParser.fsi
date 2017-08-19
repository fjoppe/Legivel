module YamlParse

open RepresentationGraph
open TagResolution

type Yaml12Parser =
    class
        new : (string->unit) -> Yaml12Parser
        new : unit -> Yaml12Parser
        member ``l-yaml-stream`` : GlobalTagSchema -> string -> Representation list
    end
