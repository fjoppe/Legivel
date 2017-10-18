module Legivel.Parser

open RepresentationGraph
open TagResolution


/// Main type for Yaml 1.2 parsing
type Yaml12Parser =
    class
        /// Constructor with logging - the parser logs to the given function
        new : (string->unit) -> Yaml12Parser

        /// Constructor for plain simple yaml parsing
        new : unit -> Yaml12Parser

        /// Main entry point for yaml parsing
        /// http://www.yaml.org/spec/1.2/spec.html#l-yaml-stream
        member ``l-yaml-stream`` : GlobalTagSchema -> string -> Representation list
    end
