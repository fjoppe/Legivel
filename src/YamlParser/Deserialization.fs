module Deserialization

open RepresentationGraph
open TagResolution


let Deserialize (node:Node) (shorthands:TagShorthand list) =
    let indent l = [1 .. l] |> List.fold(fun s _ -> s + "  ") ""
    let shortTag (lt:string) =
        shorthands
        |> List.tryFind(fun e -> lt.StartsWith(e.MappedTagUri)) 
        |> function
        |   Some mtg ->
            let rest = lt.Substring(mtg.MappedTagUri.Length) 
            mtg.ShortHand + rest
        |   None -> sprintf "!<%s>" lt
    let rec convertToString n0 l =
        match n0 with
        |   SeqNode n ->
            let ind0 = indent l
            let head = sprintf "%s %s [\n" (ind0) (shortTag n.Tag.Uri)
            let content = 
                n.Data
                |> List.sortBy(fun n -> n.Hash.Value)
                |> List.fold(fun s ni -> s + (sprintf "%s,\n" (convertToString ni (l+1)))) ""
            let tail = sprintf "%s]\n" ind0
            sprintf "%s%s%s" head content tail
        |   MapNode n -> 
            let ind0 = indent l
            let ind1 = indent (l+1)
            let head = sprintf "%s %s {\n" (ind0) (shortTag n.Tag.Uri)
            let content = 
                n.Data 
                |> List.sortBy(fun (k,_) -> k.Hash.Value)
                |> List.fold(
                    fun s (k,v) -> 
                        let kc = convertToString k (l+1)
                        let vc = convertToString v (l+1)
                        match (k,v) with
                        |   (ScalarNode(_),ScalarNode(_))   -> s + sprintf "%s? %s\t: %s,\n" ind1 kc vc
                        |   _ -> s + sprintf "%s? %s\n%s: %s,\n" ind1 kc ind1 vc
                    ) ""
            let tail = sprintf "%s}\n" ind0
            sprintf "%s%s%s" head content tail
        |   ScalarNode n ->
            let ind0 = indent l
            sprintf "%s %s \"%s\"" ind0 (shortTag n.Tag.Uri) (n.Data)
    convertToString node 0

