module Deserialization

open RepresentationGraph
open TagResolution


let Deserialize (node:Node) (shorthands:TagShorthand list) =
    let indent l = [1 .. l] |> List.fold(fun s _ -> s + "  ") ""
    let tagString (lt:TagKind) =
        let strGlobal gt =
            shorthands
            |> List.tryFind(fun e -> gt.Uri.StartsWith(e.MappedTagBase)) 
            |> function
            |   Some mtg ->
                let rest = gt.Uri.Substring(mtg.MappedTagBase.Length) 
                mtg.ShortHand + rest
            |   None -> sprintf "!<%s>" gt.Uri

        match lt with
        |   Global gt  -> strGlobal gt
        |   Unrecognized gt -> strGlobal gt
//            shorthands
//            |> List.tryFind(fun e -> gt.Uri.StartsWith(e.MappedTagUri)) 
//            |> function
//            |   Some mtg ->
//                let rest = gt.Uri.Substring(mtg.MappedTagUri.Length) 
//                mtg.ShortHand + rest
//            |   None -> sprintf "!<%s>" gt.Uri
        |   Local       s -> s.Handle
        |   NonSpecific s -> s.Handle
    let rec convertToString n0 l =
        match n0 with
        |   SeqNode n ->
            let ind0 = indent l
            let head = sprintf "%s %s [\n" (ind0) (tagString n.Tag)
            let content = 
                n.Data
                |> List.fold(fun s ni -> s + (sprintf "%s,\n" (convertToString ni (l+1)))) ""
            let tail = sprintf "%s]" ind0
            sprintf "%s%s%s" head content tail
        |   MapNode n -> 
            let ind0 = indent l
            let ind1 = indent (l+1)
            let head = sprintf "%s %s {\n" (ind0) (tagString n.Tag)
            let content = 
                n.Data 
                |> List.fold(
                    fun s (k,v) -> 
                        let kc = convertToString k (l+1)
                        let vc = convertToString v (l+1)
                        match (k,v) with
                        |   (ScalarNode(_),ScalarNode(_))   -> s + sprintf "%s? %s\t: %s,\n" ind1 kc vc
                        |   _ -> s + sprintf "%s? %s\n%s: %s,\n" ind1 kc ind1 vc
                    ) ""
            let tail = sprintf "%s}" ind0
            sprintf "%s%s%s" head content tail
        |   ScalarNode n ->
            let ind0 = indent l
            sprintf "%s %s \"%s\"" ind0 (tagString n.Tag) (n.Data)
    convertToString node 0

