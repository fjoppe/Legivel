#I __SOURCE_DIRECTORY__ 
#I "../../packages"

#load @"RegexDSL.fsx"

#r @"bin/Debug/Legivel.Tokenizer.dll"

open System.Text
open Legivel.Tokenizer
open RegexDSL

let EndOfStream = TokenData.Create (Token.EOF) ""

let ``b-break`` = RGP("\n", [Token.NewLine])

let ``c-l-block-seq-entry`` = 
    RGP("-", [Token.``c-sequence-entry``]) + OOM(RGP(" ", [Token.``s-space``])) + OOM(RGO("\u0009\u0020-\uffff", [Token.``c-printable``; Token.``nb-json``; Token.``c-sequence-entry``; Token.``ns-dec-digit``]))

let ``l+block-sequence`` =
    OOM(``c-l-block-seq-entry`` + ``b-break``)

let AssesInput (rs:RollingStream<TokenData>) (rg:RGXType) =
    let rec parse rgx tkl =
        let conditionalParse rgx tk =
            let p = rs.Position
            let r = parse rgx tk 
            if not(fst(r)) then rs.Position <- p
            r

        let mkResult t tkl =
            function 
            | true ->  (true, t :: tkl) 
            | false -> (false, tkl)

        let rec repeatWhileTrue t acc =
            let pr = conditionalParse t tkl
            if (fst pr) then repeatWhileTrue t (snd pr @ acc)
            else acc |> List.rev

        match rgx with
        |   OneInSet ois    -> rs.Stream |> Seq.take 1 |> Seq.head |> fun i -> ois.Token |> List.exists(fun e -> e=i.Token) |> mkResult i tkl
        |   Plain pl        -> rs.Stream |> Seq.take 1 |> Seq.head |> fun i -> pl.Token  |> List.exists(fun e -> e=i.Token) |> mkResult i tkl
        |   Or rl           -> 
            let rec pickFirst l =
                match l with
                |   h::tl -> 
                    let rs = parse h tkl
                    if fst(rs) then (true, snd rs @ tkl)
                    else pickFirst tl
                |   [] -> (false, tkl)
            rl |> List.rev |> pickFirst
        |   Concat rl       -> 
            let rec pickAll acc l =
                match l with
                |   h::tl -> 
                    let rs = parse h tkl
                    if fst(rs) then pickAll (snd rs @ acc) tl
                    else (false, tkl)
                |   [] -> (true, acc)
            rl |> List.rev |> pickAll []
        |   IterRange (t,_,mno) -> if mno.IsSome && mno.Value > 0 then parse (OneOrMore t) tkl else parse (ZeroOrMore t) tkl
        |   ZeroOrMore t        -> true, repeatWhileTrue t tkl
        |   ZeroOrMoreNonGreedy t -> true, repeatWhileTrue t tkl
        |   OneOrMore t         ->  repeatWhileTrue t tkl |> fun l -> (l.Length>=1), l
        |   OneOrMoreNonGreedy t -> repeatWhileTrue t tkl |> fun l -> (l.Length>=1), l
        |   Optional t          -> true, conditionalParse t tkl |> snd
        |   Group t             -> parse t tkl
    parse rg []



//let yaml = "- 5
//- 10
//- -9
//"

let yaml = "- 5
- 10
- -9 # mismatch here
"

let stream = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream

#time

let (b, tkl) = AssesInput stream ``l+block-sequence``

let parseString = (tkl |> List.map(fun td -> td.Source) |> List.fold(fun (str:StringBuilder) i -> str.Append(i)) (StringBuilder())).ToString()

stream 
