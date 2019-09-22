module Legivel.Utilities.Regex.PatternMatch

open Legivel.Tokenizer
open Legivel.Utilities.RegexDSL
open System.Text.RegularExpressions
open System.Diagnostics
open System
open System.Text
open System.Globalization


let AssesInput (rs:RollingStream<TokenData>) (nfa:NFAMachine) =  parseIt nfa rs // AssesInputPostParseCondition (fun _ -> true) rs rg


let TokenDataToString =
    function
    |   tkl when tkl.IsMatch -> (tkl.FullMatch |> List.fold(fun (str:StringBuilder) i -> str.Append(i)) (StringBuilder())).ToString() |> Some
    |   _ -> None


type MatchResult = {
        FullMatch   : string
        Groups      : string list
    }
    with
        static member Create f g = { FullMatch = f; Groups = g }
        member this.ge1 with get() = (this.Groups.[1])
        member this.ge2 with get() = (this.Groups.[1], this.Groups.[2])
        member this.ge3 with get() = (this.Groups.[1], this.Groups.[2], this.Groups.[3])
        member this.ge4 with get() = (this.Groups.[1], this.Groups.[2], this.Groups.[3], this.Groups.[4])


/// Returns whether pattern p matches on string s
[<DebuggerStepThrough>]
let IsMatch(s:RollingStream<TokenData>, p) = 
    let pos = s.Position
    AssesInput s p
    |> fun res -> 
        s.Position <- pos
        res.IsMatch


/// Checks for matches of pattern p in string s.
/// If matched, returns (true, <match-string>, <rest-string>), otherwise (false, "",s)
[<DebuggerStepThrough>]
let HasMatches(s,p) = 
    let fr =  AssesInput s p
    if not fr.IsMatch then
        (false, String.Empty)
    else
        fr.FullMatch
        |> List.fold(fun (str:StringBuilder) i -> str.Append(i)) (StringBuilder())
        |> fun sb -> (true, sb.ToString())

let charListToString cl = cl |> List.fold(fun (sb:StringBuilder) (c:char) -> sb.Append(c)) (new StringBuilder()) |> fun sb -> sb.ToString()


//[<DebuggerStepThrough>]
//let (|Regex2|_|) (nfa:NFAMachine) (input:RollingStream<TokenData>) =
//    let p = input.Position
//    AssesInput input nfa
//    |> TokenDataToString
//    |>  Option.bind(fun mts ->
//        let m = Regex.Match(mts, RGS(pattern), RegexOptions.Multiline)
//        if m.Success then 
//            let lst = [ for g in m.Groups -> g.Value ]
//            let fullMatch = lst |> List.head
//            let groups = lst |> List.tail
//            Some(MatchResult.Create fullMatch groups)
//        else 
//            failwith "Difference between assesinput and regex"
//    )
//    |>  function
//        |   None -> input.Position <- p;None
//        |   Some x -> Some x

let DecodeEncodedUnicodeCharacters value =
    Regex.Replace(value,
        @"(\\u(?<Value>[a-zA-Z0-9]{4}))|(\\U(?<Value>[a-zA-Z0-9]{8}))",
        (fun (m:Match) -> (char(Int64.Parse(m.Groups.["Value"].Value, NumberStyles.HexNumber))).ToString()))

let DecodeEncodedHexCharacters value =
    Regex.Replace(value,
        @"\\x(?<Value>[a-fA-F0-9]{2})",
        (fun (m:Match) -> (char(Int32.Parse(m.Groups.["Value"].Value, NumberStyles.HexNumber))).ToString()))

let DecodeEncodedUriHexCharacters value =
    Regex.Replace(value,
        @"%(?<Value>[a-fA-F0-9]{2})",
        (fun (m:Match) -> (char(Int32.Parse(m.Groups.["Value"].Value, NumberStyles.HexNumber))).ToString()))
    
let DecodeEncodedEscapedCharacters value =
    Regex.Replace(value,
        @"\\(?<Value>[0abtnvfre ""/N_LP])",
        (fun (m:Match) -> 
            match (m.Groups.["Value"].Value) with
            |   "0" -> "\x00"
            |   "a" -> "\a"
            |   "b" -> "\b"
            |   "t" -> "\t"
            |   "n" -> "\n"
            |   "v" -> "\v"
            |   "f" -> "\f"
            |   "r" -> "\r"
            |   "e" -> "\x1b"
            |   " " -> " "
            |   "\"" -> "\""
            |   "/" -> "\x2f"
            |   "N" -> "\u0085"
            |   "_" -> "\u00a0"
            |   "L" -> "\u2028"
            |   "P" -> "\u2029"
            | _ -> sprintf "\\%s" m.Groups.["Value"].Value
        ))




