module Legivel.Utilities.Regex.PatternMatch

open Legivel.Tokenizer
open Legivel.Utilities.RegexDSL

/// Primary input assesment, rough input match (at token-level), if match, return the input that matched
val AssesInput : RollingStream<TokenData> -> NFAMachine -> ParseResult

/// Matched tokens to matched string
val TokenDataToString : ParseResult -> string option

/// MatchResult of a regex match
type MatchResult = {
        FullMatch   : string
        Groups      : string list
    }
    with
        static member Create : string -> string list -> MatchResult
        member ge1 : string with get 
        member ge2 : string*string with get 
        member ge3 : string*string*string with get 
        member ge4 : string*string*string*string with get 


/// Returns whether pattern p matches on string s
val IsMatch : RollingStream<TokenData> * NFAMachine -> bool


/// Checks for matches of pattern p in string s.
/// If matched, returns (true, <match-string>), otherwise (false, "")
val HasMatches : RollingStream<TokenData>*NFAMachine -> bool*string

val charListToString : char list -> string 

///// Regex Active Pattern to match RGXType pattern on string input, and returns a match result
//val (|Regex2|_|) : RGXType -> RollingStream<TokenData> -> MatchResult option

/// Converts string-literal encoded unicode characters as "\u0000" or "\U00000000" to the char they represent
val DecodeEncodedUnicodeCharacters : string -> string

/// Converts string-literal encoded hex characters as "\x00" to the char they represent
val DecodeEncodedHexCharacters  : string -> string

/// Converts uri encoded hex characters as "%00" to the char they represent
val DecodeEncodedUriHexCharacters : string -> string

/// Converts string-literal encoded escape characters as "\n" to the char they represent
val DecodeEncodedEscapedCharacters : string -> string


