module Legivel.Utilities.RegexDSL 

open Legivel.Tokenizer

/// Contains a Regular Expression
type RGXType
type RGXType
    with
        static member (|||) : RGXType * RGXType -> RGXType
        static member (+) : RGXType * RGXType -> RGXType
        static member (-) : RGXType * RGXType -> RGXType


/// Regex pattern must repeat exactly given value, eg: Range(RGP("abc"), 2) := (abc){2}
val Repeat : RGXType * int -> RGXType

/// Regex pattern may repeat within given range, eg: Range(RGP("abc"), 2, 3) := (abc){2,3}
val Range : RGXType * int * int -> RGXType

/// Regex pattern may repeat zero or more, eg: ZOM(RGP("abc")) := (abc)*
val ZOM : RGXType -> RGXType

/// Regex pattern may repeat zero or more - nongreedy, eg: ZOM(RGP("abc")) := (abc)*
val ZOMNG : RGXType -> RGXType

/// Regex pattern may repeat once or more, eg: OOM(RGP("abc")) := (abc)+
val OOM : RGXType -> RGXType

/// Regex pattern may repeat once or more - non greedy, eg: OOM(RGP("abc")) := (abc)+
val OOMNG : RGXType -> RGXType

/// Make Regex optional, eg: OPT(RGP("abc")) := (abc)?
val OPT : RGXType -> RGXType

/// Plain regex pattern, eg: RGP("abc") := abc
val RGP : string  * Token list -> RGXType 

/// One in Set regex pattern, eg: RGO("a-zA-Z") := [a-zA-Z]
val RGO : string  * Token list -> RGXType

/// Exclude Set regex pattern, eg: NOT(RGO("a-zA-Z")) := [^a-zA-Z]
val NOT : RGXType -> RGXType

/// Regex ToString - match from string start
val RGS : 'a -> string

/// Regex ToString - full string match
val RGSF : 'a -> string

/// Regex ToString - match anywhere in the string (FR = free)
val RGSFR : 'a -> string

/// Creates Regex group, eg GRP(RGP("abc")) := (abc)
val GRP : RGXType -> RGXType

/// Returns rest-string, where match 'm' is removed from source 's'
val Advance : string * string -> string


//type ParseResult = {
//    Groups  : (TokenData list) list;
//    Match   : (TokenData list)
//}

//type ParseOutput = bool * ParseResult

/// Primary input assesment with Post-Parse condition. The condition is checked after each RGP token/char.
//val AssesInputPostParseCondition : (RollingStream<TokenData> * TokenData -> bool) -> RollingStream<TokenData> -> RGXType -> ParseOutput


type ParseResult = {
    IsMatch     : bool
    FullMatch   : char list
    Groups      : (char list) list
}

type NFAMachine

val rgxToNFA : rgx:RGXType -> NFAMachine

val parseIt : nfa:NFAMachine -> stream:RollingStream<TokenData> -> ParseResult

