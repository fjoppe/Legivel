module YamlParser.Utilities.RegexDSL 

/// Contains a Regular Expression
type RGXType
type RGXType
    with
        static member (|||) : RGXType * RGXType -> RGXType
        static member (+) : RGXType * RGXType -> RGXType
        static member (-) : RGXType * RGXType -> RGXType


/// Regex pattern must repeat exactly given value, eg: Range(RGP("abc"), 2) := (abc){2}
val internal Repeat : RGXType * int -> RGXType

/// Regex pattern may repeat within given range, eg: Range(RGP("abc"), 2, 3) := (abc){2,3}
val internal Range : RGXType * int * int -> RGXType

/// Regex pattern may repeat zero or more, eg: ZOM(RGP("abc")) := (abc)*
val internal ZOM : RGXType -> RGXType

/// Regex pattern may repeat zero or more - nongreedy, eg: ZOM(RGP("abc")) := (abc)*
val internal ZOMNG : RGXType -> RGXType

/// Regex pattern may repeat once or more, eg: OOM(RGP("abc")) := (abc)+
val internal OOM : RGXType -> RGXType

/// Regex pattern may repeat once or more - non greedy, eg: OOM(RGP("abc")) := (abc)+
val internal OOMNG : RGXType -> RGXType

/// Make Regex optional, eg: OPT(RGP("abc")) := (abc)?
val internal OPT : RGXType -> RGXType

/// Plain regex pattern, eg: RGP("abc") := abc
val internal RGP : string  -> RGXType

/// One in Set regex pattern, eg: RGO("a-zA-Z") := [a-zA-Z]
val internal RGO : string  -> RGXType

/// Exclude Set regex pattern, eg: NOT(RGO("a-zA-Z")) := [^a-zA-Z]
val internal NOT : RGXType -> RGXType

/// Regex ToString - match from string start
val internal RGS : 'a -> string

/// Regex ToString - full string match
val internal RGSF : 'a -> string

/// Regex ToString - match anywhere in the string (FR = free)
val internal RGSFR : 'a -> string

/// Creates Regex group, eg GRP(RGP("abc")) := (abc)
val internal GRP : RGXType -> RGXType

/// Returns rest-string, where match 'm' is removed from source 's'
val internal Advance : string * string -> string


/// MatchResult of a regex match
type internal MatchResult = {
        FullMatch   : string
        Rest        : string
        Groups      : string list
    }
    with
        static member Create : string -> string -> string list -> MatchResult
        member ge1 : string with get 
        member ge2 : string*string with get 
        member ge3 : string*string*string with get 
        member ge4 : string*string*string*string with get 


/// Returns list of match groups, for pattern p on string s
val internal Match : string*'a -> string list

/// Returns whether pattern p matches on string s
val internal IsMatch : string*'a -> bool

/// Checks for matches of pattern p in string s.
/// If matched, returns (true, <match-string>, <rest-string>), otherwise (false, "",s)
val internal HasMatches : string*'a -> bool*string*string

/// Regex Active pattern to match string pattern on string input, and returns a list of matches
val internal (|Regex|_|) : string -> string -> string list option

/// Regex Active Pattern to match RGXType pattern on string input, and returns a match result
val internal (|Regex2|_|) : RGXType -> string -> MatchResult option

/// Converts string-literal encoded unicode characters as "\u0000" or "\U00000000" to the char they represent
val internal DecodeEncodedUnicodeCharacters : string -> string

/// Converts string-literal encoded hex characters as "\x00" to the char they represent
val internal DecodeEncodedHexCharacters  : string -> string

/// Converts uri encoded hex characters as "%00" to the char they represent
val internal DecodeEncodedUriHexCharacters : string -> string

/// Converts string-literal encoded escape characters as "\n" to the char they represent
val internal DecodeEncodedEscapedCharacters : string -> string

