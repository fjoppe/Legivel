open System
open System.Text
open System.IO
open System.Text.RegularExpressions

type YamlScalarTag = string

type System.String with
    static member CreateCharRepeated n ch = System.String(Array.create n ch)

type CharacterStructure =
    |   Character of System.Char
    |   Range of System.Char * System.Char
    |   List of System.Char list

let ``convert-char-list`` chl =  chl |> List.map(fun e -> Character(e))

let rec ``is-charater-in-set`` ch st =
    let matchTerm =
        st |> List.filter(
            fun e ->
                match e with
                | Character mc     -> (mc = ch)
                | Range (mcl, mch) -> (mcl <= ch && mch >= ch)
                | List chl        -> ``is-charater-in-set`` ch (``convert-char-list`` chl)
        )
    (matchTerm.Length > 0)



//  Character set:  http://www.yaml.org/spec/1.2/spec.html#id2770814
let ``c-printable`` = 
    [
        Character('\u0009'); Character('\u000a'); Character('\u000d'); Range('\u0020', '\u007e') // 8 - bit, #x9 | #xA | #xD | [#x20-#x7E]
        Character('\u0085'); Range('\u00a0', '\ud7ff'); Range('\ue000', '\ufffd')                // 16- bit, #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD]
        //  32-bit -> currently not supported because .Net does not encode naturally. Yaml: [#x10000-#x10FFFF]
    ]

let ``nb-json`` =
    [
        Character('\u0009');Range('\u0020', '\uffff')
    ]

//  Indicator Characters:   http://www.yaml.org/spec/1.2/spec.html#id2772075
let [<Literal>] ``c-sequence-entry`` = '-'
let [<Literal>] ``c-mapping-key`` = '?'
let [<Literal>] ``c-mapping-value`` = ':'
let [<Literal>] ``c-collect-entry`` = ','
let [<Literal>] ``c-sequence-start`` = '['
let [<Literal>] ``c-sequence-end`` = ']'
let [<Literal>] ``c-mapping-start`` = '{'
let [<Literal>] ``c-mapping-end`` = '}'
let [<Literal>] ``c-comment`` = '#'
let [<Literal>] ``c-anchor`` = '#'
let [<Literal>] ``c-alias`` = '*'
let [<Literal>] ``c-tag`` = '!'
let [<Literal>] ``c-literal`` = '|'
let [<Literal>] ``c-folded`` = '>'
let [<Literal>] ``c-single-quote`` = '\''
let [<Literal>] ``c-double-quote`` = '\''
let [<Literal>] ``c-directive`` = '%'
let ``c-reserved`` = ['@'; '`']
let ``c-indicator`` = ['-' ; '?' ; ':' ; ',' ; '[' ; ']' ; '{' ; '}' ; '#' ; '&' ; '*' ; '!' ; ';' ; '>' ; ''' ; ''' ; '%' ; '@' ; '`' ]
let ``c-flow-indicator`` = [','; '['; ']'; '{'; '}']

//  Line break characters:  http://www.yaml.org/spec/1.2/spec.html#id2774608
let [<Literal>] ``b-line-feed`` = '\u000a'
let [<Literal>] ``b-carriage-return`` = '\u000d'
let ``b-char`` = [ ``b-line-feed`` ; ``b-carriage-return``]
let ``nb-char`` ch = 
    (``is-charater-in-set`` ch ``c-printable``)  &&
    not(``is-charater-in-set`` ch (``convert-char-list`` ``b-char``))
let ``b-break`` = [List([``b-carriage-return``;  ``b-line-feed``]); Character(``b-carriage-return``); Character(``b-line-feed``)]
let ``b-as-line-feed`` = ``b-break``
let ``b-non-content`` = ``b-break``

//  White space characters: http://www.yaml.org/spec/1.2/spec.html#id2775170
let [<Literal>] ``s-space`` = '\u0020'  // space
let [<Literal>] ``s-tab`` = '\u0009'    // tab
let ``s-white`` = [``s-space`` ; ``s-tab``]
let ``ns-char`` ch =
    (``nb-char`` ch)  &&
    not(``is-charater-in-set`` ch (``convert-char-list`` ``s-white`` ))


//  Misc characters:    http://www.yaml.org/spec/1.2/spec.html#id2775468
let ``ns-dec-digit`` = Range('\u0030', '\u0039')    //  0-9
let ``ns-hex-digit`` = [
    ``ns-dec-digit``
    Range('\u0041', '\u0046')   //  A-F
    Range('\u0061', '\u0066')   //  a-f
]

let ``ns-ascii-letter`` = 
    [
        Range('\u0041', '\u005A')   //  A-Z
        Range('\u0061', '\u007A')   //  a-z
    ]

let ``ns-word-char`` =
    [
        ``ns-dec-digit``
        Character('-')
    ] @ ``ns-ascii-letter``

let ``ns-uri-char`` =   // note spec indicates %XX as a hex match, which we can't do here
    [
        '%' ; '#'; 
        ';' ; '/' ; '?' ; ':' ; '@' ; '&' ; '=' ; '+' ; '$' ; ',';
        '_' ; '.' ; '!' ; '~' ; '*' ; ''' ; '(' ; ')' ; '[' ; ']'
    ]
    |> ``convert-char-list``
    |> List.append ``ns-word-char`` 

let ``ns-tag-char`` = 
    ``ns-uri-char``
    |> List.filter(fun c -> c <> Character('!'))
    |> List.filter(fun c -> not(``c-flow-indicator`` |> ``convert-char-list`` |> List.contains c))

//  Escaped characters: http://www.yaml.org/spec/1.2/spec.html#id2776092
let [<Literal>] ``c-escape`` = '\\'
let [<Literal>] ``ns-esc-null`` = '0'
let [<Literal>] ``ns-esc-bell`` = 'a'
let [<Literal>] ``ns-esc-backspace`` = 'b'
let [<Literal>] ``ns-esc-horizontal-tab`` = 't'
let [<Literal>] ``ns-esc-line-feed`` = 'n'
let [<Literal>] ``ns-esc-vertical-tab`` = 'v'
let [<Literal>] ``ns-esc-form-feed`` = 'f'
let [<Literal>] ``ns-esc-carriage-return`` = 'r'
let [<Literal>] ``ns-esc-escape`` = 'e'
let [<Literal>] ``ns-esc-space`` = '\u0020'
let [<Literal>] ``ns-esc-double-quote`` = '\"'
let [<Literal>] ``ns-esc-slash`` = '/'
let [<Literal>] ``ns-esc-backslash`` = '\\'
let [<Literal>] ``ns-esc-next-line`` = 'N'
let [<Literal>] ``ns-esc-non-breaking-space`` = '_'
let [<Literal>] ``ns-esc-line-separator`` = 'L'
let [<Literal>] ``ns-esc-paragraph-separator`` = 'P'
let [<Literal>] ``ns-esc-8-bit`` = 'x'
let [<Literal>] ``ns-esc-16-bit`` = 'u'
let [<Literal>] ``ns-esc-32-bit`` = 'U' // currently not supported

let ``c-ns-esc-char`` = [
    ``ns-esc-null`` ; ``ns-esc-bell`` ; ``ns-esc-backspace``
    ``ns-esc-horizontal-tab`` ; ``ns-esc-line-feed``
    ``ns-esc-vertical-tab`` ; ``ns-esc-form-feed``
    ``ns-esc-carriage-return`` ; ``ns-esc-escape`` ; ``ns-esc-space``
    ``ns-esc-double-quote`` ; ``ns-esc-slash`` ; ``ns-esc-backslash``
    ``ns-esc-next-line`` ; ``ns-esc-non-breaking-space``
    ``ns-esc-line-separator`` ; ``ns-esc-paragraph-separator``
    ``ns-esc-8-bit`` ; ``ns-esc-16-bit`` ; ``ns-esc-32-bit``
    ]

let ``s-indent`` n = System.String.CreateCharRepeated n ``s-space``





type Tag = {
    Name:   string;
    Kind:   string;
}

type Comment = string

type YamlNode = {
    Tags : Tag  list
    Data : YamlNodeType
}
    with
        static member Create tags data =
           { Tags = tags; Data = data}
and YamlNodeType =
    |   Sequence of YamlNode list
    |   Map of Map<YamlNode, YamlNode>
    |   Scalar of string
    |   Alias of string

type YamlDoc =
    |   Document of YamlNode

type YamlBlock = {
    blockType  : YamlNode
    indentation: int
}

let (|StartsWith|_|) start (text : string) = 
    if text.StartsWith(start) then Some() else None

let (|RegExGroup|_|) pattern input =
    let m = Regex.Match(input,pattern) 
    if (m.Success) then Some m.Groups.[1].Value else None

[<Literal>]
let Label = "[a-zA-Z]\w*"


let parseYaml (l : string list) =
    YamlNode.Create [] (Scalar "0.0")


let parseYamlDoc (lines : string list) =
    let retrieveDocument (lines : string list) =
        let rv lines = lines |> List.rev
        let rec seperateLines (document : string list) (rest : string list) =
            match rest with
            |   []      ->      (rv document, rest)    //  nothing left
            |   h :: t  ->
                match h with
                | StartsWith "---"  -> (rv document, rest) // starts new document
                | StartsWith "..."  -> (rv document, [])   // ... ends the stream
                | _ -> seperateLines (h :: document) t     // add line and continue
        seperateLines [] lines

    let rec parseYamlDocuments (lines : string list) (documents) =
        match lines with
        |   []      -> documents
        |   h :: t  ->
            match h with
            |   StartsWith("---") -> 
                let doc, rest = retrieveDocument t
                parseYamlDocuments rest (parseYaml doc :: documents)
            |   StartsWith("...") -> 
                documents
            |   _   -> 
                let doc, rest = retrieveDocument lines
                parseYamlDocuments rest (parseYaml doc :: documents)
    let cleanedLines = lines |> List.filter(fun line -> not(line.Trim().StartsWith("#")))
    parseYamlDocuments cleanedLines []


let yaml = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "example1.yaml"))
let lines = yaml.Split([|"\n"|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

let parsed = parseYamlDoc lines


let fs = File.OpenRead(Path.Combine(__SOURCE_DIRECTORY__, "example1.yaml"))
let b0 = fs.ReadByte()
let b1 = fs.ReadByte()



