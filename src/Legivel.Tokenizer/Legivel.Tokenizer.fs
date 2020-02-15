module Legivel.Tokenizer

open System.IO
open System.Collections.Generic
open System.Collections.Concurrent

type RSList<'a> = System.Collections.Generic.List<'a>

type Token =
    |   ``t-space``                 = 0b0000_0000_0000_0000_0000_0000_0000_0001
    |   ``t-tab``                   = 0b0000_0000_0000_0000_0000_0000_0000_0010
    |   NewLine                     = 0b0000_0000_0000_0000_0000_0000_0000_0100
    |   ``c-printable``             = 0b0000_0000_0000_0000_0000_0000_0000_1000
    |   ``t-hyphen``                = 0b0000_0000_0000_0000_0000_0000_0001_0000
    |   ``t-plus``                  = 0b0000_0000_0000_0000_0000_0000_0010_0000
    |   ``t-questionmark``          = 0b0000_0000_0000_0000_0000_0000_0100_0000
    |   ``t-colon``                 = 0b0000_0000_0000_0000_0000_0000_1000_0000
    |   ``t-dot``                   = 0b0000_0000_0000_0000_0000_0001_0000_0000
    |   ``t-square-bracket-start``  = 0b0000_0000_0000_0000_0000_0010_0000_0000
    |   ``t-square-bracket-end``    = 0b0000_0000_0000_0000_0000_0100_0000_0000
    |   ``t-curly-bracket-start``   = 0b0000_0000_0000_0000_0000_1000_0000_0000
    |   ``t-curly-bracket-end``     = 0b0000_0000_0000_0000_0001_0000_0000_0000
    |   ``t-hash``                  = 0b0000_0000_0000_0000_0010_0000_0000_0000
    |   ``t-ampersand``             = 0b0000_0000_0000_0000_0100_0000_0000_0000
    |   ``t-asterisk``              = 0b0000_0000_0000_0000_1000_0000_0000_0000
    |   ``t-quotationmark``         = 0b0000_0000_0000_0001_0000_0000_0000_0000
    |   ``t-pipe``                  = 0b0000_0000_0000_0010_0000_0000_0000_0000
    |   ``t-gt``                    = 0b0000_0000_0000_0100_0000_0000_0000_0000
    |   ``t-single-quote``          = 0b0000_0000_0000_1000_0000_0000_0000_0000
    |   ``t-double-quote``          = 0b0000_0000_0001_0000_0000_0000_0000_0000
    |   ``t-percent``               = 0b0000_0000_0010_0000_0000_0000_0000_0000
    |   ``t-commat``                = 0b0000_0000_0100_0000_0000_0000_0000_0000
    |   ``t-tick``                  = 0b0000_0000_1000_0000_0000_0000_0000_0000
    |   ``t-forward-slash``         = 0b0000_0001_0000_0000_0000_0000_0000_0000
    |   ``t-equals``                = 0b0000_0010_0000_0000_0000_0000_0000_0000
    |   ``nb-json``                 = 0b0000_0100_0000_0000_0000_0000_0000_0000
    |   ``ns-dec-digit``            = 0b0000_1000_0000_0000_0000_0000_0000_0000
    |   ``c-escape``                = 0b0001_0000_0000_0000_0000_0000_0000_0000
    |   ``t-comma``                 = 0b0010_0000_0000_0000_0000_0000_0000_0000
    |   ``byte-order-mark``         = 0b0100_0000_0000_0000_0000_0000_0000_0000
    //  these aren't used in regex:
    |   Other                       = 0b0100_0000_0000_0000_0000_0000_0000_0001
    |   EOF                         = 0b0100_0000_0000_0000_0000_0000_0000_0010
    |   NoToken                     = 0b0100_0000_0000_0000_0000_0000_0000_0011

type TokenData = {
        Token   : Token
        Source  : string
    }
    with
        static member Create t s = { Token = t; Source = s }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TokenData =
    let token td = td.Token
    let source td = td.Source

open TokenData

let symbolStr = "-+?:,.[]{}#&*!|>\'\"%@`/=\ufeff".ToCharArray()

let tokenizer str = 
    let strm = new StringReader(str)

    let rec charCount cnd cnt =
        let chri = strm.Peek()
        if chri < 0 then cnt
        else
            let chr = char(chri)
            if cnd(chr) then
                strm.Read() |> ignore
                charCount cnd (cnt+1)
            else
                cnt

    let rec charRead (acc:char) = acc.ToString()
    
    let isSpace = (fun c -> c = ' ')
    let isTab = (fun c -> c = '\t')
    let isDigit = (fun c -> c >= '0' && c <= '9')
    let isEscape = (fun c -> c = '\\')
    let isWhite = (fun c -> c = ' ' || c = '\t')
    let isNewLine = (fun c -> c = '\x0a' || c = '\x0d')
    let isSymbol = (fun c -> symbolStr |> Array.exists(fun sc -> sc = c))
    
    //  c-printable
    let isText = (fun c ->
        [isWhite; isTab;isDigit;isNewLine;isSymbol;isEscape]
        |>  List.forall(fun cnd -> cnd c |> not) && 
            (("\x09\x0a\x0d\x85".Contains(string c)) ||
            (c >= '\x20' && c <= '\x7e') ||
            (c >= '\xa0' && c <= '\ud7ff') ||
            (c >= '\ue000' && c <= '\ufffd'))
        )

    let isJson = (fun c -> c = '\x09' || ((c >= '\x20' && c <= '\uffff')))

    let ``c-indicator`` c =
        match c with
        |   '-' -> Token.``t-hyphen``
        |   '+' -> Token.``t-plus``
        |   '?' -> Token.``t-questionmark``
        |   ':' -> Token.``t-colon``
        |   ',' -> Token.``t-comma``
        |   '.' -> Token.``t-dot``
        |   '[' -> Token.``t-square-bracket-start``
        |   ']' -> Token.``t-square-bracket-end``
        |   '{' -> Token.``t-curly-bracket-start``
        |   '}' -> Token.``t-curly-bracket-end``
        |   '#' -> Token.``t-hash``
        |   '&' -> Token.``t-ampersand``
        |   '*' -> Token.``t-asterisk``
        |   '!' -> Token.``t-quotationmark``
        |   '|' -> Token.``t-pipe``
        |   '>' -> Token.``t-gt``
        |   '\'' -> Token.``t-single-quote``
        |   '\"' -> Token.``t-double-quote``
        |   '%' -> Token.``t-percent``
        |   '@' -> Token.``t-commat``
        |   '`' -> Token.``t-tick``
        |   '/' -> Token.``t-forward-slash``
        |   '=' -> Token.``t-equals``
        |   '\ufeff' -> Token.``byte-order-mark``
        |   _ -> failwith "Unrecognized symbol"

    let reader() = 
        let chri = strm.Read()
        if chri < 0 then 
            (TokenData.Create Token.EOF "\x00")
        else
            let chr = char(chri)
            [
                (isSpace, fun() -> TokenData.Create Token.``t-space`` (charRead chr))
                (isTab, fun() -> TokenData.Create Token.``t-tab`` (charRead chr))
                (isNewLine, fun() -> TokenData.Create Token.NewLine (string chr))
                (isEscape, fun() -> TokenData.Create Token.``c-escape`` (string chr))
                (isSymbol, fun() -> TokenData.Create (``c-indicator`` chr) (string chr))
                (isDigit, fun() -> TokenData.Create Token.``ns-dec-digit`` (charRead chr))
                (isText, fun()-> TokenData.Create Token.``c-printable`` (charRead chr))
                (isJson, fun()-> TokenData.Create Token.``nb-json`` (charRead chr))
                ((fun _ -> true), fun() -> TokenData.Create Token.Other (string chr))
            ]
            |>  List.skipWhile(fun (c,_) -> c chr |> not)
            |>  List.head
            |>  fun (_,p) -> p()

    (fun () -> reader())


let tokenProcessor str =
    let tkn = tokenizer str

    let processed = Queue<TokenData>()
    let todo = Stack<TokenData>()

    let getToken() = if todo.Count = 0 then tkn() else todo.Pop()
    let tokenTakeTwo() = Array.init 2 (fun _ -> getToken())
    let enqueueProcessed lst = lst |> List.iter processed.Enqueue

    let ``Try conversion DOS/Windows break``() =
        let tl = tokenTakeTwo()
        if tl.[0].Source = "\x0d" && tl.[1].Source = "\x0a" then
            enqueueProcessed [TokenData.Create Token.NewLine "\n"]
            true
        else
            todo.Push tl.[1]
            todo.Push tl.[0]
            false

    let ``Try conversion to b-break``() =
        let t0 = getToken()
        if t0.Token = Token.NewLine then
            enqueueProcessed [TokenData.Create Token.NewLine "\n"]
            true
        else
            todo.Push t0
            false

    let rec aggregator() = 
        if processed.Count > 0 then processed.Dequeue()
        else
            [
                ``Try conversion DOS/Windows break``
                ``Try conversion to b-break``
            ]
            |>  List.fold(fun s (fn:unit->bool) -> if not(s) then fn() else true) false
            |>  function
                |   true    -> aggregator()
                |   false   ->
                    let t0 = getToken()
                    if t0.Token = Token.EOF then t0
                    else
                        processed.Enqueue(t0)
                        aggregator()

    (fun () -> aggregator())



[<NoEquality; NoComparison>]
type RollingStream<'a when 'a : equality> = private {
        TokenStream            : RSList<'a>
        mutable StreamPosition : int
        Current                : (unit -> 'a)
        StopValue              : 'a
    }
    with
        static member Create rdr sv = { TokenStream = RSList<'a>(65536); StreamPosition = 0; Current = rdr; StopValue = sv }

        member this.Get() =
            if this.EOF then 
                failwith "Cannot read beyond EOF"
            else
                if this.StreamPosition = this.TokenStream.Count then
                    let item = this.Current()
                    this.TokenStream.Add(item)
                let current = this.StreamPosition
                this.StreamPosition <- (this.StreamPosition + 1)
                this.TokenStream.[current]


        member this.Stream = 
            let rec read() = 
                seq {
                    if this.EOF then 
                        failwith "Cannot read beyond EOF"
                    else
                        let item = this.Get()
                        yield item
                        if item <> this.StopValue then yield! read()
                }
            read()


        member private this.BufferedLength() = this.TokenStream.Count

        member this.Position 
            with get() = this.StreamPosition
            and set v =
                if v < this.Position then
                    this.StreamPosition <- v
                else
                    while (this.Position < v) do
                        this.Get() |> ignore

        member this.EOF 
            with get() = 
                this.TokenStream.Count > 0 && this.TokenStream.[this.TokenStream.Count-1] = this.StopValue && this.Position = this.TokenStream.Count

        member this.PeekPrevious() = 
            if this.TokenStream.Count > 0 && this.StreamPosition > 0 then this.TokenStream.[this.StreamPosition-1] |> Some
            else None

        member this.Peek() = 
            let cp = this.Position
            let rs = this.Get()
            this.Position <- cp
            rs



