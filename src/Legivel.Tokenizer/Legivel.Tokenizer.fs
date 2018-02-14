module Legivel.Tokenizer

open System.IO
open System.Collections.Generic


type Token =
    //|   Symbol  = 0
    |   ``t-space``             = 1
    |   ``t-tab``               = 24
    |   NewLine                 = 2
    |   ``c-printable``         = 3
    |   Other                   = 4
    |   EOF                     = 5
    |   NoToken                 = 32
    |   ``t-hyphen``    = 6
    |   ``t-plus``      = 33
    |   ``t-questionmark``       = 7
    |   ``t-colon``     = 8
    |   ``t-comma``     = 9
    |   ``t-dot``       = 34
    |   ``t-square-bracket-start``    = 10
    |   ``t-square-bracket-end``      = 11
    |   ``t-curly-bracket-start``     = 12
    |   ``t-curly-bracket-end``       = 13
    |   ``t-hash``           = 14
    |   ``t-ampersand``            = 15
    |   ``t-asterisk``             = 16
    |   ``t-quotationmark``               = 17
    |   ``t-pipe``           = 18
    |   ``t-gt``            = 19
    |   ``t-single-quote``      = 20
    |   ``t-double-quote``      = 21
    |   ``t-percent``         = 22
    |   ``t-commat``          = 23
    |   ``t-tilde``             = 32
    |   ``t-forward-slash``     = 35
    |   ``t-equals``            = 36
    |   ``ns-yaml-directive``   = 24
    |   ``ns-tag-directive``    = 25
    |   ``ns-reserved-directive`` = 26
    //|   ``c-directives-end``    = 27
    //|   ``c-document-end``      = 28
    |   ``nb-json``             = 29
    |   ``ns-dec-digit``        = 30
    |   ``c-escape``            = 31

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

    let rec charRead cnd acc =
        let chri = strm.Peek()
        if chri < 0 then new string(acc |> List.rev |> Array.ofList)
        else
            let chr = char(chri)
            if cnd(chr) then
                strm.Read() |> ignore
                charRead cnd (chr :: acc)
            else
                new string(acc |> List.rev |> Array.ofList)
    
    let isSpace = (fun c -> c = ' ')
    let isTab = (fun c -> c = '\t')
    let isDigit = (fun c -> c >= '0' && c <= '9')
    let isEscape = (fun c -> c = '\\')
    let isWhite = (fun c -> c = ' ' || c = '\t')
    let isNewLine = (fun c -> c = '\x0a' || c = '\x0d')
    let isSymbol = (fun c -> "-+?:,.[]{}#&*!|>\'\"%@`/=".Contains(c.ToString()))
    
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
        |   '`' -> Token.``t-tilde``
        |   '/' -> Token.``t-forward-slash``
        |   '=' -> Token.``t-equals``
        |   _ -> failwith "Unrecognized symbol"

    let reader() = 
        let chri = strm.Read()
        if chri < 0 then 
            (TokenData.Create Token.EOF "")
        else
            let chr = char(chri)
            [
                (isSpace, fun() -> TokenData.Create Token.``t-space`` (charRead isSpace [chr]))
                (isTab, fun() -> TokenData.Create Token.``t-tab`` (charRead isTab [chr]))
                (isNewLine, fun() -> TokenData.Create Token.NewLine (string chr))
                (isEscape, fun() -> TokenData.Create Token.``c-escape`` (string chr))
                (isSymbol, fun() -> TokenData.Create (``c-indicator`` chr) (string chr))
                (isDigit, fun() -> TokenData.Create Token.``ns-dec-digit`` (charRead isDigit [chr]))
                (isText, fun()-> TokenData.Create Token.``c-printable`` (charRead isText [chr]))
                (isJson, fun()-> TokenData.Create Token.``nb-json`` (charRead isJson [chr]))
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
    let tokenTake n = [1 .. n] |> List.map(fun _ -> getToken())
    let enqueueTodo lst = lst |> List.rev |> List.iter todo.Push
    let enqueueProcessed lst = lst |> List.iter processed.Enqueue

    let ``Try conversion DOS/Windows break``() =
        let tl = tokenTake 2
        if tl |> List.map TokenData.token = [Token.NewLine; Token.NewLine] then
            let [t0;t1] = tl
            let msBrk = sprintf "%s%s" t0.Source t1.Source
            if msBrk = "\x0d\x0a" then
                enqueueProcessed [TokenData.Create Token.NewLine "\n"]
                true
            else
                enqueueTodo tl
                false
        else
            enqueueTodo tl
            false

    let ``Try conversion to b-break``() =
        let t0 = getToken()
        if t0.Token = Token.NewLine then
            enqueueProcessed [TokenData.Create Token.NewLine "\n"]
            true
        else
            enqueueTodo [t0]
            false

    //let ``Try to combine c-sequence-entry and text``() =
    //    let tl = tokenTake 2
    //    let tokens = tl |> List.map TokenData.token
    //    match tokens with
    //    |   [Token.``t-hyphen``; Token.``t-space``] ->
    //        enqueueTodo tl
    //        false
    //    |   [Token.``t-hyphen``; _] -> 
    //        let [c;s] = tl |> List.map TokenData.source
    //        enqueueProcessed [TokenData.Create Token.``c-printable`` (sprintf "%s%s" c s)]
    //        true
    //    |   _ -> 
    //        enqueueTodo tl
    //        false

    //let ``Try conversion to c-directives-end``() =
    //    let tl = tokenTake 3
    //    if tl |> List.map TokenData.token = [Token.``t-hyphen``; Token.``t-hyphen``; Token.``t-hyphen``] then
    //        enqueueProcessed [TokenData.Create Token.``c-directives-end`` "---"]
    //        true
    //    else
    //        enqueueTodo tl
    //        false

    //let ``Try conversion to c-document-end``() =
    //    let tl = getToken()
    //    if tl = TokenData.Create Token.``c-printable`` "..." then
    //        enqueueProcessed [TokenData.Create Token.``c-document-end`` "..."]
    //        true
    //    else
    //        enqueueTodo [tl]
    //        false

    let ``Try conversion to l-directive``() =
        let tl = tokenTake 2
        if tl |> List.map TokenData.token = [Token.``t-percent`` ;Token.``c-printable``] then
            let [t0;t1] = tl 
            let s = t1 |> TokenData.source
            match s with
            |   "YAML"  ->  enqueueProcessed [t0; TokenData.Create Token.``ns-yaml-directive`` s]
            |   "TAG"   ->  enqueueProcessed [t0; TokenData.Create Token.``ns-tag-directive`` s]
            |   _       ->  enqueueProcessed [t0; TokenData.Create Token.``ns-reserved-directive`` s]
            true
        else
            enqueueTodo tl
            false

    let rec aggregator() = 
        if processed.Count > 0 then processed.Dequeue()
        else
            [
                ``Try conversion DOS/Windows break``
                ``Try conversion to b-break``
                ``Try conversion to l-directive``
                //``Try conversion to c-directives-end``
                //``Try conversion to c-document-end``
                //``Try to combine c-sequence-entry and text``
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
        mutable Past    :   'a list
        mutable Future  :   'a list
        Current         :   (unit -> 'a)
        StopValue       :   'a
    }
    with
        static member Create rdr sv = { Past = []; Future = []; Current = rdr; StopValue = sv}

        member this.Stream = 
            let rec read() = 
                seq {
                    if List.isEmpty this.Future then 
                        let item = this.Current()
                        this.Past <- item :: this.Past
                        yield item
                        if item <> this.StopValue then yield! read()
                    else
                        let item = List.head this.Future
                        this.Future <- (List.tail this.Future)
                        this.Past <- item :: this.Past
                        yield item
                        if item <> this.StopValue then yield! read()
                }
            read()


        member private this.BufferedLength() = ((List.length this.Past) + (List.length this.Future))

        member this.Position 
            with get() = List.length this.Past
            and set v =
                if v < this.Position then
                    let df = this.Position - v
                    let nFut = this.Future |> List.append (this.Past |> List.take df |> List.rev)
                    let nPast = this.Past |> List.skip df
                    this.Past <- nPast
                    this.Future <- nFut
                elif v > this.Position then
                    let df = (this.BufferedLength() - v)
                    if df <= 0 then
                        let nPast = this.Past |> List.append (this.Future |> List.rev)
                        let nFut  = []
                        this.Past <- nPast
                        this.Future <- nFut
                        if df < 0 then
                            this.Stream |> Seq.take (-df) |> Seq.iter(ignore)
                    else
                        let nPast = this.Past |> List.append (this.Future |> List.take df |> List.rev)
                        let nFut = this.Future |> List.skip df
                        this.Past <- nPast
                        this.Future <- nFut

        member this.EOF with get() = this.Past.Length > 0 && this.Past.Head = this.StopValue

        member this.Peek(n) = 
            let cp = this.Position
            let rs = this.Stream |> Seq.take n |> Seq.toList
            this.Position <- cp
            rs

        member this.Peek() = this.Peek(1) |> List.head

        member this.Take(n) = this.Stream |> Seq.take n |> Seq.toList

        member this.Take()  = this.Stream |> Seq.head


