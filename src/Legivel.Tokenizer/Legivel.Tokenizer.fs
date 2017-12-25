module Legivel.Tokenizer

open System.IO
open System.Collections.Generic


type Token =
    //|   Symbol  = 0
    |   ``s-space`` = 1
    |   ``s-tab``   = 24
    |   NewLine = 2
    |   Text    = 3
    |   Other   = 4
    |   EOF     = 5
    |   ``c-sequence-entry``    = 6
    |   ``c-mapping-key``       = 7
    |   ``c-mapping-value``     = 8
    |   ``c-collect-entry``     = 9
    |   ``c-sequence-start``    = 10
    |   ``c-sequence-end``      = 11
    |   ``c-mapping-start``     = 12
    |   ``c-mapping-end``       = 13
    |   ``c-comment``           = 14
    |   ``c-anchor``            = 15
    |   ``c-alias``             = 16
    |   ``c-tag``               = 17
    |   ``c-literal``           = 18
    |   ``c-folded``            = 19
    |   ``c-single-quote``      = 20
    |   ``c-double-quote``      = 21
    |   ``c-directive``         = 22
    |   ``c-reserved``          = 23
    |   ``ns-yaml-directive``   = 24
    |   ``ns-tag-directive``    = 25
    |   ``ns-reserved-directive`` = 26
    |   ``c-directives-end``    = 27
    |   ``c-document-end``      = 28


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
    let isWhite = (fun c -> c = ' ' || c = '\t')
    let isNewLine = (fun c -> c = '\x0a' || c = '\x0d')
    let isSymbol = (fun c -> "-?:,[]{}#&*!|>\'\"%@`".Contains(c.ToString()))
    
    //  c-printable
    let isText = (fun c ->  not(isWhite c) && not(isNewLine c) && not(isSymbol c) &&
                            (("\x09\x0a\x0d\x85".Contains(string c)) ||
                            (c >= '\x20' && c <= '\x7e') ||
                            (c >= '\xa0' && c <= '\ud7ff') ||
                            (c >= '\ue000' && c <= '\ufffd'))
                            )

    let ``c-indicator`` c =
        match c with
        |   '-' -> Token.``c-sequence-entry``
        |   '?' -> Token.``c-mapping-key``
        |   ':' -> Token.``c-mapping-value``
        |   ',' -> Token.``c-collect-entry``
        |   '[' -> Token.``c-sequence-start``
        |   ']' -> Token.``c-sequence-end``
        |   '{' -> Token.``c-mapping-start``
        |   '}' -> Token.``c-mapping-end``
        |   '#' -> Token.``c-comment``
        |   '&' -> Token.``c-anchor``
        |   '*' -> Token.``c-alias``
        |   '!' -> Token.``c-tag``
        |   '|' -> Token.``c-literal``
        |   '>' -> Token.``c-folded``
        |   '\'' -> Token.``c-single-quote``
        |   '\"' -> Token.``c-double-quote``
        |   '%' -> Token.``c-directive``
        |   '@' -> Token.``c-reserved``
        |   '`' -> Token.``c-reserved``
        |   _ -> failwith "Unrecognized symbol"

    let reader() = 
        let chri = strm.Read()
        if chri < 0 then 
            (TokenData.Create Token.EOF "")
        else
            let chr = char(chri)
            if isSpace chr then
                let txt = charRead isSpace [chr]
                TokenData.Create Token.``s-space`` txt
            elif isNewLine chr then
                TokenData.Create Token.NewLine (string chr)
            elif isSymbol chr then
                let sym = ``c-indicator`` chr
                TokenData.Create sym (string chr)
            elif isText chr then
                let txt = charRead isText [chr]
                TokenData.Create Token.Text txt
            else
                TokenData.Create Token.Other (string chr)
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

    let ``Try to combine c-sequence-entry and text``() =
        let tl = tokenTake 2
        if tl |> List.map TokenData.token = [Token.``c-sequence-entry``; Token.Text] then
            let [c;s] = tl |> List.map TokenData.source
            enqueueProcessed [TokenData.Create Token.Text (sprintf "%s%s" c s)]
            true
        else
            enqueueTodo tl
            false

    let ``Try conversion to c-directives-end``() =
        let tl = tokenTake 3
        if tl |> List.map TokenData.token = [Token.``c-sequence-entry``; Token.``c-sequence-entry``; Token.``c-sequence-entry``] then
            enqueueProcessed [TokenData.Create Token.``c-directives-end`` "---"]
            true
        else
            enqueueTodo tl
            false

    let ``Try conversion to c-document-end``() =
        let tl = getToken()
        if tl = TokenData.Create Token.Text "..." then
            enqueueProcessed [TokenData.Create Token.``c-document-end`` "..."]
            true
        else
            enqueueTodo [tl]
            false

    let ``Try conversion to l-directive``() =
        let tl = tokenTake 2
        if tl |> List.map TokenData.token = [Token.``c-directive`` ;Token.Text] then
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
                ``Try to combine c-sequence-entry and text``
                ``Try conversion DOS/Windows break``
                ``Try conversion to b-break``
                ``Try conversion to l-directive``
                ``Try conversion to c-directives-end``
                ``Try conversion to c-document-end``
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