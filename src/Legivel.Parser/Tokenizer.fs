module Legivel.Tokenizer

open System.IO
open System.Collections.Generic


type Token =
    //|   Symbol  = 0
    |   Space   = 1
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

type TokenData =
    |   StringData of string
    |   IntData of int
    |   CharData of char
    |   NoData


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
    let isNewLine = (fun c -> c = '\n' || c = '\r')
    let isText = (fun c -> 
                            (c >= 'a' && c <= 'z') ||
                            (c >= 'A' && c <= 'Z') ||
                            (c >= '0' && c <= '9')
                            )
    //let isSymbol = (fun c -> "{}[]()&?:-,".Contains(c.ToString()))

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
        |   _ -> Token.Other

    let reader() = 
        let chri = strm.Read()
        if chri < 0 then 
            (Token.EOF, NoData)
        else
            let chr = char(chri)
            if isSpace chr then
                let cnt = charCount isSpace 1
                (Token.Space, IntData cnt)
            elif isNewLine chr then
                let cnt = charCount isNewLine 1
                (Token.NewLine, IntData cnt)
            elif isText chr then
                let txt = charRead isText [chr]
                (Token.Text, StringData txt)
            else
                let sym = ``c-indicator`` chr
                if sym <> Token.Other then
                    (sym, CharData chr)
                else
                    (Token.Other, CharData chr)
    (fun () -> reader())


let tokenAggregator str =
    let tkn = tokenizer str
    let yq = Queue<Token*TokenData>()

    let ``Aggregate c-sequence-entry`` t0 =
        if fst(t0) = Token.``c-sequence-entry`` then
            let t1 = tkn()
            let tl = [t0; t1] |> List.map fst
            if tl = [Token.``c-sequence-entry``; Token.Text] then
                let (CharData c) = snd(t0)
                let (StringData s) = snd(t1)
                yq.Enqueue (Token.Text, StringData (sprintf "%c%s" c s))
                true
            else
                yq.Enqueue(t0)
                yq.Enqueue(t1)
                true
        else
            false

    let rec aggregator() = 
        if yq.Count > 0 then yq.Dequeue()
        else
            let t0 = tkn()
            if fst(t0) = Token.EOF then t0
            elif ``Aggregate c-sequence-entry`` t0 then
                aggregator()
            else t0
    (fun () -> aggregator())



[<NoEquality; NoComparison>]
type RollingStream<'a> = private {
        mutable Past    :   'a list
        mutable Future  :   'a list
        Current :   (unit -> 'a)
    }
    with
        static member Create rdr = { Past = []; Future = []; Current = rdr}

        member this.Stream = 
            let rec read() = 
                seq {
                    if List.isEmpty this.Future then 
                        let item = this.Current()
                        this.Past <- item :: this.Past
                        yield item
                        yield! read()
                    else
                        let item = List.head this.Future
                        this.Future <- (List.tail this.Future)
                        this.Past <- item :: this.Past
                        yield item
                        yield! read()
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

