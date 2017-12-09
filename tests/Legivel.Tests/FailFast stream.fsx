open System 
open System.IO
open System.Collections

open System.Collections.Generic
open System.Linq.Expressions


//let example = "
//#%RAML 1.0
//title: New API
//mediaType: [ application/json, application/xml ]
//types:
//  Person:
//  Another:
///books:
//  get:
//  put:
//  post:
//  /{bookTitle}:
//    get:
//    put:
//    delete:
//    /author:
//      get:
//    /publisher:
//      get:
//"

type Token =
    |   Symbol of string
    |   Space of int
    |   NewLine of int
    |   Text of string
    |   Other of char
    |   EOF


new string(['a'; 'b'; 'c'] |> Array.ofList)

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
    let isNewLine = (fun c -> c = '\n')
    let isText = (fun c -> 
                            (c >= 'a' && c <= 'z') ||
                            (c >= 'A' && c <= 'Z') ||
                            (c >= '0' && c <= '9')
                            )
    let isSymbol = (fun c -> "{}[]()&".Contains(c))


    let rec reader() = seq {
        let chri = strm.Read()
        if chri < 0 then yield EOF
        else
            let chr = char(chri)
            if isSpace chr then
                let cnt = charCount isSpace 1
                yield Space cnt
                yield! reader()
            elif isNewLine chr then
                let cnt = charCount isNewLine 1
                yield NewLine cnt
                yield! reader()
            elif isText chr then
                let txt = charRead isText [chr]
                yield Text txt
                yield! reader()
            else
                yield Other chr
                yield! reader()
        }
    let s = reader()
    (fun () -> s |> Seq.take 1 |> Seq.head)


//let read =
//    let q = new Queue<int>([|0 .. 100|])
//    (fun () -> if q.Count > 0 then q.Dequeue() else -1)



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

        member this.Position 
            with get() = List.length this.Past

        member private this.BufferedLength() = ((List.length this.Past) + (List.length this.Future))

        member this.SetPosition v =
            if v < this.Position then
                let df = this.Position - v
                let nFut = this.Future |> List.append (this.Past |> List.take df |> List.rev)
                let nPast = this.Past |> List.skip df
                { this with Past = nPast; Future = nFut }
            elif v > this.Position then
                let df = (this.BufferedLength() - v)
                if df <= 0 then
                    let nPast = this.Past |> List.append (this.Future |> List.rev)
                    let nFut  = []
                    let no = { this with Past = nPast; Future = nFut }
                    if df < 0 then
                        no.Stream |> Seq.take (-df) |> Seq.iter(ignore)
                    no
                else
                    let nPast = this.Past |> List.append (this.Future |> List.take df |> List.rev)
                    let nFut = this.Future |> List.skip df
                    { this with Past = nPast; Future = nFut }
            else
                this


let yaml = "
- foo:   bar
- - baz
  - baz
"

let tkn = tokenizer yaml

let stream = RollingStream<_>.Create tkn


stream.Stream
|>  Seq.takeWhile (fun c -> c <> EOF)
|>  Seq.iter(fun i -> printfn "%A" i)


let stream = stream.SetPosition 5







stream.Stream
|>  Seq.take 20
|>  Seq.iter(fun i -> printfn "%d" i)


let stream = stream.SetPosition 5

stream.Stream
|>  Seq.take 5
|>  Seq.iter(fun i -> printfn "%d" i)

let stream = stream.SetPosition 5

stream.Stream
|>  Seq.take 5
|>  Seq.iter(fun i -> printfn "%d" i)
