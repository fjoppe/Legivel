module TestTokenizer

open NUnit.Framework
open FsUnitTyped
open Legivel.Tokenizer
open System.Collections.Generic
open System.Linq.Expressions



let ReadStream() =
    let q = new Queue<int>([|0 .. 99|])
    (fun () -> if q.Count > 0 then q.Dequeue() else -1)


[<Test>]
let ``Test RollingStream - Simple forward read all``() =
    let stream = RollingStream<_>.Create (ReadStream())
    
    stream.Stream
    |>  Seq.take 100
    |>  Seq.length |> shouldEqual 100


[<Test>]
let ``Test RollingStream - Simple forward read subset``() =
    let stream = RollingStream<_>.Create (ReadStream())

    stream.Stream
    |>  Seq.take 50
    |>  Seq.length |> shouldEqual 50


[<Test>]
let ``Test RollingStream - Simple forward read beyond end``() =
    let stream = RollingStream<_>.Create (ReadStream())

    stream.Stream
    |>  Seq.take 110
    |>  Seq.filter (fun i -> i>=0)
    |>  Seq.length |> shouldEqual 100


[<Test>]
let ``Test RollingStream - Read - Set Position back - Read``() =
    let stream = RollingStream<_>.Create (ReadStream())

    stream.Stream
    |>  Seq.take 10
    |>  Seq.toList
    |>  shouldEqual [0 .. 9]

    stream.Position |> shouldEqual 10

    stream.Position <- 5

    stream.Stream
    |>  Seq.take 10
    |>  Seq.toList
    |>  shouldEqual [5 .. 14]


[<Test>]
let ``Test RollingStream - Set Position forward - Simple``() =
    let stream = RollingStream<_>.Create (ReadStream())

    stream.Position <- 5

    stream.Stream
    |>  Seq.take 10
    |>  Seq.toList
    |>  shouldEqual [5 .. 14]


[<Test>]
let ``Test RollingStream - Set Position forward - check nothing is lost``() =
    let stream = RollingStream<_>.Create (ReadStream())

    //  forward
    stream.Position <- 5

    stream.Stream
    |>  Seq.take 10
    |>  ignore 

    //  back to beginning
    stream.Position <- 0

    stream.Stream
    |>  Seq.take 10
    |>  Seq.toList
    |>  shouldEqual [0 .. 9]


[<Test>]
let ``Test Tokenizer - Flow Sequence - simple text``() =
    let yaml = "
- Mark McGwire
- Sammy Sosa
- Ken Griffey"

    let stream = RollingStream<_>.Create (tokenizer yaml)
    stream.Stream
    |>  Seq.takeWhile (fun (e,_) -> e <> Token.EOF)
    |>  Seq.toList
    |>  List.map fst
    |>  shouldEqual [
        Token.NewLine; Token.``c-sequence-entry`` ; Token.``s-space``; Token.Text; Token.``s-space``; Token.Text;  
        Token.NewLine; Token.``c-sequence-entry`` ; Token.``s-space``; Token.Text; Token.``s-space``; Token.Text; 
        Token.NewLine; Token.``c-sequence-entry`` ; Token.``s-space``; Token.Text; Token.``s-space``; Token.Text; 
    ]

[<Test>]
let ``Test Tokenizer - Block Sequence - simple text``() =
    let yaml = "[ Mark McGwire, Sammy Sosa, Ken Griffey ]"

    let stream = RollingStream<_>.Create (tokenizer yaml)
    stream.Stream
    |>  Seq.takeWhile (fun (e,_) -> e <> Token.EOF)
    |>  Seq.toList
    |>  List.map fst
    |>  shouldEqual [
        Token.``c-sequence-start``; 
        Token.``s-space``; Token.Text; Token.``s-space``; Token.Text; Token.``c-collect-entry``; 
        Token.``s-space``; Token.Text; Token.``s-space``; Token.Text; Token.``c-collect-entry``; 
        Token.``s-space``; Token.Text; Token.``s-space``; Token.Text; 
        Token.``s-space``;  Token.``c-sequence-end``; 
    ]    


[<Test>]
let ``Test Tokenizer - Flow Sequence - numbers``() =
    let yaml = "
- 5
- 10
- -9"

    let stream = RollingStream<_>.Create (tokenProcessor yaml)
    stream.Stream
    |>  Seq.takeWhile (fun (e,_) -> e <> Token.EOF)
    |>  Seq.toList
    |>  List.map fst
    |>  shouldEqual [
        Token.NewLine; Token.``c-sequence-entry`` ; Token.``s-space``; Token.Text; 
        Token.NewLine; Token.``c-sequence-entry`` ; Token.``s-space``; Token.Text; 
        Token.NewLine; Token.``c-sequence-entry`` ; Token.``s-space``; Token.Text;
    ]


