module Test.Legivel.RegexDSL

//  http://burtleburtle.net/bob/c/TestSpookyV2.cpp
open System
open Legivel.Utilities.RegexDSL
open NUnit.Framework
open FsUnitTyped
open System.Collections.Generic
open Legivel.Tokenizer

let EndOfStream = TokenData.Create (Token.EOF) ""


let ReadStream (ip:TokenData list) =
    let q = new Queue<TokenData>(ip)
    (fun () -> if q.Count > 0 then q.Dequeue() else EndOfStream)


//[<Test>]
//let ``Test Simple Parse Stream``() =

//    let yaml = "
//- 5
//- 10
//- -9"

//    let stream = RollingStream<_>.Create (tokenProcessor yaml) EndOfStream

