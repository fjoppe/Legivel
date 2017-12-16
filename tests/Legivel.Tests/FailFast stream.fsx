#I __SOURCE_DIRECTORY__ 
#I "../../packages"
#I "../.."

#r @"bin/Debug/Legivel.Parser.dll"
#r @"bin/Debug/Legivel.Mapper.dll"
#r @"NLog/lib/net45/NLog.dll"

open System 
open System.IO
open System.Collections

open System.Collections.Generic
open System.Linq.Expressions
open Legivel.Tokenizer

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


let yaml = "
- foo:   bar
- - baz
  - baz
"

let tkn = tokenizer yaml

seq [ 1; 5; 9]

