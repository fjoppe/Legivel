module APIsGuruTests
open FSharp.Data
open System

// Test that Legivel can parse real-word Swagger 2.0 schemas
// https://github.com/APIs-guru/api-models/blob/master/API.md

let private apisGuruList = lazy (
    printfn "Loading APIs.Guru list ..."
    //let list = Http.RequestString("https://api.apis.guru/v2/list.json")
    System.Net.ServicePointManager.SecurityProtocol <- System.Net.SecurityProtocolType.Tls12
    System.Net.ServicePointManager.ServerCertificateValidationCallback <-
        fun _ _ _ _ -> true
    use client = new System.Net.WebClient()
    let list = client.DownloadString("https://api.apis.guru/v2/list.json")
    JsonValue.Parse(list)
             .Properties()
  )

let private getApisGuruSchemas propertyName =
    apisGuruList.Value
    |> Array.choose (fun (name, obj)->
        obj.TryGetProperty("versions")
        |> Option.bind (fun v->
            v.Properties()
            |> Array.choose (fun (_,x)-> x.TryGetProperty(propertyName))
            |> Some)
       )
    |> Array.concat
    |> Array.map (fun x->
        FSharp.Data.JsonExtensions.AsString(x))

let apisGuruYamlSchemaUrls =
    getApisGuruSchemas "swaggerYamlUrl"

open NUnit.Framework
open FsUnitTyped
open TestUtils
open Legivel.RepresentationGraph

[<Ignore "Activate when peformance has improved">]
[<TestCaseSource("apisGuruYamlSchemaUrls")>]
let ``Parse schema from APIs.guru``(url:string) =
    let schema =
        try
            System.Net.ServicePointManager.SecurityProtocol <- System.Net.SecurityProtocolType.Tls12
            System.Net.ServicePointManager.ServerCertificateValidationCallback <-
                fun _ _ _ _ -> true
            use client = new System.Net.WebClient()
            let yamlstring = client.DownloadString(url)
            yamlstring |> Some
        with
        | :? System.Net.WebException ->
            printfn "Schema is unaccessible %s" url
            None
    match schema with
    | Some(s) ->
        let yml = YamlParseWithWarning s    // complete or partial representation
        match yml.Document with
        | MapNode n -> 
            n.Data.Length |> shouldBeGreaterThan 0
        | _    -> failwithf "Map node is expected %A" yml
    | None -> () // network issues

