module APIsGuruTests
open FSharp.Data
open System.IO

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

// [<Ignore "Activate when peformance has improved">]
[<TestCaseSource("apisGuruYamlSchemaUrls")>]
let ``Parse schema from APIs.guru``(url:string) =
    let schema =
        try
            System.Net.ServicePointManager.SecurityProtocol <- System.Net.SecurityProtocolType.Tls12
            System.Net.ServicePointManager.ServerCertificateValidationCallback <-
                fun _ _ _ _ -> true
            let tempFile = Path.GetTempFileName()
            use client = new System.Net.WebClient()
            //  A bug I encountered once...
            //  Working with TLS or without TLS. For a certain size in the content, the algorithm which processes 
            //  the content-gotten-with-TLS, breaks, while larger or smaller size content, it works. We've
            //  narrowed the problem down to a filesize of about 10k.
            //  This happens when you process a download directly in memory.
            //  The only way we got it working was, download to disk first, then open the file and process in 
            //  memory. That issue played at work, now I have the same issue here, 
            //  with this yaml: https://api.apis.guru/v2/specs/agco-ats.com/v1/swagger.yaml
            //  and crazy thing is, this issue is only when you run a release build, not with a debug build.
            //  also, this file is around 268k. But it displays the same behavior.
            //  so form now on, download first to file, then stuff it in memory to process it.
            client.DownloadFile(url, tempFile)
            let yamlstring = File.ReadAllText(tempFile)
            File.Delete(tempFile)
            yamlstring |> Some
        with
        | :? System.Net.WebException ->
            printfn "Schema is unaccessible %s" url
            None
    match schema with
    | Some(s) ->
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        let yml = YamlParseFailSafeSchemaWithWarning s    // complete or partial representation
        stopwatch.Stop()
        printf "Elapsed time: %O" (stopwatch.Elapsed)
        match yml.Document with
        | MapNode n -> 
            n.Data.Length |> shouldBeGreaterThan 0
        | _    -> failwithf "Map node is expected %A" yml
    | None -> () // network issues

