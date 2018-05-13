#I __SOURCE_DIRECTORY__
#I "../.."
#r @"packages\FSharp.Data\lib\net45\FSharp.Data.dll"


open FSharp.Data
open System

let private apisGuruList = lazy (
    printfn "Loading APIs.Guru list ..."
    let list = Http.RequestString("https://api.apis.guru/v2/list.json")
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

    |>  Array.take(2)   // for dev only

    |> Array.map (fun x->
        FSharp.Data.JsonExtensions.AsString(x))

let apisGuruYamlSchemaUrls =
    getApisGuruSchemas "swaggerYamlUrl"




