
open System.Collections.Generic


let dict = Dictionary<int, string>()



let dg = 
    dict.GetType().GetInterfaces()
    |>  List.ofArray
    |>  List.filter(fun s -> s.FullName.StartsWith("System.Collections.Generic.IDictionary`2") && s.GetGenericArguments().Length = 2)
    |>  List.head

dg.GetGenericArguments().Length

let ctor = dict.GetType().GetConstructor([||])

let nwo = ctor.Invoke([||]) :?> IDictionary<int,string>

nwo.Add(1, "tst")

nwo.[1]

dg.Name



