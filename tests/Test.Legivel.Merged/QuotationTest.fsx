open System.Reflection.Emit
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

let rec generate (il:ILGenerator) = function
    | Value(v,t) when t = typeof<int> ->
        printf "Value"
        il.Emit(OpCodes.Ldc_I4, v :?> int)
    | Call(None,mi,args) -> 
        printf "Call"
        generateAll il args
        il.EmitCall(OpCodes.Call, mi, null)
    | arg -> raise <| System.NotSupportedException(arg.ToString())
and generateAll il args = for arg in args do generate il arg

type Marker = interface end

let compile quotation =
    let f = DynamicMethod("f", typeof<int>, [||], typeof<Marker>.Module)
    let il = f.GetILGenerator()
    quotation |> generate il
    il.Emit(OpCodes.Ret)
    fun () -> f.Invoke(null,[||]) :?> int

let f = compile <@ 1 + 3 @>
let x = f ()


let vl = compile <@ 
    [0 .. 5] 
    |> List.map(fun i -> 
        printf "%d" i
        i + 10) @>

