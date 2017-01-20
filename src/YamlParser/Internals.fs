namespace YamlParser.Internals

open System
open SpookyHash

[<CustomEquality; CustomComparison>]
type NodeHash = private {
        hash1 : uint64
        hash2 : uint64
    }
    with
        static member Create (s:string) =
            let msg = System.Text.Encoding.Unicode.GetBytes(s)
            let (a,b) = Hash128 msg 0UL 0UL
            { hash1 = a; hash2 = b}

        static member Merge (nhl: NodeHash list) =
            let msg = 
                nhl
                |> List.map(fun nh -> 
                    let b1 = BitConverter.GetBytes(nh.hash1)
                    let b2 = BitConverter.GetBytes(nh.hash2)
                    Array.append b1 b2)
                |> Array.ofList
                |> Array.collect(fun e -> e)
            let (a,b) = Hash128 msg 0UL 0UL
            { hash1 = a; hash2 = b}
            

        static member private compare (a:NodeHash) (b:NodeHash) =
            if a.hash1 = b.hash1 then a.hash2.CompareTo(b.hash2)
            else a.hash1.CompareTo(b.hash1)

        override this.Equals(yobj) =
            match yobj with
            | :? NodeHash as y -> (this = y)
            | _ -> false
        
        override this.GetHashCode() = this.hash1.GetHashCode()

        interface System.IComparable with
            member this.CompareTo yobj =
               match yobj with
                 | :? NodeHash as y -> NodeHash.compare this y
                 | _ -> invalidArg "yobj" "cannot compare values of different types"    

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NodeHash = 
    let Create s = NodeHash.Create s
    let Merge nhl = NodeHash.Merge nhl


module ParserMonads =
    open System.Diagnostics

    [<DebuggerStepThrough>]
    type EitherBuilder<'a,'b,'c>(context : 'b) =
        member this.Yield (item : 'c) : 'b * Option<'a> = (context, None)

        [<CustomOperation("setcontext")>]
        member this.SetContext ((ct, pv), nw) = (nw, pv)

        [<CustomOperation("either")>]
        member this.Either ((ct, pv), nw) =
            match pv with
            |   None    -> (ct, nw <| ct)
            |   Some v  -> (ct, Some v)

        [<CustomOperation("ifneiter")>]
        member this.IfNeither ((ct, pv), nw) = 
            match pv with
            |   None    -> nw
            |   Some v  -> Some v

