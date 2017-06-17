namespace YamlParser.Internals

open System
open SpookyHash

[<CustomEquality; CustomComparison>]
[<StructuredFormatDisplay("{AsString}")>]
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

        override this.ToString() = sprintf "#%X%X" (this.hash1) (this.hash2)
        member m.AsString = m.ToString()

        interface System.IComparable with
            member this.CompareTo yobj =
               match yobj with
                 | :? NodeHash as y -> NodeHash.compare this y
                 | _ -> invalidArg "yobj" "cannot compare values of different types"    

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NodeHash = 
    let Create s = NodeHash.Create s
    let Merge nhl = NodeHash.Merge nhl


type FallibleOption<'a,'b> =
    |   Value of 'a
    |   NoResult
    |   ErrorResult of 'b
    member this.Data 
        with get() =
            match this with
            |   Value v -> v
            |   _ -> failwith "This instance has no value"

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FallibleOption =
    let bind<'a,'b,'c> (f:'a -> FallibleOption<'b,'c>) o =
        match o with
        |   Value v -> f v
        |   NoResult -> NoResult
        |   ErrorResult e -> ErrorResult e
    let map<'a,'b,'c> (f:'a -> 'b) (o:FallibleOption<'a,'c>) =
        match o with
        |   Value v -> Value (f v)
        |   NoResult -> NoResult
        |   ErrorResult e -> ErrorResult e

    let ifnoresult<'a,'b,'c> f (o:FallibleOption<'a,'c>) =
        match o with
        |   NoResult    -> f
        |   Value v     -> Value v
        |   ErrorResult e -> ErrorResult e

    let Value v = Value v


module ParserMonads =
    open System.Diagnostics

    [<DebuggerStepThrough>]
    type EitherBuilder<'a,'b,'c,'d>(context : 'c, addErr:'c->'b->'c) =
        member this.Yield (_ : 'd) : 'c * FallibleOption<'a,'b> = (context, NoResult)

        [<CustomOperation("setcontext")>]
        member this.SetContext (((_:'c), pv), nw) = (nw, pv)

        [<CustomOperation("either")>]
        member this.Either (((ct:'c), pv), nw) =
            match pv with
            |   Value v  -> (ct, Value v)
            |   NoResult -> (ct, nw ct)
            |   ErrorResult e -> 
                let ctn = addErr ct e
                (ctn, nw ctn)

        [<CustomOperation("ifneither")>]
        member this.IfNeither (((ct:'c), pv), nw) = 
            match pv with
            |   NoResult      -> (ct,nw)
            |   Value v       -> (ct, Value v)
            |   ErrorResult e -> 
                let ctn = addErr ct e            
                (ctn,nw)
                

        
        
