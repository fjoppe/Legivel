module YamlParser.Common

open System
open YamlParser.Utilities.SpookyHash

module internal InternalUtil =
    let equalsOn f (yobj:obj) =
        match yobj with
        | :? 'T as y -> (f y)
        | _ -> false

    let compareOn f (yobj: obj) =
        match yobj with
        | :? 'T as y -> (f y)
        | _ -> invalidArg "yobj" "cannot compare values of different types"


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

        override this.Equals(yobj) = yobj |> InternalUtil.equalsOn (fun that -> this.hash1 = that.hash1 && this.hash2 = that.hash2)
        
        override this.GetHashCode() = this.hash1.GetHashCode() ^^^ this.hash2.GetHashCode()

        override this.ToString() = sprintf "#%X%X" (this.hash1) (this.hash2)
        member m.AsString = m.ToString()

        interface System.IComparable with
            member this.CompareTo yobj = yobj |> InternalUtil.compareOn (fun that -> NodeHash.compare this that)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NodeHash = 
    let Create s = NodeHash.Create s
    let Merge nhl = NodeHash.Merge nhl


[<StructuredFormatDisplay("{AsString}")>]
type    DocumentLocation = {
        Line    : int
        Column  : int
    }
    with
        static member Create l cl = { Line = l; Column = cl}
        member this.AddAndSet l cl = { Line = this.Line+l; Column = cl}

        member this.ToPrettyString() = sprintf "line %d; column %d" this.Line this.Column

        override this.ToString() = sprintf "(l%d, c%d)" this.Line this.Column
        member m.AsString = m.ToString()

