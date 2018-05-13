/// Contains various utility functions
module Legivel.Customization.Utilities

open System
open Legivel.Common
open Legivel.RepresentationGraph
open Microsoft.FSharp.Reflection
open System.Reflection

let NoDocumentLocation = (DocumentLocation.Create 0 0)

let GetErrors (l:FallibleOption<_,_> list) = l |> List.choose(fun pmf -> match pmf.Result with | FallibleOption.ErrorResult -> Some pmf.Error | _ -> None) |> List.collect(id)

let AreTypesEqual (t1:Type) (t2:Type) =
    let c1 = sprintf "%s%s" t1.Namespace t1.Name
    let c2 = sprintf "%s%s" t2.Namespace t2.Name
    c1 = c2


let getMapNode (n:Node) =
    match n with
    |   MapNode n ->  FallibleOption<_,_>.Value n
    |   _    -> FallibleOption<_,_>.ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a mapping node")]


let getMapNodeQuiet (n:Node) =
    match n with
    |   MapNode n ->  FallibleOption<_,_>.Value n
    |   _    -> FallibleOption<_,_>.NoResult()


let getSeqNode (n:Node) =
    match n with
    |   SeqNode n ->  FallibleOption<_,_>.Value n 
    |   _    -> FallibleOption<_,_>.ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a Sequence Node")]


let getSeqNodeQuiet (n:Node) =
    match n with
    |   SeqNode n ->  FallibleOption<_,_>.Value n 
    |   _    -> FallibleOption<_,_>.NoResult()


let getScalarNode (n:Node) =
    match n with
    |   ScalarNode n ->  FallibleOption<_,_>.Value n
    |   _    -> FallibleOption<_,_>.ErrorResult [(ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a Scalar Node")]


let getScalarNodeQuiet (n:Node) =
    match n with
    |   ScalarNode n ->  FallibleOption<_,_>.Value n
    |   _    -> FallibleOption<_,_>.NoResult()


let GetCustomAttributeTp<'T when 'T :> Attribute> (st:Type) =
    let at = Attribute.GetCustomAttributes(st, typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> FallibleOption<_,_>.NoResult()
    |   1   -> FallibleOption<_,_>.Value (at.Head :?> 'T)
    |   _   -> FallibleOption<_,_>.ErrorResult [(ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.ToString()) (st.Name) (typeof<'T>.FullName)))]


let GetCustomAttributeMmbr<'T when 'T :> Attribute> (st:MemberInfo) =
    let at = Attribute.GetCustomAttributes(st, typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> FallibleOption<_,_>.NoResult()
    |   1   -> FallibleOption<_,_>.Value (at.Head :?> 'T)
    |   _   -> FallibleOption<_,_>.ErrorResult [(ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.MemberType.ToString()) (st.Name) (typeof<'T>.FullName)))]

let GetCustomAttributeFld<'T when 'T :> Attribute> (st:FieldInfo) =
    let at = [for i in st.GetCustomAttributes(typeof<'T>) do yield i]
    match at.Length with
    |   0   -> FallibleOption<_,_>.NoResult()
    |   1   -> FallibleOption<_,_>.Value (at.Head :?> 'T)
    |   _   -> FallibleOption<_,_>.ErrorResult [(ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.MemberType.ToString()) (st.Name) (typeof<'T>.FullName)))]

let GetCustomAttributeDU<'T when 'T :> Attribute> (st:UnionCaseInfo) =
    let at = st.GetCustomAttributes(typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> FallibleOption<_,_>.NoResult()
    |   1   -> FallibleOption<_,_>.Value (at.Head :?> 'T)
    |   _   -> FallibleOption<_,_>.ErrorResult [(ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.DeclaringType.ToString()) (st.Name) (typeof<'T>.FullName)))]

