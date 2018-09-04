/// Contains various utility functions
module Legivel.Customization.Utilities

open System
open Legivel.Common
open Legivel.RepresentationGraph
open Microsoft.FSharp.Reflection
open System.Reflection

let NoDocumentLocation = (DocumentLocation.Create 0 0)

type ParseMessageAtLineList = System.Collections.Generic.List<ParseMessageAtLine>

let AddError (l:ParseMessageAtLineList) e = l.Add e; FallibleOption.ErrorResult()

let GetErrors (l:FallibleOption<_> list) = l |> List.filter(fun pmf -> pmf.Result=FallibleOptionValue.ErrorResult)

let AreTypesEqual (t1:Type) (t2:Type) =
    let c1 = sprintf "%s%s" t1.Namespace t1.Name
    let c2 = sprintf "%s%s" t2.Namespace t2.Name
    c1 = c2


let getMapNode (errList:ParseMessageAtLineList) (n:Node) =
    match n with
    |   MapNode n ->  FallibleOption.Value n
    |   _    ->  AddError errList (ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a mapping node")


let getMapNodeQuiet (n:Node) =
    match n with
    |   MapNode n ->  FallibleOption.Value n
    |   _    -> FallibleOption.NoResult()


let getSeqNode (errList:ParseMessageAtLineList) (n:Node) =
    match n with
    |   SeqNode n ->  FallibleOption.Value n 
    |   _    -> AddError errList (ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a Sequence Node")


let getSeqNodeQuiet (n:Node) =
    match n with
    |   SeqNode n ->  FallibleOption.Value n 
    |   _    -> FallibleOption.NoResult()


let getScalarNode (errList:ParseMessageAtLineList) (n:Node) =
    match n with
    |   ScalarNode n ->  FallibleOption.Value n
    |   _    -> AddError errList (ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a Scalar Node")


let getScalarNodeQuiet (n:Node) =
    match n with
    |   ScalarNode n ->  FallibleOption.Value n
    |   _    -> FallibleOption.NoResult()


let GetCustomAttributeTp<'T when 'T :> Attribute> (errList:ParseMessageAtLineList) (st:Type) =
    let at = Attribute.GetCustomAttributes(st, typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> FallibleOption.NoResult()
    |   1   -> FallibleOption.Value (at.Head :?> 'T)
    |   _   -> AddError errList (ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.ToString()) (st.Name) (typeof<'T>.FullName)))


let GetCustomAttributeMmbr<'T when 'T :> Attribute> (errList:ParseMessageAtLineList) (st:MemberInfo) =
    let at = Attribute.GetCustomAttributes(st, typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> FallibleOption.NoResult()
    |   1   -> FallibleOption.Value (at.Head :?> 'T)
    |   _   -> AddError errList (ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.MemberType.ToString()) (st.Name) (typeof<'T>.FullName)))

let GetCustomAttributeFld<'T when 'T :> Attribute> (errList:ParseMessageAtLineList) (st:FieldInfo) =
    let at = [for i in st.GetCustomAttributes(typeof<'T>) do yield i]
    match at.Length with
    |   0   -> FallibleOption.NoResult()
    |   1   -> FallibleOption.Value (at.Head :?> 'T)
    |   _   -> AddError errList (ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.MemberType.ToString()) (st.Name) (typeof<'T>.FullName)))

let GetCustomAttributeDU<'T when 'T :> Attribute> (errList:ParseMessageAtLineList) (st:UnionCaseInfo) =
    let at = st.GetCustomAttributes(typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> FallibleOption.NoResult()
    |   1   -> FallibleOption.Value (at.Head :?> 'T)
    |   _   -> AddError errList (ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.DeclaringType.ToString()) (st.Name) (typeof<'T>.FullName)))

