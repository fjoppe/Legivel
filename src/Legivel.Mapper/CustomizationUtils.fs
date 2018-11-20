/// Contains various utility functions
module Legivel.Customization.Utilities

open System
open Legivel.Common
open Legivel.RepresentationGraph
open Microsoft.FSharp.Reflection
open System.Reflection

let NoDocumentLocation = (DocumentLocation.Create 0 0)

type ProcessMessageList = System.Collections.Generic.List<ParseMessageAtLine>

type ProcessMessages = {
    Errors   : ProcessMessageList
    Warnings : ProcessMessageList
}
with
    static member Create() = { Errors = ProcessMessageList(); Warnings = ProcessMessageList()}

let AddError (l:ProcessMessages) e = l.Errors.Add e; FallibleOption.ErrorResult(),l
let AddWarning (l:ProcessMessages) w = l.Warnings.Add w; FallibleOption.NoResult(),l

let GetErrors (l:(FallibleOption<_>*ProcessMessages) list) = l |> List.filter(fun (pmf,pm) -> pmf.Result=FallibleOptionValue.ErrorResult)

let AreTypesEqual (t1:Type) (t2:Type) =
    let c1 = sprintf "%s%s" t1.Namespace t1.Name
    let c2 = sprintf "%s%s" t2.Namespace t2.Name
    c1 = c2


let getMapNode (msgList:ProcessMessages) (n:Node) =
    match n with
    |   MapNode n ->  FallibleOption.Value n, msgList
    |   _    ->  AddError msgList (ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a mapping node")


let getMapNodeQuiet (msgList:ProcessMessages) (n:Node) =
    match n with
    |   MapNode n ->  FallibleOption.Value n, msgList
    |   _    -> FallibleOption.NoResult(), msgList


let getSeqNode (msgList:ProcessMessages) (n:Node) =
    match n with
    |   SeqNode n ->  FallibleOption.Value n, msgList
    |   _    -> AddError msgList (ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a Sequence Node")


let getSeqNodeQuiet (msgList:ProcessMessages)  (n:Node) =
    match n with
    |   SeqNode n ->  FallibleOption.Value n, msgList
    |   _    -> FallibleOption.NoResult(), msgList


let getScalarNode (msgList:ProcessMessages) (n:Node) =
    match n with
    |   ScalarNode n ->  FallibleOption.Value n, msgList
    |   _    -> AddError msgList (ParseMessageAtLine.Create (n.ParseInfo.Start) "Expecting a Scalar Node")


let getScalarNodeQuiet (msgList:ProcessMessages) (n:Node) =
    match n with
    |   ScalarNode n ->  FallibleOption.Value n, msgList
    |   _    -> FallibleOption.NoResult(), msgList


let GetCustomAttributeTp<'T when 'T :> Attribute> (msgList:ProcessMessages) (st:Type) =
    let at = Attribute.GetCustomAttributes(st, typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> FallibleOption.NoResult(), msgList
    |   1   -> FallibleOption.Value (at.Head :?> 'T), msgList
    |   _   -> AddError msgList (ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.ToString()) (st.Name) (typeof<'T>.FullName)))


let GetCustomAttributeMmbr<'T when 'T :> Attribute> (msgList:ProcessMessages) (st:MemberInfo) =
    let at = Attribute.GetCustomAttributes(st, typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> FallibleOption.NoResult(), msgList
    |   1   -> FallibleOption.Value (at.Head :?> 'T), msgList
    |   _   -> AddError msgList (ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.MemberType.ToString()) (st.Name) (typeof<'T>.FullName)))

let GetCustomAttributeFld<'T when 'T :> Attribute> (msgList:ProcessMessages) (st:FieldInfo) =
    let at = [for i in st.GetCustomAttributes(typeof<'T>) do yield i]
    match at.Length with
    |   0   -> FallibleOption.NoResult(), msgList
    |   1   -> FallibleOption.Value (at.Head :?> 'T), msgList
    |   _   -> AddError msgList (ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.MemberType.ToString()) (st.Name) (typeof<'T>.FullName)))

let GetCustomAttributeDU<'T when 'T :> Attribute> (msgList:ProcessMessages) (st:UnionCaseInfo) =
    let at = st.GetCustomAttributes(typeof<'T>) |> List.ofArray
    match at.Length with
    |   0   -> FallibleOption.NoResult(), msgList
    |   1   -> FallibleOption.Value (at.Head :?> 'T), msgList
    |   _   -> AddError msgList (ParseMessageAtLine.Create NoDocumentLocation (sprintf "'%s.%s' has too many attributes of type '%s'" (st.DeclaringType.ToString()) (st.Name) (typeof<'T>.FullName)))

