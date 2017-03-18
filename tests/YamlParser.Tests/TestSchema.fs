module TestSchema

open NUnit.Framework
open FsUnit
open TagResolution
open RepresentationGraph
open YamlParser.Internals


[<Test>]
let ``Test JSON Schema Tags``() =
    JSON.NullGlobalTag.canonFn "null" |> should equal "null"

    JSON.BooleanGlobalTag.canonFn "true" |> should equal "true"
    JSON.BooleanGlobalTag.canonFn "false" |> should equal "false"

    [("1234","+1234"); ("-1234", "-1234"); ("0","+0"); ("-0" ,"+0")]
    |> List.iter(fun (i,e) -> JSON.IntegerGlobalTag.canonFn i |> should equal e)

    //  http://www.yaml.org/spec/1.2/spec.html#id2804318
    [
        ("0.", "+0.0e+001");("0.0", "+0.0e+001");("1.0", "+0.1e+001");("20.0","+0.2e+002");
        ("-0.0", "+0.0e+001");
        ("3.141500","+0.31415e+001");("100.01500","+0.100015e+003");("-1.","-0.1e+001")
        ("2.3e+4", "+0.23e+005")
    ]
    |> List.iter(fun (i,e) -> JSON.FloatGlobalTag.canonFn i |> should equal e)


[<Test>]
let ``Test JSON TagResolution``() =
    let tagResolveScalar s =
        let nst = TagResolution.Common.NonSpecificTagQM Scalar
        let makeScalar s =
            let nh = lazy(NodeHash.Create s)
            ScalarNode(NodeData<string>.Create nst s nh)
        let scalar = makeScalar s
        TagResolutionInfo.Create (scalar.NodeTag.Uri) ([]) (scalar) (scalar.NodeTag.Kind)
        |> JSONSchema.TagResolution 

    tagResolveScalar "null" 
        |> Option.map(fun e -> (e.Uri |> should equal (JSON.NullGlobalTag.Uri)); e) |> Option.isSome |> should equal true

    ["false" ; "true"]
    |> List.iter(fun s ->
        tagResolveScalar s |> Option.map(fun e -> (e.Uri |> should equal (JSON.BooleanGlobalTag.Uri)); e) |> Option.isSome |> should equal true
    )

    ["0" ; "-0" ; "3" ;"-19" ]
    |> List.iter(fun s ->
        tagResolveScalar s |> Option.map(fun e -> (e.Uri |> should equal (JSON.IntegerGlobalTag.Uri)); e) |> Option.isSome |> should equal true
    )

    ["0." ; "-0.0" ;"12e+03";"-2E+05" ]
    |> List.iter(fun s ->
        tagResolveScalar s |> Option.map(fun e -> (e.Uri |> should equal (JSON.FloatGlobalTag.Uri)); e) |> Option.isSome |> should equal true
    )

    ["True"; "Null"; "0o7"; "0x3A"; "+12.3"]
    |> List.iter(fun s ->
        (fun () -> tagResolveScalar s |> ignore) |> should throw typeof<TagResolutionException>
    )



[<Test>]    //  http://www.yaml.org/spec/1.2/spec.html#id2805712
let ``Test Yaml Core Schema Tags``() =
    ["null"; "NULL"; "Null"]
    |> List.iter(fun i -> YamlCore.NullGlobalTag.canonFn i |> should equal "null")


    ["true";"True";"TRUE"]
    |> List.iter(fun i -> YamlCore.BooleanGlobalTag.canonFn i |> should equal "true")

    ["false";"False";"FALSE"]
    |> List.iter(fun i -> YamlCore.BooleanGlobalTag.canonFn i |> should equal "false")


    [("0","+0"); ("-0","+0"); ("0o7", "+7"); ("0x3A","+58"); ("-19" ,"-19")]
    |> List.iter(fun (i,e) -> YamlCore.IntegerGlobalTag.canonFn i |> should equal e)

    //  http://www.yaml.org/spec/1.2/spec.html#id2804318
    [
        ("0.", "+0.0e+001");("-0.0", "+0.0e+001");(".5", "+0.5e+000");("+12e+03","+0.12e+005");
        ("-2E+05", "-0.2e+006")
        (".inf","+.inf");("-.Inf","-.inf");("+.INF","+.inf")
        (".NAN", ".nan")
    ]
    |> List.iter(fun (i,e) -> YamlCore.FloatGlobalTag.canonFn i |> should equal e)


[<Test>]
let ``Test YamlCore TagResolution``() =
    let tagResolveScalar s =
        let nst = TagResolution.Common.NonSpecificTagQM Scalar
        let makeScalar s =
            let nh = lazy(NodeHash.Create s)
            ScalarNode(NodeData<string>.Create nst s nh)
        let scalar = makeScalar s
        TagResolutionInfo.Create (scalar.NodeTag.Uri) ([]) (scalar) (scalar.NodeTag.Kind)
        |> YamlCoreSchema.TagResolution 

    ["null"; "NULL"; "Null"]
    |> List.iter(fun s ->
        tagResolveScalar s |> Option.map(fun e -> (e.Uri |> should equal (YamlCore.NullGlobalTag.Uri)); e) |> Option.isSome |> should equal true
    )

    ["true";"True";"TRUE";"false";"False";"FALSE"]
    |> List.iter(fun s ->
        tagResolveScalar s |> Option.map(fun e -> (e.Uri |> should equal (YamlCore.BooleanGlobalTag.Uri)); e) |> Option.isSome |> should equal true
    )

    ["0" ; "-0" ; "3" ;"-19"; "0o7"; "0x3A" ]
    |> List.iter(fun s ->
        tagResolveScalar s |> Option.map(fun e -> (e.Uri |> should equal (YamlCore.IntegerGlobalTag.Uri)); e) |> Option.isSome |> should equal true
    )

    [
        "0."; "-0.0"; ".5"; "+12e+03";
        "-2E+05"
        ".inf"; "-.Inf"; "+.INF"
        ".NAN"
    ]
    |> List.iter(fun s ->
        System.Console.WriteLine s
        tagResolveScalar s |> Option.map(fun e -> (e.Uri |> should equal (YamlCore.FloatGlobalTag.Uri)); e) |> Option.isSome |> should equal true
    )

    ["not matched"; "2017-03-18"; "plain string"]
    |> List.iter(fun s ->
        tagResolveScalar s |> Option.map(fun e -> (e.Uri |> should equal (Failsafe.StringGlobalTag.Uri)); e) |> Option.isSome |> should equal true
    )


