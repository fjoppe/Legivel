module Legivel.Mapping.Tests

open NUnit.Framework
open FsUnitTyped
open Legivel.Attributes
open Legivel.Serialization
open System


type SimpleRecord = {
    Name   : string
    Age    : int
}


let DeserializeSuccess<'tp> yml = 
    let r = Deserialize<'tp> yml
    r
    |> List.head
    |>  function
        |   Success s -> s.Data
        |   Error e -> failwith "Unexpected error"
      
      
let DeserializeError<'tp> yml = 
    Deserialize<'tp> yml
    |> List.head
    |>  function
        |   Success _ -> failwith "Unexpected success"
        |   Error e -> e


[<Test>]
let ``Deserialize - check docs - time to string - Sunny Day`` () =
    let yml = "20:03:20"
    let res = DeserializeSuccess<string> yml 
    res |> shouldEqual "20:03:20"


[<Test>]
let ``Deserialize - int - Sunny Day`` () =
    let yml = "43"
    let res = DeserializeSuccess<int> yml 
    res |> shouldEqual 43


[<Test>]
let ``Deserialize - float - Sunny Day`` () =
    let yml = "43.5"
    let res = DeserializeSuccess<float> yml 
    res |> shouldEqual 43.5


[<Test>]
let ``Deserialize - bool - Sunny Day`` () =
    let yml = "true"
    let res = DeserializeSuccess<bool> yml 
    res |> shouldEqual true


[<Test>]
let ``Deserialize - DateTime 1 - Sunny Day`` () =
    let yml = "2014-09-12"
    let res = DeserializeSuccess<DateTime> yml 
    res |> shouldEqual (DateTime(2014, 09, 12))

[<Test>]
let ``Deserialize - DateTime 2 - Sunny Day`` () =
    let yml = "2019-12-29T11:43:30+01:00"

    let res = DeserializeSuccess<DateTime> yml

    res.ToUniversalTime() |> shouldEqual (DateTime.Parse("2019-12-29T11:43:30+01:00").ToUniversalTime())


[<Test>]
let ``Deserialize - DateTimeOffset - Sunny Day`` () =
    let yml = "2019-12-29T11:43:30+01:00"
    let res = DeserializeSuccess<DateTimeOffset> yml
    res |> shouldEqual (DateTimeOffset.Parse "2019-12-29T11:43:30+01:00")


[<Test>]
let ``Deserialize - Naked Record Fields - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Age: 43 }"
    let res = DeserializeSuccess<SimpleRecord> yml 
    res.Name    |> shouldEqual "Frank"
    res.Age     |> shouldEqual 43


[<Ignore("Acceptance of default values must be a configurable option")>]
[<Test>]
let ``Deserialize - Naked Record Fields - default values - Sunny Day`` () =
    let yml = "{ Name: 'Frank' }"
    let res = DeserializeSuccess<SimpleRecord> yml 
    res.Name    |> shouldEqual "Frank"
    res.Age     |> shouldEqual 0


type SimpleAnnotatedRecord = {
    [<YamlField(Name = "name")>] Name   : string
    [<YamlField(Name = "age")>] Age     : int
}

[<Test>]
let ``Deserialize - Annotated Record Fields - Sunny Day`` () =
    let yml = "{ name: 'Frank', age: 43 }"
    let res = DeserializeSuccess<SimpleAnnotatedRecord> yml 
    res.Name    |> shouldEqual "Frank"
    res.Age     |> shouldEqual 43


[<Ignore("Acceptance of default values must be a configurable option")>]
[<Test>]
let ``Deserialize - Annotated Record Fields - default values - Sunny Day`` () =
    let yml = "{ name: 'Frank' }"
    let res = DeserializeSuccess<SimpleAnnotatedRecord> yml 
    res.Name    |> shouldEqual "Frank"
    res.Age     |> shouldEqual 0


type OptionalField = {
    Name   : string
    Age    : int option
}


[<Test>]
let ``Deserialize - Optional Record Field - present - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Age: 43 }"
    let res = DeserializeSuccess<OptionalField> yml 
    res.Name    |> shouldEqual "Frank"
    res.Age     |> shouldEqual (Some 43)


[<Test>]
let ``Deserialize - Optional Record Fields - missing - Sunny Day`` () =
    let yml = "{ Name: 'Frank' }"
    let res = DeserializeSuccess<OptionalField> yml 
    res.Name    |> shouldEqual "Frank"
    res.Age     |> shouldEqual None


[<Test>]
let ``Deserialize - Optional Record Fields - null value - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Age: null }"
    let res = DeserializeSuccess<OptionalField> yml 
    res.Name    |> shouldEqual "Frank"
    res.Age     |> shouldEqual None


type NestedRecord = {
        Street      : string
        HouseNumber : int
    }


type ContainingNested = {
        Name    : string
        Address : NestedRecord
    }


[<Test>]
let ``Deserialize - Nested Record - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Address: { Street: 'Rosegarden', HouseNumber: 5 } }"
    let res = DeserializeSuccess<ContainingNested> yml 
    res.Name    |> shouldEqual "Frank"
    res.Address.Street |> shouldEqual "Rosegarden"
    res.Address.HouseNumber |> shouldEqual 5


type ContainingOptionalNested = {
        Name    : string
        Address : NestedRecord option
    }


[<Test>]
let ``Deserialize - Nested Optional Record - present - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Address: { Street: 'Rosegarden', HouseNumber: 5 } }"
    let res = DeserializeSuccess<ContainingOptionalNested> yml 
    res.Name    |> shouldEqual "Frank"
    res.Address |> Option.get |> fun a -> a.Street |> shouldEqual "Rosegarden"
    res.Address |> Option.get |> fun a -> a.HouseNumber |> shouldEqual 5


[<Test>]
let ``Deserialize - Nested Optional Record - missing - Sunny Day`` () =
    let yml = "{ Name: 'Frank' }"
    let res = DeserializeSuccess<ContainingOptionalNested> yml 
    res.Name    |> shouldEqual "Frank"
    res.Address |> shouldEqual None


[<Test>]
let ``Deserialize - List - Sunny Day`` () =
    let yml = "[1, 1, 3, 5, 8, 9]" // not a pattern
    let res = DeserializeSuccess<int list> yml 
    res |> shouldEqual [1; 1; 3; 5; 8; 9]


[<Test>]
let ``Deserialize - List with type mismatch scalar element - Rainy Day`` () =
    let yml = "[1, 1, a]" 
    let res = DeserializeError<int list> yml 
    res.Error.Length |> shouldBeGreaterThan 0
    res.Error.Head |> fun m -> m.Message.StartsWith("Incorrect format: 'a', for tag: tag:yaml.org,2002:int") |> shouldEqual true


[<Test>]
let ``Deserialize - List with type mismatch map element - Rainy Day`` () =
    let yml = "[1, 1, {1 : 2}]"
    let res = DeserializeError<int list> yml 
    res.Error.Length |> shouldBeGreaterThan 0
    res.Error.Head |> fun m -> m.Message.StartsWith("Type mismatch") |> shouldEqual true


[<Test>]
let ``Deserialize - List with type mismatch seq element - Rainy Day`` () =
    let yml = "[1, 1, [1, 2]]"
    let res = DeserializeError<int list> yml 
    res.Error.Length |> shouldBeGreaterThan 0
    res.Error.Head |> fun m -> m.Message.StartsWith("Type mismatch") |> shouldEqual true


[<Test>]
let ``Deserialize - List with general type mismatch, given scalar for seq - Rainy Day`` () =
    let yml = "wrongtype"
    let res = DeserializeError<int list> yml 
    res.Error.Length |> shouldBeGreaterThan 0
    res.Error.Head |> fun m -> m.Message.StartsWith("Incorrect format: 'wrongtype', for tag: tag:yaml.org,2002:int") |> shouldEqual true


[<Test>]
let ``Deserialize - List with general type mismatch, given map<string,int> for seq - Rainy Day`` () =
    let yml = "{ a : 1}"
    let res = DeserializeError<int list> yml 
    res.Error.Length |> shouldBeGreaterThan 0
    res.Error.Head |> fun m -> m.Message.StartsWith("Incorrect format: 'a', for tag: tag:yaml.org,2002:int") |> shouldEqual true


[<Test>]
let ``Deserialize - List with general type mismatch, given map<int,string> for seq - Rainy Day`` () =
    let yml = "{ 1 : a}"
    let res = DeserializeError<int list> yml 
    res.Error.Length |> shouldBeGreaterThan 0
    res.Error.Head |> fun m -> m.Message.StartsWith("Expecting a Sequence Node") |> shouldEqual true


type ListField = {
    Name   : string
    Scores : int list
}


[<Test>]
let ``Deserialize - Record with List - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Scores: [1, 1, 3, 5, 8, 9]}"
    let res = DeserializeSuccess<ListField> yml 
    res.Name    |> shouldEqual "Frank"
    res.Scores  |> shouldEqual [1; 1; 3; 5; 8; 9]


type ListOptonField = {
    Name   : string
    Scores : int option list
}


[<Test>]
let ``Deserialize - Record with Option List - Sunny Day`` () =
    let yml = "{ Name: 'Frank', Scores: [1, 1, ~, 5, null, 9]}"
    let res = DeserializeSuccess<ListOptonField> yml 
    res.Name    |> shouldEqual "Frank"
    res.Scores  |> shouldEqual [Some 1; Some 1; None; Some 5; None ; Some 9]


type UnionCaseNoData =
    |   Zero
    |   One
    |   Two
    |   Three


[<Test>]
let ``Deserialize - Discriminated Union Simple - Sunny Day`` () =
    let yml = "One"
    let res = DeserializeSuccess<UnionCaseNoData> yml 
    res |> shouldEqual UnionCaseNoData.One


[<Test>]
let ``Deserialize - Discriminated Union - Bad value - Rainy Day`` () =
    let yml = "Four"    //  does not exist
    let res = DeserializeError<UnionCaseNoData> yml 
    res.Error.Length |> shouldBeGreaterThan 0
    res.Error.Head |> fun m -> m.Message.StartsWith("Union case 'Four' not availble in type") |> shouldEqual true


type UCData1 = {
    Name : string
    Age  : int
}


[<YamlField("TypeOf")>]
type UnionCaseWithData =
    |   One of UCData1
    |   [<YamlValue("two")>] Two of UCData1


[<Test>]
let ``Deserialize - Discriminated Union With Data - Sunny Day`` () =
    let yml = "
        Name: 'Frank'
        Age:  43
        TypeOf : One
    "
    let res = DeserializeSuccess<UnionCaseWithData> yml
    match res with
    |   One d -> d.Name |> shouldEqual "Frank"
                 d.Age  |> shouldEqual 43
    | _ -> failwith "Incorrect value!"


[<Test>]
let ``Deserialize - Discriminated Union With Bad Data - Rainy Day`` () =
    let yml = "
        Bad:  Value
        TypeOf : One
    "
    let res = DeserializeError<UnionCaseWithData> yml
    res.Error.Length |> shouldBeGreaterThan 0
    res.Error.Head |> fun m -> m.Message.StartsWith("Missing value for field") |> shouldEqual true


[<Test>]
let ``Deserialize - Discriminated Union-Alias With Data - Sunny Day`` () =
    let yml = "
        Name: 'Frank'
        Age:  43
        TypeOf : two    # alias
    "
    let res = DeserializeSuccess<UnionCaseWithData> yml
    match res with
    |   Two d -> d.Name |> shouldEqual "Frank"
                 d.Age  |> shouldEqual 43
    | _ -> failwith "Incorrect value!"


type UnionCaseEnum =
    |   Zero=0      
    |   One=1
    |   [<YamlValue("two")>] Two=2
    |   Three=3


[<Test>]
let ``Deserialize - Discriminated Union Enum Simple - Sunny Day`` () =
    let yml = "One"
    let res = DeserializeSuccess<UnionCaseEnum> yml
    res |> shouldEqual UnionCaseEnum.One


[<Test>]
let ``Deserialize - Discriminated Union Enum-Alias - Sunny Day`` () =
    let yml = "two # alias"
    let res = DeserializeSuccess<UnionCaseEnum> yml
    res |> shouldEqual UnionCaseEnum.Two


[<Test>]
let ``Deserialize - Discriminated Union Enum - Bad value - Rainy Day`` () =
    let yml = "Four"    //  does not exist
    let res = DeserializeError<UnionCaseEnum> yml 
    res.Error.Length |> shouldBeGreaterThan 0
    res.Error.Head |> fun m -> m.Message.StartsWith("Union case 'Four' not availble in type") |> shouldEqual true


[<Test>]
let ``Deserialize - Mapping blockstyle - Sunny Day`` () =
    let yml = "{a : b, c : d}"
    let res = DeserializeSuccess<Map<string,string>> yml
    res.["a"] |> shouldEqual "b"
    res.["c"] |> shouldEqual "d"


[<Test>]
let ``Deserialize - Mapping flowstyle - Sunny Day`` () =
    let yml = "
    a : b
    c : d"
    let res = DeserializeSuccess<Map<string,string>> yml
    res.["a"] |> shouldEqual "b"
    res.["c"] |> shouldEqual "d"


[<Test>]
let ``Deserialize - Mapping with DU enum - Sunny Day`` () =
    let yml = "{a : Zero, c : two}"
    let res = DeserializeSuccess<Map<string,UnionCaseEnum>> yml
    res.["a"] |> shouldEqual UnionCaseEnum.Zero
    res.["c"] |> shouldEqual UnionCaseEnum.Two


[<Test>]
let ``Deserialize - Mapping with DU and record - Sunny Day`` () =
    let yml = "
        Zero : { Name: 'Frank', Age:  43 }
        two  : { Name: 'Rosi', Age:  45 } "

    let res = DeserializeSuccess<Map<UnionCaseEnum,UCData1>> yml
    res.[UnionCaseEnum.Zero] |> shouldEqual {UCData1.Name = "Frank"; Age = 43 }
    res.[UnionCaseEnum.Two] |> shouldEqual {UCData1.Name = "Rosi"; Age = 45 }


type RecursiveType = {
        Data : int
        Next : RecursiveType option
    }


[<Test>]
let ``Deserialize - Recursive Type - Sunny Day`` () =
    let yml = "
Data: 1
Next:
    Data: 2
"
    let res = DeserializeSuccess<RecursiveType> yml
    res.Data            |> shouldEqual 1
    res.Next.Value.Data |> shouldEqual 2
    res.Next.Value.Next |> shouldEqual None


let CustomDeserializeSuccess<'tp> opt yml = 
    let r = DeserializeWithOptions<'tp> opt yml
    r
    |> List.head
    |>  function
        |   Success s -> s.Data
        |   Error e -> failwith "Unexpected error"


let CustomDeserializeSuccessAndWarnings<'tp> opt yml = 
    let r = DeserializeWithOptions<'tp> opt yml
    r
    |> List.head
    |>  function
        |   Success s -> s.Data, s.Warn
        |   Error e -> failwith "Unexpected error"
        

let CustomDeserializeError<'tp> opt yml = 
    DeserializeWithOptions<'tp> opt yml
    |> List.head
    |>  function
        |   Success _ -> failwith "Unexpected success"
        |   Error e -> e


type ProcessingOptionsType = {
    Field1 : string
    Field2 : string option
}


[<Test>]
let ``Deserialize - Processing Options Default - Sunny Day`` () =
    let yml = "
Field1: testvalue
#   below shuld not cause any warning or error bc the input is suffcient for the target type
NoWarnMap: mappingValue
"

    //  default
    let res = CustomDeserializeSuccess<ProcessingOptionsType> [] yml
    res.Field1 |> shouldEqual "testvalue"
    res.Field2 |> shouldEqual None
    
    //  explicit
    let res = CustomDeserializeSuccess<ProcessingOptionsType> [MappingMode(MapYaml.ToModelOnly)] yml
    res.Field1 |> shouldEqual "testvalue"
    res.Field2 |> shouldEqual None


[<Test>]
let ``Deserialize - Processing Options Warnings - Sunny Day`` () =
    let yml = "
Field1: testvalue
#   below must cause a warning bc they're not part of the target type
ShouldWarnMap : mappingValue
"

    //  default
    let (res, wns) = CustomDeserializeSuccessAndWarnings<ProcessingOptionsType> [MappingMode(MapYaml.WithCrossCheck)] yml
    res.Field1 |> shouldEqual "testvalue"
    res.Field2 |> shouldEqual None

    wns |> List.length |> shouldEqual 1
    wns.Head.Message.StartsWith("Field 'ShouldWarnMap' cannot be mapped to target type") |> shouldEqual true


[<Test>]
let ``Deserialize - Processing Options Errors - Sunny Day`` () =
    let yml = "
Field1: testvalue
#   below must cause a error bc they're not part of the target type
ShouldErrorMap : mappingValue
"

    //  default
    let err = CustomDeserializeError<ProcessingOptionsType> [MappingMode(MapYaml.AndRequireFullProjection)] yml

    err.Error |> List.length |> shouldEqual 1
    err.Error.Head.Message.StartsWith("Field 'ShouldErrorMap' cannot be mapped to target type") |> shouldEqual true


[<Test>]
let ``Deserialize - IDictionary Mapping blockstyle - Sunny Day`` () =
    let yml = "{a : b, c : d}"
    let res = DeserializeSuccess<System.Collections.Generic.Dictionary<string,string>> yml
    res.["a"] |> shouldEqual "b"
    res.["c"] |> shouldEqual "d"


//#if NETCOREAPP2_0
//open Open.Collections

//[<Test>]
//let ``Deserialize - OrderedDictionary Mapping blockstyle - Sunny Day`` () =
//    let yml = "{a : b, c : d}"
//    let res = DeserializeSuccess<Open.Collections.OrderedDictionary<string,string>> yml
//    res.["a"] |> shouldEqual "b"
//    res.["c"] |> shouldEqual "d"
    
//    res.Keys |> List.ofSeq |> shouldEqual ["a"; "c"]


//#endif


type Doc = {
    t1: string list
    t2: Map<string, int>
    t3: Map<int, string>
}


[<Test>]
let ``Deserialize - undesired boolean conversion - Sunny Day`` () =
    let yml = "
t1: [yes, maybe, no]
t2: {true: 2, false: 0, ambiguous: 1, null: -1}
t3: {1: yes, 0: maybe, -1: no}"


    let r = DeserializeWithOptions<Doc> [MappingMode(MapYaml.AndRequireFullProjection)] yml

    r
    |> List.head
    |>  function
        |   Success s -> 
            s.Data.t1 |> List.head  |> shouldEqual "yes"
            s.Data.t2.["true"]      |> shouldEqual 2
            s.Data.t2.["false"]     |> shouldEqual 0
            s.Data.t2.["ambiguous"] |> shouldEqual 1
            s.Data.t2.["null"]      |> shouldEqual -1
            s.Data.t3.[1]           |> shouldEqual "yes"
            s.Data.t3.[0]           |> shouldEqual "maybe"
            s.Data.t3.[-1]          |> shouldEqual "no"
        |   Error e -> failwith "Unexpected error"




