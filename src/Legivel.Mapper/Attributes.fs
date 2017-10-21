module Legivel.Attributes

//  https://stackoverflow.com/questions/44804767/f-create-custom-attribute-to-expression
type YamlFieldAttribute(Name : string) = 
    inherit System.Attribute()
    member this.Name' = Name


type YamlValueAttribute(Id : string) = 
    inherit System.Attribute()
    member this.Id' = Id

