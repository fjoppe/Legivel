/// Contains yaml-processor attributes
module Legivel.Attributes

//  https://stackoverflow.com/questions/44804767/f-create-custom-attribute-to-expression

/// Used to make a mappng between yaml-field name, and a target property-name
type YamlFieldAttribute(Name : string) = 
    inherit System.Attribute()
    member this.Name' = Name

/// Used to make a mapping between a yaml-value, and a target value (ie Union-Case)
type YamlValueAttribute(Id : string) = 
    inherit System.Attribute()
    member this.Id' = Id

