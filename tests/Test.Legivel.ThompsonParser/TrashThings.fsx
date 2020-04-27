

open System.Text.RegularExpressions


let m = Regex.Match("AB", "(A)B|AB")
m.Groups.[1]
m.Value




