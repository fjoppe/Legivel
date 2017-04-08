
open System
open System.Text
open System.Text.RegularExpressions
open System.Globalization

let utf8 = new UTF8Encoding();

let sample = "Sosa did fine.\\u263A\\u263A"

utf8.GetBytes(sample)

let DecodeEncodedNonAsciiCharacters value =
        Regex.Replace(
            value,
            @"\\u(?<Value>[a-zA-Z0-9]{4})",
            (fun (m:Match) -> (char(Int32.Parse(m.Groups.["Value"].Value, NumberStyles.HexNumber))).ToString()))

DecodeEncodedNonAsciiCharacters sample
