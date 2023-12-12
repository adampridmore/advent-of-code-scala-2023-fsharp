module IntHelperTests

open System
open FsUnit.Xunit
open Xunit
open IntHelper

[<Fact>]
let ``Parse non number to None`` () =
    (TryParseInt("a")) |> should equal None

[<Fact>]
let ``Parse number`` () =
    (TryParseInt("12")) |> should equal (Some(12))

[<Fact>]
let ``GetValue for some``() =
    Some(4) |> GetValue |> should equal 4

[<Fact>]
let ``GetValue for none``() =
    try
        None |> GetValue |> ignore
        failwith("Exception not thrown")
    with
        | _ -> ()

let GetValue(x: Option<int>) : int = x.Value

[<Fact>]
let ``ParseInt for number`` =
    "73" |> ParseInt |> should equal 73
