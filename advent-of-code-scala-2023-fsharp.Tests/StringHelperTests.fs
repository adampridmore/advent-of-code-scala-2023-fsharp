module StringHelperTests

open System
open FsUnit.Xunit
open Xunit
open StringHelper

[<Fact>]
let ``Parse non number to None`` () =
     "abc" |> Reverse |> should equal "cba"
