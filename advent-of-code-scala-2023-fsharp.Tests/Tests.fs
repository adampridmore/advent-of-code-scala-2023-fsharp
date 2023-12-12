module Tests

open System
open FsUnit.Xunit
open Xunit

[<Fact>]
let ``My test`` () =
    6 |> should equal 6

[<Fact>]
let test1() = 
    Assert.Equal("Dave", advent_of_code_scala_2023_fsharp.Say.hello("Dave"))
