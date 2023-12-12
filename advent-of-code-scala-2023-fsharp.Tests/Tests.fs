module Tests

open System
open Xunit

[<Fact>]
let ``My test`` () =
    Assert.True(true)

[<Fact>]
let test1() = 
    Assert.Equal("Dave", advent_of_code_scala_2023_fsharp.Say.hello("Dave"))
