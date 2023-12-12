module Dat2Tests

open Day2
open FsUnit.Xunit
open Xunit


[<Fact>]
let ``parseBeadScore for "4 red"``() =
  let x =   {
    BeadScore.count = 4;
    colour = "red"
  }

  "4 red" |> parseBeadScore |> should equal x
  
[<Fact>]
let ``parseReveal for "3 blue, 4 red``() = 
  let expected = ([{
      BeadScore.count = 3;
      colour = "blue"
    };{
      BeadScore.count = 4;
      colour = "red"
    }])

  let expected2 = {
    Reveal.BeadScores = expected
  }
 
  let parsed = "3 blue, 4 red" |> parseReveal 

  parsed |> should equal expected2


[<Fact>]
let ``stuff``() = 
  let line = "Game 1: 3 blue, 4 red; 2 green"

  let expected = {
    Game.Index = 1;
    Reveals = [{
      BeadScores = [{
        BeadScore.count = 3;
        colour = "blue"
      };{
        BeadScore.count = 4;
        colour = "red"
      }]
    };{
      BeadScores = [{
        BeadScore.count = 2;
        colour = "green"
      }]
    }]}

  line |> parseLineToGame |> should equal expected
