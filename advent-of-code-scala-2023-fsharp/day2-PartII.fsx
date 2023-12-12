open FSharp.Collections;

#load "IntHelper.fs"
#load "StringHelper.fs"
#load "Day2.fs"

open IntHelper
open StringHelper
open Day2

let line = "Game 12: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

let x = line |> parseLineToGame
// (x.Reveals |> Seq.head)


let power(reveals: seq<Reveal>) : int =
  //  let reveals = x.Reveals
  reveals
  |> Seq.collect(fun reveal -> reveal.BeadScores)
  |> Seq.groupBy(fun score -> score.colour)
  |> Seq.map(fun (key, values) -> (key, values |> Seq.max))
  |> Seq.map(fun (_, x) -> x.count)
  |> Seq.fold (*) 1

  
let lines = """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

let realLines =
  System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day2.input.txt")

// lines.Split("\n")
realLines
|> Seq.filter(fun text -> (System.String.IsNullOrWhiteSpace(text) |> not))
|> Seq.map(parseLineToGame)
|> Seq.map(fun game -> game.Reveals |> power)
|> Seq.sum

// Total: 76008
