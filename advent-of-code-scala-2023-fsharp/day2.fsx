open FSharp.Collections;

#load "IntHelper.fs"
#load "StringHelper.fs"
#load "Day2.fs"

open IntHelper
open StringHelper
open Day2



//only 12 red cubes, 13 green cubes, and 14 blue cubes
let isBadBeadScore(score: BeadScore) : bool = 
  match score.colour with
  | "red" when score.count > 12 -> true
  | "green" when score.count > 13 -> true
  | "blue" when score.count > 14 -> true
  | _ -> false

let isBadReveal(reveal: Reveal) : bool =
  reveal.BeadScores
  |> Seq.exists(isBadBeadScore)
  

let isBadGame(game: Game) : bool =
  game.Reveals
  |> Seq.exists(isBadReveal)
  

// let line = "Game 12: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

// let x = line |> parseLineToGame
// (x.Reveals |> Seq.head)


let lines = """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

let realLines =
  System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day2.input.txt")

//lines.Split("\n")
realLines
|> Seq.filter(fun text -> (System.String.IsNullOrWhiteSpace(text) |> not))
|> Seq.map(parseLineToGame)
|> Seq.filter(fun line -> not (isBadGame(line)))
|> Seq.map(fun game -> game.Index)
|> Seq.sum

// Total: 2348
