open System.Text.RegularExpressions;

let TryParseInt (str: string) : int option = 
  match str.Trim() |> System.Int32.TryParse with
  | true, out -> Some out
  | false, _ -> None

let GetValue(x: Option<int>) : int = x.Value

let ParseInt = TryParseInt >> GetValue

type BeadScore = {
  count : int;
  colour: string
}

type Reveal = {
  BeadScores: seq<BeadScore>;
}

type Game = {
  Index: int;
  Reveals: seq<Reveal>;
}

//4 red
let parseBeadScore(beadScore: string) : BeadScore = 
  let result = beadScore.Trim().Split(" ")
  let beadCount = result.[0] |> ParseInt
  let beadColor = result.[1]
  {
    BeadScore.count = beadCount;
    colour = beadColor
  }

//3 blue, 4 red
let parseReveal(revealText: string) : Reveal =
        
  let beadScores = 
    revealText.Split(",")
    |> Seq.map(parseBeadScore)

  {
    Reveal.BeadScores = beadScores
  }

// 3 blue, 4 red; 1 red, 2 green
let parseReveals(revealsText: string) : seq<Reveal> =
  let reveals = revealsText.Split(";")

  reveals
  |> Seq.map(parseReveal)

let parseLineToGame(line: string) : Game =
  let split = line.Split(":")
  let gameHeaderText = split.[0]
  let regexGameHeader = Regex("Game\s(\d+)")
  let result = regexGameHeader.Match(gameHeaderText)
  let gameIndex = result.Groups.[1].Value |> ParseInt

  let revealsText = split.[1].Trim()
  let reveals = revealsText |> parseReveals

  { 
    Game.Index = gameIndex;
    Reveals = reveals
  }


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
  System.IO.File.ReadAllLines("Day2.input.txt")

//lines.Split("\n")
realLines
|> Seq.filter(fun text -> (System.String.IsNullOrWhiteSpace(text) |> not))
|> Seq.map(parseLineToGame)
|> Seq.filter(fun line -> not (isBadGame(line)))
|> Seq.map(fun game -> game.Index)
|> Seq.sum

// Total: 2348
