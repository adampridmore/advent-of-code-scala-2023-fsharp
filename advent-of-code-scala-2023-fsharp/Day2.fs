module Day2

open IntHelper
open System.Text.RegularExpressions;
open FSharp.Collections;

type BeadScore = {
  count : int;
  colour: string
}

type Reveal = {
  BeadScores: List<BeadScore>;
}

type Game = {
  Index: int;
  Reveals: List<Reveal>;
}

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
    Reveal.BeadScores = beadScores |> Seq.toList
  }


// TODO: Test
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
  let reveals = revealsText |> parseReveals |> Seq.toList

  { 
    Game.Index = gameIndex;
    Reveals = reveals
  }
