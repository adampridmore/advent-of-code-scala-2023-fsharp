open System.Text.RegularExpressions
open System

let exampleText = ("""
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
""")

let lines =
  // exampleText.Split("\n", StringSplitOptions.RemoveEmptyEntries)
  // exampleData.Split("\n")
  System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day4.input.txt")
  |> Seq.filter(fun line -> line <> "")

type Card(index: int, winningNumbers: seq<int>, yourNumbers : seq<int>) =
  member this.index = index
  member this.winCount = 
    yourNumbers
    |> Seq.filter(fun yourNumber -> winningNumbers |> Seq.exists(fun wn -> yourNumber = wn))
    |> Seq.length

  override this.ToString() = sprintf "Index %d winCount: %d" index this.winCount

let parseNumbers(text: String) : seq<int> = 
  text.Split(" ", StringSplitOptions.RemoveEmptyEntries)
  |> Seq.map(System.Convert.ToInt32)

let regex = Regex "Card\s+([0-9]+)"

let parseLineToCard(line: string) : Card  = 
  // printfn "line: %s" line
  let lineParts = line.Split(":")
  let cardText = lineParts.[0]
  let scoringText = lineParts.[1].Split("|")

  let result = regex.Match(cardText)
  let cardIndex = result.Groups.[1].Value |> System.Convert.ToInt32

  let winningNumbers = 
    scoringText.[0] |> parseNumbers

  let yourNumbers = scoringText.[1] |> parseNumbers

  Card(cardIndex, winningNumbers, yourNumbers)

let cardScores = 
  lines
  |> Seq.map parseLineToCard
  |> Seq.map (fun card -> card)
  |> Seq.toList


let cardsWithCount = new System.Collections.Generic.Dictionary<int,int>()

cardScores
|> Seq.iter(fun card -> cardsWithCount.Add(card.index, 1))

cardScores 
|> Seq.iter(fun card -> 
    let numberOfCurrentCards = cardsWithCount.[card.index];

    seq{card.index + 1 .. (card.index+card.winCount)}
    |> Seq.iter(fun index -> cardsWithCount.[index] <- cardsWithCount.[index] + numberOfCurrentCards)
    ()
  )

cardsWithCount.Values
|> Seq.sum
