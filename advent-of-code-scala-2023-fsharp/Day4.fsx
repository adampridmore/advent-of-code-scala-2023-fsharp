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

let lines = exampleText.Split("\n", StringSplitOptions.RemoveEmptyEntries)

type Card = {
  index: int;
  winningNumbers: seq<int>;
  yourNumbers : seq<int>
}

let parseNumbers(text: String) : seq<int> = 
  text.Split(" ", StringSplitOptions.RemoveEmptyEntries)
  |> Seq.map(System.Convert.ToInt32)

let regex = Regex "Card ([0-9]+)"
let parseLineToCard(line: string) : Card  = 
  let lineParts = line.Split(":")
  let cardText = lineParts.[0]
  let scoreingText = lineParts.[1].Split("|")

  let result = regex.Match(cardText)
  let cardIndex = result.Groups.[1].Value |> System.Convert.ToInt32

  let winningNumbers = 
    scoreingText.[0] |> parseNumbers

  let yourNumbers = scoreingText.[1] |> parseNumbers

  {
    Card.index = cardIndex;
    winningNumbers = winningNumbers;
    yourNumbers = yourNumbers
  }

let countWinningNumbers(card: Card) : int = 
  card.yourNumbers
  |> Seq.filter(fun yourNumber -> card.winningNumbers |> Seq.exists(fun wn -> yourNumber = wn))
  |> Seq.length

let pow(x: int)(y: int) : int = (x |> float) ** (y |> float) |> int32

pow 2 4

lines
|> Seq.map parseLineToCard
|> Seq.map countWinningNumbers
|> Seq.map (fun count -> pow 2 (count - 1))
|> Seq.sum
