open System 

let cardValues = 
  "AKQJT98765432".ToCharArray() 
  |> Seq.rev 
  |> Seq.indexed 
  |> Seq.map(fun (a,b) ->(b,a + 2))
  |> dict

type Hand(cards: string, bid: int) = 
  member this.cards = cards
  member this.bid = bid
  member this.score = 
    let groupedCards = 
      cards.ToCharArray()
      |> Seq.groupBy(id)
      |> Seq.map(fun (_, v) -> v |> Seq.length)
      |> Seq.toList
      |> List.sortDescending

    match(groupedCards) with
    | [5] -> 7 // 5 of a kind
    | 4::tail -> 6 // 4 of a kind
    | [3;2] -> 5 // Full house
    | 3::tail -> 4 // 3 of a kind
    | 2::2::tail -> 3 // 2 pair
    | 2::tail -> 2 // 2 of a kind
    | _ -> 1

  override this.ToString():string = sprintf "Card: %s Score: %d Bid: %d" cards this.score bid

let assertEquals(actualValue: obj, expectedValue: obj) : Unit = 
  if (actualValue.Equals(expectedValue)) then printfn "%A" actualValue
  else failwith (sprintf "assertEquals failed: Expected %A but was %A" expectedValue actualValue)

assertEquals(Hand("AAAAA", 1).score, 7)
assertEquals(Hand("AAAAJ", 1).score, 6)
assertEquals(Hand("AAAJJ", 1).score, 5)
assertEquals(Hand("AAAJK", 1).score, 4)
assertEquals(Hand("AAJJK", 1).score, 3)
assertEquals(Hand("AA123", 1).score, 2)
assertEquals(Hand("12345", 1).score, 1)

let compareFirstCards(hand1:Hand)(hand2:Hand) : int = 
  let x = 
    Seq.zip (hand1.cards.ToCharArray()) (hand2.cards.ToCharArray())
    |> Seq.filter(fun (a,b) -> a <> b)
    |> Seq.map(fun (a,b) ->  cardValues[a].CompareTo(cardValues[b]) )
    |> Seq.tryHead
  match x with
  | Some(x) -> x
  | None -> 0

let handCompare (hand1:Hand)(hand2:Hand) : int = 
  let result = hand1.score.CompareTo(hand2.score)
  if (result = 0) then compareFirstCards(hand1)(hand2)
  else result

let exampleHands = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""

let toTuple (array:array<'T>) : ('T * 'T) =  
  match (array |> Seq.toList) with 
  | a::b::_ -> (a,b)
  | _ -> failwith "Unable to split array to tuple"

//exampleHands.Split("\n")
System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day7.input.txt")
|> Seq.filter(fun x -> x <> "" )
|> Seq.map(fun line -> line.Split(" ") |> toTuple)
|> Seq.map(fun (cards, bid) -> Hand(cards, System.Convert.ToInt32(bid)))
|> Seq.sortWith handCompare
|> Seq.indexed
|> Seq.map(fun (index, hand) -> (index + 1) * hand.bid)
|> Seq.sum
|> printfn "%A"
