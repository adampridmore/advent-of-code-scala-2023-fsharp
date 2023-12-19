let example1 = """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"""

let example2 = """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"""

let lines = 
  //example1.Split("\n")
  System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day8.input.txt")

type Node = 
  {
    key: string;
    left: string;
    right: string
  }
  member this.isEnd : bool = this.key = "ZZZ"

  override this.ToString() : string = sprintf "key: %s left: %s right: %s isEnd: %b" this.key this.left this.right this.isEnd

let regex = new System.Text.RegularExpressions.Regex("(\w*) = \((\w*), (\w*)\)")

let parseNode(nodeText: string) : Node =
  
  let result = regex.Match(nodeText)
  let key = result.Groups.[1].Value
  let left = result.Groups.[2].Value
  let right = result.Groups.[3].Value
  
  {
    Node.key = key;
    left = left;
    right = right
  }

let nodes = 
  lines 
  |> Seq.skip 2
  |> Seq.map parseNode

let keyedNodes = 
  nodes 
  |> Seq.map(fun node -> (node.key, node))
  |> dict

type Side =
  | Left
  | Right

let toSide(c: char) : Side =
  match (c) with 
  | 'L' -> Left
  | 'R' -> Right

let allSides = 
  lines[0].ToCharArray() 
  |> Seq.map(toSide)


let nextNode(node: Node, sides: seq<Side>) : Node * seq<Side> =
  let side = sides |> Seq.head
  let nextKey =
    match (side) with
    | Left -> node.left
    | Right -> node.right
  
  let nextNode = keyedNodes[nextKey]
  let remainingSides = sides |> Seq.tail
  
  let nextSides = 
    if (remainingSides |> Seq.isEmpty) then allSides
    else remainingSides

  // printfn "%O %A %A" node sides side

  (nextNode, nextSides)

let firstNode = keyedNodes["AAA"]

nextNode(nodes |> Seq.head, allSides)

let plusOne(x) = x + 1

Seq.unfold(fun (node, sides) -> 
    let (node, remainingSide)  = nextNode(node, sides)
    Some(node, (node, remainingSide))
  ) (firstNode, allSides)
|> Seq.takeWhile(fun x -> not x.isEnd )
|> Seq.indexed
|> Seq.map(fun (i,t) -> 
  if (i % 1000 = 0) 
  then (printfn "%d" i |> ignore) 
  else ()
  t)
|> Seq.length
|> plusOne


// Ans Day1: 12737
