// let example = """0 3 6 9 12 15
// 1 3 6 10 15 21
// 10 13 16 21 30 45"""


let all(predicate : 'T -> bool) (s : seq<'T>) : bool =
  s
  |> Seq.filter(fun x -> not ( predicate x))
  |> Seq.isEmpty

let isZero x = x = 0

let getRows(numbers: list<int>) : list<list<int>> = 
  let nextRow(row : list<int32>) : Option<list<int32>*list<int32>> = 
    if (row |> all isZero) then None
    else 
      let nextRow = 
        row 
        |> Seq.pairwise 
        |> Seq.map(fun (a, b) -> b - a)
        |> Seq.toList

      Some(nextRow, nextRow)

  let tail =
    numbers
    |> List.unfold nextRow 
    
  numbers::tail

let toIntList(line: string) : list<int> = 
  line.Split(" ")
  |> Seq.map System.Convert.ToInt32
  |> Seq.toList

let calculatePreviousValue(line:list<int>) : int = 
  let previousValue(state: int) (rowInt: int) : int = 
    rowInt - state

  let startOfRowValues = 
    line
    |> getRows
    |> Seq.map(fun numbers -> Seq.head(numbers))
    |> Seq.rev

  Seq.fold previousValue 0 startOfRowValues

System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day9.input.txt")
|> Seq.map toIntList
|> Seq.map calculatePreviousValue
|> Seq.sum
