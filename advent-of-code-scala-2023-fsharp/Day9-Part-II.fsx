// let example = """0 3 6 9 12 15
// 1 3 6 10 15 21
// 10 13 16 21 30 45"""

let all( predicate : 'T -> bool) (s : seq<'T>) : bool =
  s
  |> Seq.filter(fun x -> not ( predicate x))
  |> Seq.isEmpty

let getRows(numbers: list<int>) : list<list<int>> = 
  let nextRow(row : list<int32>) : Option<list<int32>*list<int32>> = 
    if (row |> all (fun x -> x = 0)) then None
    else 
      let nextRow = 
        row 
        |> Seq.pairwise 
        |> Seq.map(fun (a, b) ->(b-a))
        |> Seq.toList

      Some(nextRow, nextRow)

  let tail =
    numbers
    |> List.unfold nextRow 
    
  (numbers |> Seq.toList)::tail

let toIntList(line: string) : list<int> = 
  line.Split(" ")
  |> Seq.map System.Convert.ToInt32
  |> Seq.toList

let calculateNextValue(line:list<int>) : int = 
  let fn (state: int) (rowInt: int) : int = 
    rowInt - state

  let startOfRowValues = 
    line
    |> getRows
    |> Seq.map(fun numbers -> Seq.head(numbers))
    |> Seq.rev

  Seq.fold fn 0 startOfRowValues

System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day9.input.txt")
|> Seq.map toIntList
|> Seq.map calculateNextValue
|> Seq.sum

// let example = """0 3 6 9 12 15
// 1 3 6 10 15 21
// 10 13 16 21 30 45"""
// let lines = example.Split("\n")|>Seq.toList
// let line1 = lines[0] 
// let line2 = lines[1] 
// let line3 = lines[2] 

// // "10 13 16 21 30 45" 
// line2
// |> toIntList
// |> calculateNextValue
