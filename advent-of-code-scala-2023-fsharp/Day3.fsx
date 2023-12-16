open System.Text.RegularExpressions

let exampleData = """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""

let regex = Regex "([0-9]+)"

type Number = {
  value: int;
  columnIndex: int;
  rowIndex: int;
  characterLength: int;
}

let getNumbers(rowIndex: int, line: string) : seq<Number> =
  let result = regex.Matches(line)
  result
  // |> Seq.map(fun m -> System.Convert.ToInt32(m.Value), m.Index, m.Value.Length)
  |> Seq.map(fun m -> {
      value = System.Convert.ToInt32(m.Value);
      columnIndex = m.Index;
      rowIndex = rowIndex;
      characterLength = m.Value.Length
      }
    )


let lines = 
  System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day3.input.txt")
  |> Seq.filter(fun line -> line <> "")
  |> Seq.toList
  

let numbers = 
  lines 
  |> Seq.indexed
  |> Seq.collect getNumbers

let getCellsToCheck(number: Number) : seq<int*int> = 
  seq {
    for col in (number.columnIndex-1) .. (number.columnIndex + number.characterLength) do
        for row in (number.rowIndex - 1) .. (number.rowIndex + 1) -> (row, col)
  }
  
let isSymbol(c:char) : bool = 
  match (c) with
  | c when System.Char.IsDigit(c) -> false
  | c when c = '.' -> false
  | _ -> true

let isPartNumber(number: Number) : bool = 
  let getCharacter (rowIndex:int,columnIndex:int) : char = 
    match (rowIndex, columnIndex) with
    | (r,_) when r < 0 || r >= (lines.Length - 1) -> '.'
    | (_,c ) when c < 0 || c >= (lines[0].Length - 1) -> '.'
    | (r,c) ->
      // (printfn "r: %d c: %d Number:%A" r c number);
      lines[rowIndex][c]

  number 
  |> getCellsToCheck
  |> Seq.map getCharacter
  |> Seq.filter isSymbol
  |> Seq.isEmpty 
  |> not

numbers 
|> Seq.filter(isPartNumber)
|> Seq.map(fun n -> n.value)
|> Seq.sum

