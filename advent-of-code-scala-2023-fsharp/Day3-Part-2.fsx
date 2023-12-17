open System.Text.RegularExpressions

let exampleData = """
467..114..
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
  |> Seq.map(fun m -> {
      value = System.Convert.ToInt32(m.Value);
      columnIndex = m.Index;
      rowIndex = rowIndex;
      characterLength = m.Value.Length
      }
    )

let lines = 
  System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day3.input.txt")
  // exampleData.Split("\n")
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
  
let isGear(c:char) : bool = 
  match (c) with
  | '*' -> true
  | _ -> false

let isGearNumber(number: Number) : bool = 
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
  |> Seq.filter isGear
  |> Seq.isEmpty 
  |> not

numbers 
|> Seq.filter(isGearNumber)

type Gear ={
  x: int;
  y: int
}

let getGears(lines: seq<string>) : seq<Gear> = 
  let getGearsForLine(lineIndex: int, line: string) : seq<Gear> =

    let toGear(c: char, columnIndex: int, lineIndex: int) : Option<Gear> = 
      match (c) with
      | '*' -> Some({Gear.x = columnIndex; y = lineIndex})
      | _ -> None

    line.ToCharArray()
    |> Seq.indexed
    |> Seq.map(fun (columnIndex: int , c: char) -> toGear(c, columnIndex, lineIndex) )
    |> Seq.choose id

  lines
  |> Seq.indexed
  |> Seq.map getGearsForLine
  |> Seq.collect(id)

let between(x:int, min: int, max: int) : bool = 
  (x >= min) && (x <= max)

let isAdjacent(number, gear) : bool = 
  let result1 =
    between(gear.y, number.rowIndex-1, number.rowIndex + 1)
  let result2 =
    between(gear.x, number.columnIndex-1, number.columnIndex + number.characterLength)

  result1 && result2

let getGearRatio(gear:Gear) : int = 
  let gearNumbers = 
    numbers
    |> Seq.filter(fun number -> isAdjacent(number, gear))

  match(gearNumbers |> Seq.toList) with
  | [a; b] -> a.value * b.value
  | _ -> 0

lines
|> getGears
|> Seq.map getGearRatio
|> Seq.sum

