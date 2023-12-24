let example = """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."""

type Cell =
  | E
  | G of x : int * y : int 

let parseCell(y:int)(x: int, c: char) : Cell = 
  match c with
  | '.' -> E
  | '#' -> G(x,y)

let parseLine(y: int, text: string) : array<Cell> = 
  text.ToCharArray()
  |> Seq.indexed
  |> Seq.map(parseCell(y))
  |> Seq.toArray

let cells = 
  example.Split('\n')
  // System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day11.input.txt")
  |> Seq.indexed
  |> Seq.map(parseLine)
  |> Seq.toArray

let isGalaxy(cell: Cell) = 
  match (cell) with
  | G(_) -> true
  | _ -> false

let getExpandColumns(cells: array<array<Cell>>) : seq<int> =
  let columnHasGalaxies(columnIndex) : bool = 
    seq{0..(cells.Length-1)}
    |> Seq.exists(fun rowIndex -> cells[rowIndex][columnIndex] |> isGalaxy)

  seq{0..(cells.[0].Length-1)}
  |> Seq.filter(fun columnIndex -> not (columnHasGalaxies(columnIndex)) )

let getExpandRows(cells: array<array<Cell>>) : seq<int> = 
  let rowHasGalaxies(row: array<Cell>) : bool = 
    row
    |> Seq.exists(isGalaxy)

  cells 
  |> Seq.indexed
  |> Seq.map(fun (rowIndex, row) -> rowIndex, rowHasGalaxies row)
  |> Seq.filter(fun (_, hasGalaxy) -> hasGalaxy = false)
  |> Seq.map(fst)

let expandColumns = cells |> getExpandColumns
let expandRows = cells |> getExpandRows

let filterGalaxies(calls: array<array<Cell>>) : seq<Cell> = 
  seq {
    for row in cells do
      for cell in row do
        yield cell
  }
  |> Seq.filter(isGalaxy)
  |> Seq.choose(fun x ->
        match x with 
        | G(_) as g -> Some g
        | _ -> None
      )

let getShiftPos(doX: bool)(expanders:seq<int>)(galaxy: Cell) : int = 
  // let multiplier = 10000000 - 1
  let multiplier = 100 - 1
  match (doX, galaxy) with
  | false, G(_,y) -> (  (expanders |> Seq.filter(fun i -> i < y) |> Seq.length) * multiplier + y)
  | true, G(x,_) ->  (  (expanders |> Seq.filter(fun i -> i < x) |> Seq.length) * multiplier + x)

let shiftGalaxy(galaxy: Cell) : Cell = 
  let newX = galaxy |> getShiftPos(true)(expandColumns)
  let newY = galaxy |> getShiftPos(false)(expandRows)
  G(newX, newY)

let shiftedGalaxies = 
  cells
  |> filterGalaxies
  |> Seq.map shiftGalaxy
  |> Seq.toList

let abs(x: int) : int = 
  if (x >= 0) then x
  else -x

let distance(g1 : Cell,g2: Cell) : int = 
  match(g1, g2) with
  | G(x1, y1), G(x2, y2) -> abs(x2-x1) + abs(y2-y1)

let rec combinations n l =
  match (n,l) with
  | (0,_) -> [[]]
  | (_,[]) -> []
  | (n,x::xs) ->
      let useX = List.map (fun l -> x::l) (combinations (n-1) xs)
      let noX = combinations n xs
      useX @ noX

shiftedGalaxies 
|> combinations 2
// |> Seq.length
|> Seq.map(fun list -> (list[0], list[1]) )
|> Seq.map(distance)
|> Seq.sum
