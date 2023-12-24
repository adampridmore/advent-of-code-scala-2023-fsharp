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
