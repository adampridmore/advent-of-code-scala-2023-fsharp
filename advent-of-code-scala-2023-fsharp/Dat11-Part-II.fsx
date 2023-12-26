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
  | G of x : int64 * y : int64 

let parseCell(y:int64)(x: int64, c: char) : Cell = 
  match c with
  | '.' -> E
  | '#' -> G(x,y)

let parseLine(y: int64, text: string) : array<Cell> = 
  text.ToCharArray()
  |> Seq.indexed
  |> Seq.map(fun (i,x) -> (int64 i),x)
  |> Seq.map(parseCell(y))
  |> Seq.toArray

let cells = 
  // example.Split('\n')
  System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day11.input.txt")
  |> Seq.indexed
  |> Seq.map(fun (i,x) -> (int64 i), x)
  |> Seq.map(parseLine)
  |> Seq.toArray

let isGalaxy(cell: Cell) = 
  match (cell) with
  | G(_) -> true
  | _ -> false

let getExpandColumns(cells: array<array<Cell>>) : seq<int64> =
  let columnHasGalaxies(columnIndex: int64) : bool = 
    seq{0..(cells.Length-1)}
    |> Seq.exists(fun rowIndex -> cells[rowIndex][(columnIndex|> int)] |> isGalaxy)

  seq{0..(cells.[0].Length-1)}
  |> Seq.map(int64)
  |> Seq.filter(fun columnIndex -> not (columnHasGalaxies(columnIndex)) )

let getExpandRows(cells: array<array<Cell>>) : seq<int64> = 
  let rowHasGalaxies(row: array<Cell>) : bool = 
    row
    |> Seq.exists(isGalaxy)

  cells 
  |> Seq.indexed
  |> Seq.map(fun (i, x) -> (int64 i),x)
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

// let expansion = int64 100
let expansion = int64 1000000 

let getShiftPos(doX: bool)(expanders:seq<int64>)(galaxy: Cell) : int64 =  
  let multiplier = expansion - 1L
  match (doX, galaxy) with
  | false, G(_,y) -> (  (expanders |> Seq.filter(fun i -> i < y) |> Seq.length |> int64) * multiplier + y)
  | true, G(x,_) ->  (  (expanders |> Seq.filter(fun i -> i < x) |> Seq.length |> int64) * multiplier + x)

let shiftGalaxy(galaxy: Cell) : Cell = 
  let newX = galaxy |> getShiftPos(true)(expandColumns)
  let newY = galaxy |> getShiftPos(false)(expandRows)
  G(newX, newY)

let shiftedGalaxies = 
  cells
  |> filterGalaxies
  |> Seq.map shiftGalaxy
  |> Seq.toList

let abs(x: int64) : int64 = 
  if (x >= 0L) then x
  else -x

let distance(g1 : Cell,g2: Cell) : int64 = 
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

let totalDistance=
  shiftedGalaxies 
  |> combinations 2
  |> Seq.map(fun list -> (list[0], list[1]) )
  |> Seq.map(distance)
  |> Seq.sum

printfn "Expansion: %d TotalDistance: %d" expansion totalDistance
