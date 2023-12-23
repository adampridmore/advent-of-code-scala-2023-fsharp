open FSharp.Core.LanguagePrimitives

let example1 = """.....
.S-7.
.|.|.
.L-J.
....."""

// let example2 = """..F7.
// .FJ|.
// SJ.L7
// |F--J
// LJ..."""


//let example3 = """.S"""
[<Measure>] type RowIndex
[<Measure>] type ColumnIndex

[<AbstractClass>]
// [<CustomEquality;CustomComparison>]
type Pipe() =
  override x.ToString() = x.GetType().Name
  override x.Equals(yobj) = x.GetType() = yobj.GetType()
  
  abstract member left: Unit -> bool
  abstract member right: Unit -> bool
  abstract member top: Unit -> bool
  abstract member bottom: Unit -> bool

type Horizontal() =
    inherit Pipe()
    override this.left() = true
    override this.right() = true
    override this.top() = false
    override this.bottom() = false

type Vertical() =
    inherit Pipe()
    override this.left() = false
    override this.right() = false
    override this.top() = true
    override this.bottom() = true
type TopLeft() =
    inherit Pipe()
    override this.left() = true
    override this.right() = true
    override this.top() = true
    override this.bottom() = false
type TopRight() =
    inherit Pipe()
    override this.left() = false
    override this.right() = true
    override this.top() = true
    override this.bottom() = false
type BottomLeft() =
    inherit Pipe()
    override this.left() = true
    override this.right() = false
    override this.top() = false
    override this.bottom() = true
type BottomRight() =
    inherit Pipe()
    override this.left() = false
    override this.right() = true
    override this.top() = false
    override this.bottom() = true
type Missing() =
    inherit Pipe()
    override this.left() = false
    override this.right() = false
    override this.top() = false
    override this.bottom() = false
type Start() =
    inherit Pipe()
    override this.left() = true
    override this.right() = true
    override this.top() = true
    override this.bottom() = true

type Coord = {
  columnIndex : int<ColumnIndex>;
  rowIndex : int<RowIndex>
}

type Cell = 
  {
    pipeType : Pipe;
    location : Coord
  }
  override this.ToString() : string = ""
  
  member this.Print(text: string) = 
    let convertedY = (int this.location.rowIndex) + 1
    let convertedX =  (int this.location.columnIndex) + 1
    printfn "%s Ln %3d, Col %3d %A " text convertedY convertedX this.pipeType 

let parsePipe(c: char) : Pipe = 
  match(c) with
  | '.' -> Missing()
  | '-' -> Horizontal()
  | '|' -> Vertical()
  | 'J' -> TopLeft()
  | 'L' -> TopRight()
  | 'F' -> BottomRight()
  | '7' -> BottomLeft()
  | 'S' -> Start()
  | _ -> raise (System.Exception(sprintf "Unknown symbol [%c]" c))

let parseLine(rowIndex:int, line:string) : array<Cell> = 
  line.ToCharArray()
  |> Seq.map parsePipe
  |> Seq.indexed
  |> Seq.map(fun (columnIndex , pipe) -> {
      Cell.location = {
          Coord.columnIndex = columnIndex * 1<ColumnIndex>
          rowIndex = rowIndex * 1<RowIndex>
        }; pipeType = pipe 
      })
  |> Seq.toArray

  // Array.empty

let parseText(text: seq<string>) = 
  text 
  |> Seq.indexed
  |> Seq.map(parseLine)
  |> Seq.toArray


let cells = 
  // example1.Split("\n")
  System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day10.input.txt")
  |> parseText

let startCell = 
  cells
  |> Array.collect(id)
  |> Seq.find(fun (c:Cell) -> c.pipeType = Start())

let isOutsideArray (array : array<'T>)(i: int) : bool =  (i < 0) || (i >= array.Length)

let getFromArray(array : array<'T>)(i: int) : Option<'T> =
  if isOutsideArray(array)(i) then None
  else Some(array[i])

let getOrElse(elseValue: 'T)(x: Option<'T>) : 'T = 
  match (x) with
  | Some(x) -> x
  | None -> elseValue

let getCell(columnIndex:int<ColumnIndex>)(rowIndex:int<RowIndex>) (cells: array<array<Cell>>): Cell =
  let missing = {
        Cell.pipeType = Missing();
        location = {
            Coord.rowIndex = rowIndex;
            columnIndex = columnIndex;
          }
      }

  match (getFromArray(cells)(int rowIndex)) with
  | Some(row) -> getFromArray(row)(int columnIndex) |> getOrElse(missing)
  | None -> missing

let getNextCells(cell: Cell) :seq<Cell> = 
  let topCell = cells |> getCell cell.location.columnIndex (cell.location.rowIndex - 1<RowIndex>) 
  let first = 
    if (cell.pipeType.top() && topCell.pipeType.bottom()) then [topCell]
    else []

  let bottomCell = cells |> getCell cell.location.columnIndex (cell.location.rowIndex + 1<RowIndex>)
  let second = 
    if (cell.pipeType.bottom() && bottomCell.pipeType.top()) then [bottomCell]
    else []

  let leftCell = cells |> getCell (cell.location.columnIndex - 1<ColumnIndex>) (cell.location.rowIndex)
  let third = 
    if (cell.pipeType.left() && leftCell.pipeType.right()) then [leftCell]
    else []

  let rightCell = cells |> getCell (cell.location.columnIndex + 1<ColumnIndex>) (cell.location.rowIndex)
  let forth = 
    if (cell.pipeType.right() && rightCell.pipeType.left()) then [rightCell]
    else []

  first @ second @ third @ forth
  
let getNextCell(currentCell: Cell, previousCell: Cell) : Option<Cell*(Cell*Cell)> = 
  let nextCell = 
    getNextCells currentCell
    |> Seq.find(fun cell -> previousCell <> cell)

  if (nextCell.pipeType = Start()) then None
  else Some(nextCell, (nextCell, currentCell))

let secondCell = startCell |> getNextCells |> Seq.head
let state : Cell*Cell = startCell , secondCell

let length = 
  Seq.unfold getNextCell state
  |> Seq.indexed
  // |> Seq.map (fun (i, x:Cell) -> x.Print(i.ToString());x)
  |> Seq.length
  |> ( (+) 1) // As the list doesn't contain the start cell
  
let ans = length / 2
// Ans 6846
