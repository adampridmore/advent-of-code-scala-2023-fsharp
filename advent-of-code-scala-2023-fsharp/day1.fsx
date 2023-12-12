#load "IntHelper.fs"
open IntHelper

let line = "a1abc2"


let rec FirstDigit(line: string) : int =
  let character = line.ToCharArray() |> Seq.head

  let x = character |> string |> TryParseInt
  match x with
  | Some(x) -> x
  | None -> FirstDigit(line.Substring(1))

let LastDigit(line: string) : int =
  let reverseLine = new string(line.ToCharArray() |> Array.rev)
  reverseLine |> FirstDigit

line |> FirstDigit
line |> LastDigit

let processLine(line:string) : int =
  let firstDigit = line |> FirstDigit |> string
  let lastDigit = line |> LastDigit |> string
  ParseInt(firstDigit + lastDigit)
  
line |> processLine

let ans = 
  System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day1.input.txt")
  |> Seq.map(processLine)
  |> Seq.sum
  // 54338

printfn "The answer is %d" ans
