#load "StringHelper.fs"
open StringHelper

let line = "a1abc2"

let rec FirstDigit(line: string) : int =
  let character = line.Chars(0)
  
  if (System.Char.IsDigit(character)) then
    System.Int32.Parse(character.ToString())
  else
    FirstDigit(line.Substring(1))

let LastDigit(line: string) : int = 
  let reverseLine = line |> Reverse
  reverseLine |> FirstDigit

let processLine(line:string) : int =
  let firstDigit = line |> FirstDigit |> string
  let lastDigit = line |> LastDigit |> string
  System.Int32.Parse(firstDigit + lastDigit)
  
line |> processLine

let ans = 
  System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day1.input.txt")
  |> Seq.map(processLine)
  |> Seq.sum
  // 54338

printfn "The answer is %d" ans
