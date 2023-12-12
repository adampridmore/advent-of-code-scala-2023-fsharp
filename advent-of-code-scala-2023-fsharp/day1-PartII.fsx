#load "IntHelper.fs"
#load "StringHelper.fs"
open IntHelper
open StringHelper

let line = "abcone2threexyz"



let digitText = 
  seq{1..9}
  |> Seq.map(fun i -> (i.ToString(),i))
  |> Seq.append [
    "one", 1;
    "two", 2;
    "three", 3;
    "four", 4;
    "five", 5;
    "six", 6;
    "seven", 7;
    "eight", 8;
    "nine", 9;   
  ]

let TryMatchText((digitText: (string * int) seq ))(line: string) : Option<int> = 
  digitText
  |> Seq.filter(fun (text, _) -> line.StartsWith(text))
  |> Seq.map(snd)
  |> Seq.tryHead

let rec FindDigit((digitText:  (string * int) seq ))(line: string) : int =
  match line |> TryMatchText(digitText) with
  | Some(x) -> x
  | None -> FindDigit(digitText)(line.Substring(1))

let rec FirstDigit(line: string) : int =
  FindDigit(digitText)(line)

let LastDigit(line: string) : int =
  let reverseLine = new string(line.ToCharArray() |> Array.rev)
  let digitTextReverse = 
    digitText 
    |> Seq.map(fun (text,value) -> (text |> Reverse, value))
  FindDigit(digitTextReverse)(reverseLine)

// line |> FirstDigit
// line |> LastDigit

let processLine(line:string) : int =
  let firstDigit = line |> FirstDigit |> string
  let lastDigit = line |> LastDigit |> string
  ParseInt(firstDigit + lastDigit)
  
// line |> processLine

let ans = 
  System.IO.File.ReadAllLines("./advent-of-code-scala-2023-fsharp/Day1.input.txt")
  |> Seq.map(processLine)
  |> Seq.sum
  // 53389

printfn "The answer is %d" ans
