let line = "a1abc2"

let ParseInt (str: string) : int option = 
  match str |> System.Int32.TryParse with
  | true, out -> Some out
  | false, _ -> None

let rec FirstDigit(line: string) : int =
  let character = line.ToCharArray() |> Seq.head

  let x = character |> string |> ParseInt
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
  ParseInt(firstDigit + lastDigit).Value
  
line |> processLine

let ans = 
  System.IO.File.ReadAllLines("Day1.input.txt")
  |> Seq.map(processLine)
  |> Seq.sum
  // 54338

printfn "The answer is %d" ans
