let line = "abcone2threexyz"

let ParseInt (str: string) : int option = 
  match str |> System.Int32.TryParse with
  | true, out -> Some out
  | false, _ -> None

let Reverse( s : string) : string = 
  let charArray = s.ToCharArray();
  new string( charArray |> Array.rev)

let matchText = 
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

let TryMatchText((matchText:  (string * int) seq ))(line: string) : Option<int> = 
  matchText 
  |> Seq.filter(fun (text, value) -> line.StartsWith(text))
  |> Seq.map(snd)
  |> Seq.tryHead

let rec FindDigit((matchText:  (string * int) seq ))(line: string) : int =
  match line |> TryMatchText(matchText) with
  | Some(x) -> x
  | None -> FindDigit(matchText)(line.Substring(1))

let rec FirstDigit(line: string) : int =
  FindDigit(matchText)(line)

let LastDigit(line: string) : int =
  let reverseLine = new string(line.ToCharArray() |> Array.rev)
  let matchTextReverse = 
    matchText |> Seq.map(fun (text,value) -> (text |> Reverse, value) )
  FindDigit(matchTextReverse)(reverseLine)

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
  // 53389

printfn "The answer is %d" ans
