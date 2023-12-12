module IntHelper

let TryParseInt (str: string) : int option = 
  match str.Trim() |> System.Int32.TryParse with
  | true, out -> Some out
  | false, _ -> None

let GetValue(x: Option<int>) : int = x.Value

let ParseInt = TryParseInt >> GetValue
