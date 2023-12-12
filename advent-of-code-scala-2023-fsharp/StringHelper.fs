module StringHelper

let Reverse(text : string) : string = 
  new string(text.ToCharArray() |> Array.rev)
