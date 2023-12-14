
let calculateDistance(timeHold: int, timeRace : int) : int = 
  (timeHold * timeRace) - (timeHold * timeHold)

// printfn "`%d` should be 10" (calculateDistance(2, 7))
// printfn "`%d` should be 6" (calculateDistance(6, 7))

let calculateNumberOfWays (raceTime: int, targetDistance: int) : int =
  let minHold = 
    Seq.initInfinite(id)
    |> Seq.map(fun timeHold -> timeHold, calculateDistance(timeHold, raceTime))
    |> Seq.filter(fun (_, distance) -> distance > targetDistance)
    |> Seq.head
    |> fst

  let maxHold = raceTime - minHold

  let numberOfWays = maxHold - minHold + 1
  numberOfWays

let raceTime = 30
let targetDistance = 200

(*  Time:      7  15   30
    Distance:  9  40  200 *)

let races = [
    (7, 9);
    (15,40);
    (30,200)
  ]


let puzzleRaces = [
    (62,553);
    (64,1010);
    (91,1473);
    (90,1074)
  ]


// races
puzzleRaces
|> Seq.map(calculateNumberOfWays)
|> Seq.reduce (*)

// calculateNumberOfWays(raceTime, targetDistance)
