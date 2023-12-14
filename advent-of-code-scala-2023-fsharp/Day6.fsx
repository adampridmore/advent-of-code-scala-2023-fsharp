
let calculateDistance(timeHold, timeRace) = 
  (timeHold * timeRace) - (timeHold * timeHold)

let calculateNumberOfWays (raceTime, targetDistance) =
  let minHold = 
    Seq.initInfinite(id)
    |> Seq.map(int64)
    |> Seq.map(fun timeHold -> timeHold, calculateDistance(timeHold, raceTime))
    |> Seq.filter(fun (_, distance) -> distance > targetDistance)
    |> Seq.head
    |> fst

  let maxHold = raceTime - minHold

  let numberOfWays = maxHold - minHold + 1L
  numberOfWays

let exampleRacesPartI = [
    (7, 9);
    (15,40);
    (30,200)
  ]

let puzzleRacesPartI = [
    (62,553);
    (64,1010);
    (91,1473);
    (90,1074)
  ]
// Answer: 840336

let exampleRacesPartII = [(71530L,940200L)]
let puzzleRacesPartII = [(62649190L,553101014731074L)]

// races
let ans = 
  puzzleRacesPartII
  |> Seq.map(calculateNumberOfWays)
  |> Seq.reduce (*)

printfn "%d" ans

// calculateNumberOfWays(raceTime, targetDistance)
