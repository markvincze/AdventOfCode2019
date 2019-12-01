open System
open System.IO

let masses = File.ReadAllLines "FSharp/01-rocket-input.txt"
             |> Array.map Int32.Parse

let result1 = masses
                |> Array.sumBy (fun m -> m / 3 - 2)

let rec totalFuel total remaining  =
    match (max (remaining / 3 - 2) 0) with
    | 0 -> total
    | fuel -> totalFuel (total + fuel) fuel 

let result2 = masses
              |> Array.sumBy (totalFuel 0)
