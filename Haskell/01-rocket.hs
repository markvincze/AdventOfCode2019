-- let result1 = masses
--                 |> Array.sumBy (fun m -> m / 3 - 2)

-- let rec totalFuel total remaining  =
--     match (max (remaining / 3 - 2) 0) with
--     | 0 -> total
--     | fuel -> totalFuel (total + fuel) fuel 

-- let result2 = masses
--               |> Array.sumBy (totalFuel 0)


import Text.Printf

fuelForMass m = m / 3 - 2

main = do
    content <- readFile "01-rocket-input.txt"
    let contentLines = lines content
    let masses = map (\m -> read m :: Integer) contentLines
    let fuels = map fuelForMass masses
    let result1 = sum fuels

    printf "Solution 1: %d" result1


