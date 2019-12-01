module Main where
-- let result1 = masses
--                 |> Array.sumBy (fun m -> m / 3 - 2)

-- let rec totalFuel total remaining  =
--     match (max (remaining / 3 - 2) 0) with
--     | 0 -> total
--     | fuel -> totalFuel (total + fuel) fuel

-- let result2 = masses
--               |> Array.sumBy (totalFuel 0)

import Text.Printf

fuelForMassSimple m = (div m 3) - 2

fuelForMass total remaining =
    case (div remaining 3) - 2 of
        0 -> total
        fuel -> fuelForMass (total + fuel) fuel

main = do
    content <- readFile "01-rocket-input.txt"
    let contentLines = lines content
    let masses = map (\m -> read m :: Integer) contentLines
    let fuels1 = map fuelForMassSimple masses
    let result1 = sum fuels1

    let fuels2 = map (\m -> fuelForMass 0 m) masses
    let result2 = sum fuels2
    -- let result2 = 1234 :: Integer


    printf "Solution 1: %d, Solution 2: %d" result1 result2

--main :: IO ()
--main = putStrLn "Hello, Haskell!"
