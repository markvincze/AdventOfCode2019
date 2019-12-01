module Rocket01 where

fuelForMass m = max ((div m 3) - 2) 0

fuelForMassRec total remaining =
    case fuelForMass remaining of
        0 -> total
        fuel -> fuelForMassRec (total + fuel) fuel

rocket01 = do
    content <- readFile "Rocket01-Input.txt"
    let contentLines = lines content
    let masses = map (\m -> read m :: Integer) contentLines
    let fuels1 = map fuelForMass masses
    let result1 = sum fuels1

    let fuels2 = map (\m -> fuelForMassRec 0 m) masses
    let result2 = sum fuels2

    return (result1, result2)
