module Rocket01 where

fuelForMass :: Integer -> Integer
fuelForMass m = max ((div m 3) - 2) 0

fuelForMassRec :: Integer -> Integer -> Integer
fuelForMassRec total remaining =
    case fuelForMass remaining of
        0 -> total
        fuel -> fuelForMassRec (total + fuel) fuel

rocket01 :: String -> (Integer, Integer)
rocket01 input = (result1, result2) where
    contentLines = lines input
    masses = map (\m -> read m :: Integer) contentLines
    fuels1 = map fuelForMass masses
    result1 = sum fuels1

    fuels2 = map (\m -> fuelForMassRec 0 m) masses
    result2 = sum fuels2