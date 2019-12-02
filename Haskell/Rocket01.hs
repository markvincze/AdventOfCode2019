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
    masses = map (\m -> read m :: Integer) . lines $ input
    result1 = sum . map fuelForMass $ masses

    result2 = sum . map (\m -> fuelForMassRec 0 m) $ masses