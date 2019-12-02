module Program02 where

import Data.List
import Data.List.Split

modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth _ _ [] = []
modifyNth n modify (h:t)
    | n == 0 = modify h:t
    | otherwise = h:modifyNth (n - 1) modify t

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal = modifyNth n (const newVal)

processCode :: Int -> [Int] -> [Int]
processCode index numbers =
    case numbers!!index of
        99 -> numbers
        o -> processCode (index + 4) newNumbers where
             op1 = numbers!!(numbers!!(index+1))
             op2 = numbers!!(numbers!!(index+2))
             result = case o of
                          1 -> op1 + op2
                          2 -> op1 * op2
             newNumbers = replaceNth (numbers!!(index+3)) result numbers

searchValue = 19690720

adjustedNumbers :: Int -> Int -> [Int] -> [Int]
adjustedNumbers n v numbers = replaceNth 1 n . replaceNth 2 v $ numbers

program02 :: String -> (Int, Int)
program02 input = (result1, result2) where
    numbers = map (\s -> read s :: Int) . splitOn "," $ input

    numbers1 = adjustedNumbers 12 2 numbers
    result1 = (processCode 0 numbers1)!!0

    combinations = [(n, v) | n <- [0..99], v <- [0..99]]

    findOutput n v = (processCode 0 . adjustedNumbers n v $ numbers)!!0
    outputs = map (\(n, v) -> (n, v, findOutput n v)) combinations
    solution = find (\(_, _, o) -> o == searchValue) outputs
    result2 = case solution of
                  Nothing -> 0
                  Just (n, v, _) -> 100 * n + v
