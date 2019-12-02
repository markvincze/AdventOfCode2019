module Main where

import Text.Printf
import Rocket01

main :: IO ()
main = do
    input <- readFile "Rocket01-Input.txt"
    let (rocket1, rocket2) = rocket01 input
    printf "Day 1. Solution 1: %d, Solution 2: %d" rocket1 rocket2
