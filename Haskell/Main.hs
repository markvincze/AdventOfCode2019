module Main where

import Text.Printf
import Rocket01
import Program02

main :: IO ()
main = do
    input01 <- readFile "Rocket01-Input.txt"
    let (rocket1, rocket2) = rocket01 input01
    printf "Day 1. Solution 1: %d, Solution 2: %d\n" rocket1 rocket2

    input02 <- readFile "Program02-Input.txt"
    let (program1, program2) = program02 input02
    printf "Day 2. Solution 1: %d, Solution 2: %d\n" program1 program2
