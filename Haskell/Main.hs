module Main where

import Text.Printf
import Rocket01

main :: IO ()
main = do
    (rocket1, rocket2) <- rocket01
    printf "Solution 1: %d, Solution 2: %d" rocket1 rocket2
