module Main where

import Expression
import Reader

main = do
    input <- readFile "task1.in"
    writeFile "task1.out" $ show $ readLambda input
