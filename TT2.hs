module Main where

import Expression
import Reader
import Free

main = do
    input <- readFile "task2.in"
    writeFile "task2.out" $ freeVariables $ readLambda input
