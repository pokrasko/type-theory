module Main where

import Expression
import Reader
import Writer

main = do
	input <- readFile "task1.in"
	writeFile "task1.out" $ writeLambda $ readLambda input
