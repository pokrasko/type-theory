module Main where

import Bound
import Bruijn
import Reader
import Reduction

unmained :: String -> String
unmained s = let (e, f) = bruijnize $ bound $ readLambda s in
    show $ unbound $ deBruijnize (reduce e, f)

main = do
    input <- readFile "task4.in"
    let (e, f) = bruijnize $ bound $ readLambda input
    writeFile "task4.out" $ show $ unbound $ deBruijnize (reduce e, f)
