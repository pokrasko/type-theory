module Main where

import Bound
import Bruijn
import Reader
import Reduction
import Writer

unmained :: String -> String
unmained s	=	let (e, f) = bruijnize $ bound $ readLambda s in
				writeLambda $ unbound $ deBruijnize (reduce e, f)

main =	do
		input <- readFile "task4.in"
		let (e, f) = bruijnize $ bound $ readLambda input
		writeFile "task4.out" $ writeLambda $ unbound $ deBruijnize (reduce e, f)
