module Main where

import Data.List.Split
import Expression
import Reader
import Substitution
import Writer

parseSubstitution :: String -> (Expression, String, Expression)
parseSubstitution s		=	let (a : la : _) = splitOn "[" s in
								let (x : lx : _) = splitOn ":=" la in
									let (b : _) = splitOn "]" lx in
										(readLambda a, readVar x, readLambda b)

main = do
	input <- readFile "task3.in"
	let (b, x, a)	= 	parseSubstitution input
	let output		=	case substitute' a x b of
						Nothing	->	"Нет свободы для подстановки для переменной " ++ x
						Just c	->	writeLambda c
	writeFile "task3.out" $ output ++ "\n"
