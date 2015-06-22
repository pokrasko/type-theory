module Writer where

import Expression

writeLambda :: Expression -> String
writeLambda (Var x)		   = x
writeLambda (Lambda x a)   = "(\\" ++ x ++ "." ++ writeLambda a ++ ")"
writeLambda (Apply a b)	   = "(" ++ writeLambda a ++ " " ++ writeLambda b ++ ")"
