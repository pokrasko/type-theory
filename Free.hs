module Free where

import Expression
import Data.Set

foldVariables :: String -> String -> String
foldVariables [] b = b
foldVariables a b  = a ++ "\n" ++ b

freeVariables :: Expression -> String
freeVariables = Prelude.foldl foldVariables [] . toList . expressionToSet

expressionToSet :: Expression -> Set String
expressionToSet (Var x)			= 	singleton x
expressionToSet (Lambda x a)	= 	if member x (expressionToSet a)
									then delete x (expressionToSet a)
									else expressionToSet a
expressionToSet (Apply a b)		= 	union (expressionToSet a) (expressionToSet b)
