module Substitution where

import Data.Set as Set
import Expression
import Free

substitute' :: Expression -> String -> Expression -> Maybe Expression
substitute' = substituteWV' empty

substituteWV' :: Set String -> Expression -> String -> Expression -> Maybe Expression
substituteWV' bb a y (Var x)			=	case x /= y || member x bb of
											True	->	Just $ Var x
											False	->	case Set.null $ intersection bb $ expressionToSet a of
														True	->	Just a
														False	->	Nothing
substituteWV' bb a y (Lambda x b)		=	case substituteWV' (insert x bb) a y b of
											Nothing	->	Nothing
											Just sb	->	Just $ Lambda x sb
substituteWV' bb a y (Apply b c)		=	case substituteWV' bb a y b of
											Nothing	->	Nothing
											Just sb	->	case substituteWV' bb a y c of
														Nothing	->	Nothing
														Just sc	->	Just $ Apply sb sc
