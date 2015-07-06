module Bound where

import Expression
import Free
import Data.Set

bound :: Expression -> (Expression, Int)
bound e = bound' (e, toList $ expressionToSet e, 0)

bound' :: (Expression, [String], Int) -> (Expression, Int)
bound' (e, [], n) = (e, n)
bound' (e, s : cs, n) = bound' (Lambda s e, cs, n + 1)

unbound :: (Expression, Int) -> Expression
unbound (Lambda x a, 0)	=	Lambda x a
unbound (Lambda x a, n)	=	unbound (a, n - 1)
unbound (e, _)			=	e
