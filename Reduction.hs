module Reduction where

import Bruijn
import Substitution

reduce :: BExpression -> BExpression
reduce (BVar x)					=	BVar x
reduce (BLambda a)				=	BLambda $ reduce a
reduce (BApply (BLambda a) b)	=	reduce $ substitute b a
reduce (BApply a b)				=	case reduce a of
									BLambda c	->	reduce $ substitute b c
									c			->	BApply c $ reduce b
