module Reduction where

import Bruijn
import Expression
import Free
import Substitution

reduce :: BExpression -> BExpression
reduce (BVar x)               = BVar x
reduce (BLambda a)            = BLambda $ reduce a
reduce (BApply (BLambda a) b) = reduce $ substitute (reduce b) a
reduce (BApply a b)           = case reduce a of
                                BLambda c -> reduce $ substitute (reduce b) c
                                c         -> BApply c $ reduce b
