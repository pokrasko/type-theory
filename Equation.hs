module Equation where

import Term

data Equation = Eq Term Term
                deriving Eq

instance Show Equation where
    show (Eq l r) = show l ++ "=" ++ show r
