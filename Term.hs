module Term where

import Data.List           (intercalate)

data Term
        = Val String
        | Function String [Term]
          deriving Eq

instance Show Term where
    show (Val x) = x
    show (Function f args) = f ++ "(" ++ intercalate "," (map show args) ++ ")"
