module TT5 where

import Control.Monad             (liftM)
import Data.List                 (intercalate)
import Data.List.Split           (splitOn)
import Equation
import Reader5
import Unify

showEquations :: Maybe [Equation] -> String
showEquations Nothing    = "No solutions"
showEquations (Just eqs) = intercalate "\n" $ map show eqs

main = do
    lines' <- liftM lines (readFile "task5.in")
    let eqstr = map (splitOn "=") lines'
    let eqs = map (\[a, b] -> Eq (read a) (read b)) eqstr
    writeFile "task5.out" $ showEquations $ unify eqs
