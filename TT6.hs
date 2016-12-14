module TT6 where

import Data.Map.Strict           (Map, empty, insert, lookup)
import Expression
import Equation
import Prelude         hiding    (lookup)
import Reader
import Term
import Unify

getMainType :: Maybe [Equation] -> String
getMainType Nothing                        = "Лямбда-выражение не имеет типа."
getMainType (Just [])                      = "t0"
getMainType (Just (Eq (Val "t0") t : eqs)) = showType t
getMainType (Just (_ : eqs))               = getMainType $ Just eqs

getTypeVar :: Int -> Term
getTypeVar n = Val $ "t" ++ show n

induceTS :: Expression -> [Equation]
induceTS = snd . induceTS' 0 empty

induceTS' :: Int -> Map String Int -> Expression -> ((Term, Int), [Equation])
induceTS' n m (Var x)       = case lookup x m of
    Nothing -> ((getTypeVar n, n + 1), [])
    Just n0 -> ((getTypeVar n0, n), [])
induceTS' n m (Lambda x e)  = ((getTypeVar n, n'),
    Eq (getTypeVar n) (Function "->" [getTypeVar $ n + 1, t']) : eq')
  where
    ((t', n'), eq') = induceTS' (n + 2) (insert x (n + 1) m) e
induceTS' n m (Apply e1 e2) =  ((getTypeVar n, n2),
    Eq t1 (Function "->" [t2, getTypeVar n]) : eq1 ++ eq2)
  where
    ((t1, n1), eq1) = induceTS' (n + 1) m e1
    ((t2, n2), eq2) = induceTS' n1 m e2

showType :: Term -> String
showType (Val n) = n
showType (Function f [t1, t2])
    | f == "->"  = "(" ++ showType t1 ++ "->" ++ showType t2 ++ ")"
showType t       = "Wrong type term: " ++ show t

main = do
    input <- readFile "task6.in"
    let e = readLambda input
    writeFile "task6.out" $ getMainType $ unify $ induceTS e
