module Bruijn where

import Data.Map.Strict as Map
import Expression

data BExpression    = BVar Int
                    | BLambda BExpression
                    | BApply BExpression BExpression

instance Show BExpression where
    show (BVar n)       = show n
    show (BLambda e)    = "(\\." ++ show e ++ ")"
    show (BApply e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

alp = "abcdefghijklmnopqrstuvw"
digits = "0123456789"

randomName :: Int -> String
randomName a = let arg = 127 * a in
    let r23 = arg `mod` 23 in
        let r10 = arg `mod` 10 in
            let q230 = arg `div` 230 in
                alp !! r23 : digits !! r10 : iterate ('\'':) "" !! q230

bruijnize :: (Expression, Int) -> (BExpression, [String])
bruijnize = bruijnize' 0 empty

bruijnize' :: Int -> Map String Int -> (Expression, Int) -> (BExpression, [String])
bruijnize' n map (Var x, _)       = case Map.lookup x map of
    Nothing -> (BVar (n + 1), [])
    Just n0 -> (BVar (n - n0), [])
bruijnize' n map (Lambda x a, n0) = case n0 of
    0 -> (BLambda $ fst $ bruijnize' (n + 1) (insert x n map) (a, 0), [])
    _ -> let (e, l) = bruijnize' (n + 1) (insert x n map) (a, n - 1) in
        (BLambda e, x : l)
bruijnize' n map (Apply a b, _)   = (BApply (fst $ bruijnize' n map (a, 0)) (fst $ bruijnize' n map (b, 0)), [])

deBruijnize :: (BExpression, [String]) -> (Expression, Int)
deBruijnize (e, l) = (deBruijnize' 0 (e, l), length l)

deBruijnize' :: Int -> (BExpression, [String]) -> Expression
deBruijnize' n (BVar x, list)     = if n - x < length list
    then Var $ list !! (n - x)
    else Var $ randomName (n - x)
deBruijnize' n (BLambda a, list)  = if n < length list
    then Lambda (list !! n) $ deBruijnize' (n + 1) (a, list)
    else Lambda (randomName n) $ deBruijnize' (n + 1) (a, list)
deBruijnize' n (BApply a b, list) = Apply (deBruijnize' n (a, list)) (deBruijnize' n (b, list))

add :: Int -> Int -> BExpression -> BExpression
add n m (BVar x)     = if x > n
    then BVar (x + m)
    else BVar x
add n m (BLambda a)  = BLambda $ add (n + 1) m a
add n m (BApply a b) = BApply (add n m a) (add n m b)
