module Unify where

import Equation
import Term

unify :: [Equation] -> Maybe [Equation]
unify eqs = unify' (eqs, [])

unify' :: ([Equation], [Equation]) -> Maybe [Equation]
unify' ([], rs)                                  = Just rs
unify' (Eq l r : eqs, rs)
    | l == r                                     = unify' (eqs, rs)
unify' (Eq (Function fn fargs) (Function gn gargs) : eqs, rs)
    | fn /= gn || length fargs /= length gargs   = Nothing
    | otherwise                                  = unify' (map mkE (zip fargs gargs) ++ eqs, rs)
  where
    mkE = \(l, r) -> Eq l r
unify' (Eq l@(Function _ _) r@(Val _) : eqs, rs) = unify' (Eq r l : eqs, rs)
unify' (Eq (Val x) r@(Function _ _) : eqs, _)
    | x `elem` vars r                            = Nothing
  where
    vars (Val x)           = [x]
    vars (Function _ args) = concatMap vars args
unify' (Eq (Val x) r : eqs, rs)                  = unify' (sIE x r eqs, Eq (Val x) r : sIE x r rs)
  where
    sIE x t = map (\(Eq l r) -> Eq (substitute x t l) (substitute x t r))
    substitute x t t'@(Val y)          = if x == y then t else t'
    substitute x t (Function fn fargs) = Function fn $ map (substitute x t) fargs
