module Expression where

data Expression = Var String
                | Lambda String Expression
                | Apply Expression Expression

instance Show Expression where
    show (Var x)       = x
    show (Lambda x e)  = "(\\" ++ x ++ "." ++ show e ++ ")"
    show (Apply e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
