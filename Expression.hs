module Expression where
	data Expression = Var String
					| Lambda String Expression
					| Apply Expression Expression
					deriving Show
