{
module Reader where

import Data.Char
import Expression
}


%name parseLambda
%tokentype { Token }
%error { parseError }


%token
		slash	{ TokenSlash }
		'.'		{ TokenDot }
		'('		{ TokenLB }
		')'		{ TokenRB }
		' '		{ TokenSpace }
		var		{ TokenVar $$ }
%%


Expression		: Expression ' '			{ $1 }
				| Lambda					{ $1 }

Lambda			: slash var '.' Lambda		{ Lambda $2 $4 }
				| Apply						{ $1 }

Right			: Atomic					{ $1 }
				| slash var '.' Lambda		{ Lambda $2 $4 }

Apply			: Apply ' ' Right			{ Apply $1 $3 }
				| Atomic					{ $1 }

Atomic			: var						{ Var $1 }
				| '(' Lambda ')'			{ $2 }


{
data Token = Nil
		   | TokenSlash
		   | TokenDot
		   | TokenLB
		   | TokenRB
		   | TokenSpace
		   | TokenVar String
		   deriving Show

lexer :: String -> [Token]
lexer = lexing Nil

lexing _ []			= []
lexing p ('\\':cs)	= TokenSlash	: lexing TokenSlash cs
lexing p ('.':cs)	= TokenDot		: lexing TokenDot cs
lexing p ('(':cs)	= TokenLB		: lexing TokenLB cs
lexing p (')':cs)	= TokenRB		: lexing TokenRB cs
lexing p (c:cs)
	| isSpace c = case (span isSpace cs) of
				  (_, []) 	-> []
				  (_, _)	-> case p of
				  		   TokenRB		-> TokenSpace : lexing TokenSpace cs
				  		   TokenVar c'	-> TokenSpace : lexing TokenSpace cs
				  		   otherwise		-> lexing p cs
	| isAlpha c = lexVar p (c:cs)

lexVar :: Token -> String -> [Token]
lexVar p cs = let (var, rest) = span isVarLetter cs in
			TokenVar var : lexing (TokenVar var) rest

isVarLetter :: Char -> Bool
isVarLetter c = isAlpha c || isDigit c || c == '\''

parseError :: [Token] -> a
parseError _ = error "Parse error"

readLambda = parseLambda . lexer
}
