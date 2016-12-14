{
module Reader5 where

import Data.Char
import Data.List
import Term
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    var             { TokenVar $$ }
    ','             { TokenComma }
    '('             { TokenLpar }
    ')'             { TokenRpar }

%%

Term    : var '(' args ')'              { Function $1 $3 }
        | var                           { Val $1 }

args    : {- empty -}                   { [] }
        | Term                          { [$1] }
        | Term ',' args                 { $1:$3 }

{
{- Error function -}

parseError :: [Token] -> a
parseError _ = error "Parse error"

{- Lexems -}

data Token
        = TokenVar String
        | TokenComma
        | TokenLpar
        | TokenRpar
        deriving Show

{- Instances & helpers -}

instance Read Term where
    readsPrec _ s = [(parse $ lexer s, "")]

{- Lexer -}

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = TokenVar var : lexer rest
        where var = c : ds ++ as
              (ds, ccs) = span isDigit cs
              (as, rest) = span (== '\'') ccs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenLpar : lexer cs
lexer (')':cs) = TokenRpar : lexer cs
}
