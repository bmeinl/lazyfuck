module Main where
import Prelude hiding (lex)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P (parse)

data Expr = Op Char | Loop [Expr] deriving (Show)

lex :: String -> String
lex [] = []
lex (x:xs)
    | x `elem` "<>+-,.[]" = x : lex xs
    | otherwise = lex xs

parse = P.parse expr "" . lex

expr :: Parser [Expr]
expr = many (op <|> loop)

loop :: Parser Expr
loop = between (char '[') (char ']') expr >>= (return . Loop)

op :: Parser Expr
op = noneOf "[]" >>= (return . Op)