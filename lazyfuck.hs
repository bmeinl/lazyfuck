module Main where
import Prelude hiding (lex)

lex :: String -> [Char]
lex [] = []
lex (x:xs)
    | x `elem` "<>+-,.[]" = x : lex xs
    | otherwise = lex xs

