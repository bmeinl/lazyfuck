module Main where
import Prelude hiding (lex)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P (parse)
import Data.Char (chr)

data Expr = Op Char | Loop [Expr] deriving (Show)

lex :: String -> String
lex [] = []
lex (x:xs)
    | x `elem` "<>+-,.[]" = x : lex xs
    | otherwise = lex xs

parse s = case (P.parse expr "" . lex $ s) of
          Left _ -> error "parse error."
          Right a -> a

expr :: Parser [Expr]
expr = many (op <|> loop)

loop :: Parser Expr
loop = between (char '[') (char ']') expr >>= (return . Loop)

op :: Parser Expr
op = noneOf "[]" >>= (return . Op)

run :: [Expr] -> IO ()
run = run' (replicate 10 0)

run' :: [Int] -> [Expr] -> IO ()
run' buf [] = return ()
run' buf@(b:bs) (x:xs) = case x of
                           Op '>' -> run' (cycleRight buf) xs
                           Op '<' -> run' (cycleLeft buf) xs
                           Op '+'-> run' (b + 1 : bs) xs
                           Op '-' -> run' (b - 1 : bs) xs
                           Op '.' -> putChar (chr b) >> run' buf xs
                           Op ',' -> getLine >>= \n -> return ()
                           Loop (x:xs) -> undefined

cycleLeft (x:xs) = reverse (x : reverse xs)
cycleRight xs = last xs : init xs