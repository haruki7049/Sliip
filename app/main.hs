module Main where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as TT
import qualified Text.Parsec.Language as Lang

type Parser a = Parsec String () a

natural :: Parser Integer
natural = TT.natural Lang.haskell

main :: IO ()
main = do
  putStrLn "Enter your number..."

  s <- getLine
  case parse natural "stdin" s of
    Left err -> print err
    Right x -> print x
