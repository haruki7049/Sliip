module Main where

import qualified Sliip (programs)
import Text.Parsec (parse)

main :: IO ()
main = do
  putStrLn "Enter your SExpression..."

  s <- getLine
  case parse Sliip.programs "stdin" s of
    Left err -> print err
    Right x -> print x
