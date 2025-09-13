module Main where

import Text.Parsec (parse)
import qualified Sliip (sexpr)

main :: IO ()
main = do
  putStrLn "Enter your number..."

  s <- getLine
  case parse Sliip.sexpr "stdin" s of
    Left err -> print err
    Right x -> print x
