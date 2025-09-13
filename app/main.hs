module Main where

import qualified Sliip (sexpr)
import Text.Parsec (parse)

main :: IO ()
main = do
  putStrLn "Enter your number..."

  s <- getLine
  case parse Sliip.sexpr "stdin" s of
    Left err -> print err
    Right x -> print x
