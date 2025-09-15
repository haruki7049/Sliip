module Sliip.Evaluator
  ( eval,
  )
where

import Sliip.Parser (Programs, parse)
import Text.Parsec.Error (ParseError)

eval :: String -> IO ()
eval script = do
  let parsed_result :: Either ParseError Programs
      parsed_result = parse script

  case parsed_result of
    Left err -> print err
    Right x -> evalPrograms x

evalPrograms :: Programs -> IO ()
evalPrograms p = do
  print p
