module Sliip.Evaluator
  ( eval,
    Executable,
    Statement (..),
    EvaluationError (..),
    evalSExpr,
  )
where

import Control.Monad (forM_)
import Data.List (find)
import Sliip.Evaluator.Utils (isDefine, isMain)
import Sliip.Parser (Programs, SExpression (SExpr), Value (Builtin, SExprV, StringLiteral), parse)
import Text.Parsec.Error (ParseError)

type Executable = [Statement]

newtype Statement = WriteLine String
  deriving (Show, Eq)

data EvaluationError
  = NoMainFound
  | InvalidForm SExpression
  | UnknownBuitin String
  deriving (Show, Eq)

eval :: String -> IO ()
eval script = do
  let parsed_result :: Either ParseError Programs
      parsed_result = parse script

  case parsed_result of
    Left err -> print err
    Right x -> evalPrograms x

evalPrograms :: Programs -> IO ()
evalPrograms p = do
  let mainSExpr :: Maybe SExpression
      mainSExpr = getMain p

      executable :: Either EvaluationError Executable
      executable = maybe (Left NoMainFound) evalSExpr mainSExpr

  case executable of
    Left err -> print err
    Right exe -> runExecutable exe

evalSExpr :: SExpression -> Either EvaluationError Executable
evalSExpr (SExpr [Builtin "define", Builtin "main", SExprV sexpr]) = evalSExpr sexpr
evalSExpr (SExpr [Builtin "lambda", SExprV args, SExprV sexpr]) = evalLambda args sexpr
evalSExpr sexpr = Left (InvalidForm sexpr)

evalLambda :: SExpression -> SExpression -> Either EvaluationError Executable
evalLambda _ (SExpr [Builtin "write-line", StringLiteral stringLiteral]) = Right [WriteLine stringLiteral]
evalLambda _ (SExpr []) = Right []
evalLambda _ sexpr = Left (InvalidForm sexpr)

getMain :: Programs -> Maybe SExpression
getMain = find (\expr -> isDefine expr && isMain expr)

runExecutable :: Executable -> IO ()
runExecutable exe = do
  forM_ exe $ \statement -> do
    runStatement statement

runStatement :: Statement -> IO ()
runStatement (WriteLine str) = putStrLn str
