module Sliip.Evaluator
  ( eval,
    Executable,
    Statement (..),
    EvaluationError (..),
    Environment,
    getMain,
    evalSExpr,
    buildEnv,
  )
where

import Control.Monad (forM_)
import Data.List (find)
import Data.Map (Map, empty, insert, lookup)
import Sliip.Evaluator.Utils (isDefine, isMain)
import Sliip.Parser (Atom (Builtin, Reference, SExprV, StringLiteral), Programs, SExpression (SExpr), parse)
import Text.Parsec.Error (ParseError)
import Prelude hiding (lookup)

type Executable = [Statement]

newtype Statement = WriteLine String
  deriving (Show, Eq)

data EvaluationError
  = NoMainFound
  | InvalidForm SExpression
  | UnknownBuitin String
  | UnknownReference String
  deriving (Show, Eq)

data Value
  = VString String
  | VLambda [String] SExpression Environment
  | VBuiltin String (Value -> Either EvaluationError Value)

instance Show Value where
  show (VString s) = "VString " ++ show s
  show (VLambda args sexpr env) = "VLambda " ++ show args ++ show sexpr ++ show env
  show (VBuiltin name _) = "VBuiltin " ++ show name

type Environment = Map String Value

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

      env :: Environment
      env = buildEnv p

      executable :: Either EvaluationError Executable
      executable = case mainSExpr of
        Nothing -> Left NoMainFound
        Just sexpr -> evalSExpr env sexpr

  case executable of
    Left err -> print err
    Right exe -> runExecutable exe

buildEnv :: Programs -> Environment
buildEnv = foldl insertDef empty
  where
    insertDef env (SExpr [Builtin "define", Reference name, StringLiteral s]) =
      insert name (VString s) env
    insertDef env _ = env

lookupVar :: String -> Environment -> Either EvaluationError Value
lookupVar name env =
  case lookup name env of
    Just v -> Right v
    Nothing -> Left (UnknownReference name)

evalSExpr :: Environment -> SExpression -> Either EvaluationError Executable
evalSExpr env (SExpr [Builtin "define", Builtin "main", SExprV sexpr]) = evalSExpr env sexpr
evalSExpr env (SExpr [Builtin "lambda", SExprV args, SExprV sexpr]) = evalLambda env args sexpr
evalSExpr _ sexpr = Left (InvalidForm sexpr)

evalLambda :: Environment -> SExpression -> SExpression -> Either EvaluationError Executable
evalLambda env _ (SExpr [Builtin "write-line", StringLiteral stringLiteral]) = Right [WriteLine stringLiteral]
evalLambda env _ (SExpr [Builtin "write-line", Reference ref]) =
  case lookupVar ref env of
    Right (VString s) -> Right [WriteLine s]
    Right _ -> Left (InvalidForm (SExpr [Builtin "write-line", Reference ref]))
    Left err -> Left err
evalLambda _ _ (SExpr []) = Right []
evalLambda _ _ sexpr = Left (InvalidForm sexpr)

getMain :: Programs -> Maybe SExpression
getMain = find (\expr -> isDefine expr && isMain expr)

-- Run Executable

runExecutable :: Executable -> IO ()
runExecutable exe = do
  forM_ exe $ \statement -> do
    runStatement statement

runStatement :: Statement -> IO ()
runStatement (WriteLine str) = putStrLn str
