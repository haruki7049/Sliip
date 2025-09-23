module Sliip.Evaluator (eval) where

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
  | InvalidAtom Atom
  | UnknownBuitin String
  | UnknownReference String
  deriving (Show, Eq)

data Value
  = VString String
  | VLambda [String] SExpression Environment
  | VBuiltin String (Value -> Either EvaluationError Value)
  | VThunk Atom Environment

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
    insertDef :: Environment -> SExpression -> Environment
    insertDef env (SExpr [Builtin "define", Reference name, expr]) =
      insert name (VThunk expr env) env
    insertDef env _ = env

lookupVar :: String -> Environment -> Either EvaluationError Value
lookupVar name env =
  case lookup name env of
    Just (VThunk sexpr closureEnv) -> do
      val <- evalValue closureEnv sexpr
      Right val
    Just v -> Right v
    Nothing -> Left (UnknownReference name)

evalSExpr :: Environment -> SExpression -> Either EvaluationError Executable
evalSExpr env (SExpr [Builtin "define", Builtin "main", SExprV sexpr]) = evalSExpr env sexpr
evalSExpr env (SExpr [Builtin "lambda", SExprV args, SExprV body]) =
  case buildLambda args body env of
    Right (VLambda a b e) -> evalLambda e a b
    Left err -> Left err
evalSExpr env (SExpr (fnExpr : argExprs)) =
  do
    fnVal <- evalValue env fnExpr
    argVals <- mapM (evalValue env) argExprs
    apply fnVal argVals
evalSExpr _ sexpr = Left (InvalidForm sexpr)

buildLambda :: SExpression -> SExpression -> Environment -> Either EvaluationError Value
buildLambda (SExpr args) body env =
  let argNames = [name | Reference name <- args]
   in Right (VLambda argNames body env)
buildLambda _ _ _ = Left (InvalidForm (SExpr []))

evalValue :: Environment -> Atom -> Either EvaluationError Value
evalValue _ (StringLiteral s) = Right (VString s)
evalValue env (Reference ref) = lookupVar ref env
evalValue env (SExprV (SExpr [Builtin "lambda", SExprV args, SExprV body])) =
  buildLambda args body env
evalValue _ atom = Left (InvalidAtom atom)

evalLambda :: Environment -> [String] -> SExpression -> Either EvaluationError Executable
evalLambda env _ (SExpr [Builtin "write-line", StringLiteral stringLiteral]) = Right [WriteLine stringLiteral]
evalLambda env _ (SExpr [Builtin "write-line", Reference ref]) =
  case lookupVar ref env of
    Right (VString s) -> Right [WriteLine s]
    Right _ -> Left (InvalidForm (SExpr [Builtin "write-line", Reference ref]))
    Left err -> Left err
evalLambda _ _ (SExpr []) = Right []
evalLambda _ _ sexpr = Left (InvalidForm sexpr)

-- Apply lambda
apply :: Value -> [Value] -> Either EvaluationError Executable
apply (VLambda params body closureEnv) args
  | length params == length args =
      let localEnv = foldl (\e (p, a) -> insert p a e) closureEnv (zip params args)
       in evalLambda localEnv params body
  | otherwise = Left (InvalidForm body)

getMain :: Programs -> Maybe SExpression
getMain = find (\expr -> isDefine expr && isMain expr)

-- Run Executable

runExecutable :: Executable -> IO ()
runExecutable exe = do
  forM_ exe $ \statement -> do
    runStatement statement

runStatement :: Statement -> IO ()
runStatement (WriteLine str) = putStrLn str
