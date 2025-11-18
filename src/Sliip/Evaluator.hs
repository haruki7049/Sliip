module Sliip.Evaluator (eval) where

import Control.Monad (forM_)
import Data.List (find)
import Data.Map (Map, empty, insert, lookup)
import Sliip.Evaluator.Utils (isDefine, isMain)
import Sliip.Parser (Expr (..), Param (..), Programs, parse)
import Text.Parsec.Error (ParseError)
import Prelude hiding (lookup)

type Executable = [Statement]

newtype Statement = WriteLine String
  deriving (Show, Eq)

data EvaluationError
  = NoMainFound
  | InvalidForm Expr
  | InvalidExpr Expr
  | UnknownBuiltin String
  | UnknownReference String
  | TypeMismatch String
  deriving (Show, Eq)

data Value
  = VString String
  | VNumber Integer
  | VFloat Double
  | VBool Bool
  | VLambda [String] [Expr] Environment
  | VBuiltin String (Value -> Either EvaluationError Value)
  | VThunk Expr Environment

instance Show Value where
  show (VString s) = "VString " ++ show s
  show (VNumber n) = "VNumber " ++ show n
  show (VFloat d) = "VFloat " ++ show d
  show (VBool b) = "VBool " ++ show b
  show (VLambda args _ _) = "VLambda " ++ show args ++ " <body> <env>"
  show (VBuiltin name _) = "VBuiltin " ++ show name
  show (VThunk _ _) = "VThunk <expr> <env>"

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
  let mainExpr = getMain p
      env = buildEnv p
      executable = case mainExpr of
        Nothing -> Left NoMainFound
        Just expr -> evalMain env expr
  case executable of
    Left err -> print err
    Right exe -> runExecutable exe

buildEnv :: Programs -> Environment
buildEnv progs = env
  where
    env = foldl insertDef empty progs
    insertDef envAcc (EDefine name expr) = insert name (VThunk expr env) envAcc
    insertDef envAcc _ = envAcc

lookupVar :: String -> Environment -> Either EvaluationError Value
lookupVar name env =
  case lookup name env of
    Just (VThunk expr closureEnv) -> evalExpr closureEnv expr
    Just v -> Right v
    Nothing -> Left (UnknownReference name)

evalMain :: Environment -> Expr -> Either EvaluationError Executable
evalMain env (EDefine "main" body) =
  case body of
    ELambda params bodyExprs -> do
      let paramNames = map (\(Param name _) -> name) params
      evalLambdaBody env paramNames bodyExprs
    _ -> do
      val <- evalExpr env body
      case val of
        VLambda params bodyExprs closureEnv ->
          evalLambdaBody closureEnv params bodyExprs
        _ -> Left (InvalidForm (EDefine "main" body))
evalMain _ expr = Left (InvalidForm expr)

evalExpr :: Environment -> Expr -> Either EvaluationError Value
evalExpr _ (ENumber n) = Right (VNumber n)
evalExpr _ (EFloat d) = Right (VFloat d)
evalExpr _ (EString s) = Right (VString s)
evalExpr _ (EBool b) = Right (VBool b)
evalExpr env (ESymbol name) = lookupVar name env
evalExpr env (ELambda params body) = do
  let paramNames = map (\(Param name _) -> name) params
  Right (VLambda paramNames body env)
evalExpr env (EAscription expr _) = evalExpr env expr
evalExpr env (EApp func args) = do
  funcVal <- evalExpr env func
  argVals <- mapM (evalExpr env) args
  applyFunc funcVal argVals
evalExpr _ expr = Left (InvalidExpr expr)

applyFunc :: Value -> [Value] -> Either EvaluationError Value
applyFunc (VLambda params body closureEnv) args
  | length params == length args = do
      let localEnv = foldl (\e (p, a) -> insert p a e) closureEnv (zip params args)
      case body of
        [expr] -> evalExpr localEnv expr
        _ -> Left (InvalidExpr (EBegin body))
  | otherwise = Left (TypeMismatch "Wrong number of arguments")
applyFunc (VBuiltin _ f) [arg] = f arg
applyFunc _ _ = Left (TypeMismatch "Cannot apply non-function")

evalLambdaBody :: Environment -> [String] -> [Expr] -> Either EvaluationError Executable
evalLambdaBody env _ = evalExprsToExecutable env

evalExprsToExecutable :: Environment -> [Expr] -> Either EvaluationError Executable
evalExprsToExecutable env exprs = do
  results <- mapM (evalExprToStatement env) exprs
  return (concat results)

evalExprToStatement :: Environment -> Expr -> Either EvaluationError Executable
evalExprToStatement env (EApp (ESymbol "write-line") [arg]) = do
  val <- evalExpr env arg
  case val of
    VString s -> Right [WriteLine s]
    _ -> Left (TypeMismatch "write-line expects string")
evalExprToStatement env (EBegin exprs) = evalExprsToExecutable env exprs
evalExprToStatement env (EApp func args) = do
  -- Evaluate function application for side effects
  funcVal <- evalExpr env func
  argVals <- mapM (evalExpr env) args
  case funcVal of
    VLambda params body closureEnv
      | length params == length argVals -> do
          let localEnv = foldl (\e (p, a) -> insert p a e) closureEnv (zip params argVals)
          evalExprsToExecutable localEnv body
      | otherwise -> Left (TypeMismatch "Wrong number of arguments")
    _ -> Right [] -- Non-lambda application, no side effects in our simple model
evalExprToStatement _ _ = Right []

getMain :: Programs -> Maybe Expr
getMain = find (\expr -> isDefine expr && isMain expr)

runExecutable :: Executable -> IO ()
runExecutable exe = forM_ exe runStatement

runStatement :: Statement -> IO ()
runStatement (WriteLine str) = putStrLn str
