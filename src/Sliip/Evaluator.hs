-- |
-- Module      : Sliip.Evaluator
-- Description : Evaluator for the Sliip Lisp interpreter
-- Maintainer  : haruki7049
--
-- This module implements the evaluation engine for Sliip, a Lisp interpreter.
-- It provides functionality to:
--
-- * Parse and evaluate Lisp programs
-- * Manage lexical environments with variable bindings
-- * Execute lambda functions with closures
-- * Handle thunks for lazy evaluation of definitions
-- * Generate executable statements (currently limited to 'write-line')
--
-- The evaluator uses a map-based environment for variable storage and supports
-- basic Lisp features including lambda functions, variable references, and
-- function application.
module Sliip.Evaluator (eval) where

import Control.Monad (forM_)
import Data.List (find)
import Data.Map (Map, empty, insert, lookup)
import Sliip.Evaluator.Utils (isDefine, isMain)
import Data.Void (Void)
import Sliip.Parser (Expr (..), Param (..), Programs, parse)
import Text.Megaparsec (ParseErrorBundle)
import Prelude hiding (lookup)

-- | An executable program represented as a list of statements.
type Executable = [Statement]

-- | A statement that can be executed as part of a program.
--
-- Currently only supports 'WriteLine' for outputting strings.
newtype Statement = WriteLine String
  deriving (Show, Eq)

-- | Errors that can occur during evaluation.
data EvaluationError
  = -- | No 'main' function was found in the program
    NoMainFound
  | -- | The expression is not a valid form
    InvalidForm Expr
  | -- | The expression cannot be evaluated
    InvalidExpr Expr
  | -- | Reference to an undefined built-in function
    UnknownBuiltin String
  | -- | Reference to an undefined variable
    UnknownReference String
  | -- | Type mismatch during evaluation
    TypeMismatch String
  deriving (Show, Eq)

-- | Runtime values in the Sliip interpreter.
data Value
  = -- | String value
    VString String
  | -- | Integer value
    VNumber Integer
  | -- | Floating-point value
    VFloat Double
  | -- | Boolean value
    VBool Bool
  | -- | Lambda function with parameters, body, and closure
    VLambda [String] [Expr] Environment
  | -- | Built-in function
    VBuiltin String (Value -> Either EvaluationError Value)
  | -- | Unevaluated expression with its environment (for lazy evaluation)
    VThunk Expr Environment

instance Show Value where
  show (VString s) = "VString " ++ show s
  show (VNumber n) = "VNumber " ++ show n
  show (VFloat d) = "VFloat " ++ show d
  show (VBool b) = "VBool " ++ show b
  show (VLambda args _ _) = "VLambda " ++ show args ++ " <body> <env>"
  show (VBuiltin name _) = "VBuiltin " ++ show name
  show (VThunk _ _) = "VThunk <expr> <env>"

-- | An environment mapping variable names to their values.
type Environment = Map String Value

-- | Evaluate a Lisp program from a string.
--
-- Parses the input string and evaluates it, printing any errors or executing
-- the resulting program.
eval :: String -> IO ()
eval script = do
  let parsed_result :: Either (ParseErrorBundle String Void) Programs
      parsed_result = parse script
  case parsed_result of
    Left err -> print err
    Right x -> evalPrograms x

-- | Evaluate a parsed program.
--
-- Builds the environment from top-level definitions, finds the 'main' function,
-- and executes it.
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

-- | Build the initial environment from a program.
--
-- Collects all top-level 'define' forms and creates thunks for them.
-- This allows for mutual recursion between definitions.
buildEnv :: Programs -> Environment
buildEnv progs = env
  where
    env = foldl insertDef empty progs
    insertDef envAcc (EDefine name expr) = insert name (VThunk expr env) envAcc
    insertDef envAcc _ = envAcc

-- | Look up a variable in the environment.
--
-- If the variable is bound to a thunk, evaluates it first.
lookupVar :: String -> Environment -> Either EvaluationError Value
lookupVar name env =
  case lookup name env of
    Just (VThunk expr closureEnv) -> evalExpr closureEnv expr
    Just v -> Right v
    Nothing -> Left (UnknownReference name)

-- | Evaluate the 'main' function.
--
-- The main function must be a lambda (either directly or after evaluation).
-- Returns an executable program consisting of statements to execute.
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

-- | Evaluate an expression to a value.
--
-- Handles all basic expression types:
--
-- * Literals (numbers, strings, booleans)
-- * Variable references (symbols)
-- * Lambda expressions
-- * Type ascriptions
-- * Function applications
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

-- | Apply a function to arguments.
--
-- Supports both lambda functions and built-in functions.
-- Creates a new environment with the parameters bound to the arguments.
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

-- | Evaluate a lambda body to an executable.
--
-- Converts the expressions in the lambda body to executable statements.
evalLambdaBody :: Environment -> [String] -> [Expr] -> Either EvaluationError Executable
evalLambdaBody env _ = evalExprsToExecutable env

-- | Convert a list of expressions to an executable program.
--
-- Each expression is converted to zero or more statements.
evalExprsToExecutable :: Environment -> [Expr] -> Either EvaluationError Executable
evalExprsToExecutable env exprs = do
  results <- mapM (evalExprToStatement env) exprs
  return (concat results)

-- | Convert an expression to executable statements.
--
-- Recognizes special forms that produce side effects:
--
-- * @(write-line string)@ - produces a 'WriteLine' statement
-- * @(begin ...)@ - evaluates a sequence of expressions
-- * Function applications that may have side effects
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

-- | Find the 'main' function definition in a program.
--
-- Returns 'Just' the main definition if found, 'Nothing' otherwise.
getMain :: Programs -> Maybe Expr
getMain = find (\expr -> isDefine expr && isMain expr)

-- | Execute a compiled program.
--
-- Runs each statement in sequence.
runExecutable :: Executable -> IO ()
runExecutable exe = forM_ exe runStatement

-- | Execute a single statement.
runStatement :: Statement -> IO ()
runStatement (WriteLine str) = putStrLn str
