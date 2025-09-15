module Sliip.Evaluator.Utils (isDefine, isMain) where

import Sliip.Parser (SExpression (SExpr), Value (Builtin))

isDefine :: SExpression -> Bool
isDefine (SExpr (Builtin "define" : _)) = True
isDefine _ = False

isMain :: SExpression -> Bool
isMain (SExpr (_ : Builtin "main" : _)) = True
isMain _ = False

-- hasLambda :: SExpression -> Bool
