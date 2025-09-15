module Sliip.Evaluator.Utils (isDefine) where

import Sliip.Parser (Programs, SExpression (SExpr), Value (Builtin))

isDefine :: SExpression -> Bool
isDefine (SExpr (Builtin "define" : _)) = True
isDefine _ = False

-- hasLambda :: SExpression -> Bool
