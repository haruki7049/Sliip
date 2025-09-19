module Sliip.Evaluator.Utils (isDefine, isMain, hasLambda) where

import Sliip.Parser (Atom (Builtin, SExprV), SExpression (SExpr))

isDefine :: SExpression -> Bool
isDefine (SExpr (Builtin "define" : _)) = True
isDefine _ = False

isMain :: SExpression -> Bool
isMain (SExpr (_ : Builtin "main" : _)) = True
isMain _ = False

hasLambda :: SExpression -> Bool
hasLambda (SExpr (_ : _ : SExprV (SExpr (Builtin "lambda" : _)) : _)) = True
hasLambda _ = False
