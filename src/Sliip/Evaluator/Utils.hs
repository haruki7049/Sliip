module Sliip.Evaluator.Utils (isDefine, isMain, hasLambda) where

import Sliip.Parser (Expr (..))

isDefine :: Expr -> Bool
isDefine (EDefine _ _) = True
isDefine _ = False

isMain :: Expr -> Bool
isMain (EDefine "main" _) = True
isMain _ = False

hasLambda :: Expr -> Bool
hasLambda (EDefine _ (ELambda _ _)) = True
hasLambda _ = False
