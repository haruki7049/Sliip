{-|
Module      : Sliip.Evaluator.Utils
Description : Utility functions for the Sliip evaluator
Maintainer  : haruki7049

This module provides utility functions for working with Sliip expressions,
primarily for checking expression types and structure.
-}
module Sliip.Evaluator.Utils (isDefine, isMain, hasLambda) where

import Sliip.Parser (Expr (..))

-- | Check if an expression is a 'define' form.
--
-- Returns 'True' if the expression is an 'EDefine', 'False' otherwise.
isDefine :: Expr -> Bool
isDefine (EDefine _ _) = True
isDefine _ = False

-- | Check if an expression is a 'main' function definition.
--
-- Returns 'True' if the expression is @(define main ...)@, 'False' otherwise.
isMain :: Expr -> Bool
isMain (EDefine "main" _) = True
isMain _ = False

-- | Check if an expression is a 'define' form with a lambda body.
--
-- Returns 'True' if the expression is @(define name (lambda ...))@, 'False' otherwise.
hasLambda :: Expr -> Bool
hasLambda (EDefine _ (ELambda _ _)) = True
hasLambda _ = False
