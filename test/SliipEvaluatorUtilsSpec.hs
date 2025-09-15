module SliipEvaluatorUtilsSpec (spec) where

import Sliip.Evaluator.Utils (isDefine, isMain)
import Sliip.Parser (Programs, SExpression (SExpr), Value (Builtin, Reference, SExprV, StringLiteral), parse)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec.Error (ParseError)

spec :: Spec
spec = do
  describe "isDefine" $ do
    it "judges whether the sexpr uses \"define\" function or not" $ do
      let defineAST :: SExpression
          defineAST = SExpr [Builtin "define", Builtin "main", StringLiteral "hoge"]

          result :: Bool
          result = isDefine defineAST

      result `shouldBe` True

      let notDefineAST :: SExpression
          notDefineAST = SExpr [Reference "foobar", Builtin "main", StringLiteral "fuga"]

          result :: Bool
          result = isDefine notDefineAST

      result `shouldBe` False

  describe "isMain" $ do
    it "judges whether the sexpr uses \"main\" reference or not" $ do
      let defineAST :: SExpression
          defineAST = SExpr [Builtin "define", Builtin "main", StringLiteral "hoge"]

          result :: Bool
          result = isMain defineAST

      result `shouldBe` True

      let notDefineAST :: SExpression
          notDefineAST = SExpr [Builtin "define", Reference "foobar", StringLiteral "fuga"]

          result :: Bool
          result = isMain notDefineAST

      result `shouldBe` False
