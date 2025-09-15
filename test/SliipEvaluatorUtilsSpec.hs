module SliipEvaluatorUtilsSpec (spec) where

import Sliip.Evaluator.Utils (isDefine)
import Sliip.Parser (Programs, SExpression (SExpr), Value (Builtin, Reference, SExprV, StringLiteral), parse)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec.Error (ParseError)

spec :: Spec
spec = do
  describe "isDefine" $ do
    it "judges whether the sexpr uses \"define\" function or not" $ do
      let defineAST :: SExpression
          defineAST = SExpr [Builtin "define", Builtin "main", StringLiteral "\"hoge\""]

          result :: Bool
          result = isDefine defineAST

      result `shouldBe` True
