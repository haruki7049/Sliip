module SliipEvaluatorUtilsSpec (spec) where

import Sliip.Evaluator.Utils (hasLambda, isDefine, isMain)
import Sliip.Parser (Expr (..), Param (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "isDefine" $ do
    it "judges whether the expr uses \"define\" or not" $ do
      let defineAST = EDefine "main" (EString "hoge")
          result = isDefine defineAST
      result `shouldBe` True

      let notDefineAST = EApp (ESymbol "foobar") [ESymbol "main", EString "fuga"]
          result2 = isDefine notDefineAST
      result2 `shouldBe` False

  describe "isMain" $ do
    it "judges whether the expr defines \"main\" or not" $ do
      let defineAST = EDefine "main" (EString "hoge")
          result = isMain defineAST
      result `shouldBe` True

      let notDefineAST = EDefine "foobar" (EString "fuga")
          result2 = isMain notDefineAST
      result2 `shouldBe` False

  describe "hasLambda" $ do
    it "judges whether the value is a \"lambda\" define or not" $ do
      let lambdaAST = EDefine "main" (ELambda [] [EApp (ESymbol "println") [EString "hello"]])
          result = hasLambda lambdaAST
      result `shouldBe` True

      let notLambdaAST = EDefine "foobar" (EString "fuga")
          result2 = hasLambda notLambdaAST
      result2 `shouldBe` False
