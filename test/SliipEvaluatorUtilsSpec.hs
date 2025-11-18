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

  describe "isDefine with different expr types" $ do
    it "returns False for symbol" $ do
      let expr = ESymbol "test"
          result = isDefine expr
      result `shouldBe` False

    it "returns False for number" $ do
      let expr = ENumber 42
          result = isDefine expr
      result `shouldBe` False

    it "returns False for application" $ do
      let expr = EApp (ESymbol "func") [ESymbol "arg"]
          result = isDefine expr
      result `shouldBe` False

    it "returns True for any define" $ do
      let expr = EDefine "anything" (ENumber 123)
          result = isDefine expr
      result `shouldBe` True

  describe "isMain with different names" $ do
    it "returns False for 'Main' with capital M" $ do
      let expr = EDefine "Main" (EString "test")
          result = isMain expr
      result `shouldBe` False

    it "returns False for 'MAIN' all caps" $ do
      let expr = EDefine "MAIN" (EString "test")
          result = isMain expr
      result `shouldBe` False

    it "returns False for 'main2'" $ do
      let expr = EDefine "main2" (EString "test")
          result = isMain expr
      result `shouldBe` False

    it "returns True for 'main' only" $ do
      let expr = EDefine "main" (ELambda [] [])
          result = isMain expr
      result `shouldBe` True

  describe "hasLambda with different body types" $ do
    it "returns False for define with string body" $ do
      let expr = EDefine "var" (EString "value")
          result = hasLambda expr
      result `shouldBe` False

    it "returns False for define with number body" $ do
      let expr = EDefine "num" (ENumber 42)
          result = hasLambda expr
      result `shouldBe` False

    it "returns False for define with application body" $ do
      let expr = EDefine "app" (EApp (ESymbol "f") [ENumber 1])
          result = hasLambda expr
      result `shouldBe` False

    it "returns True for define with lambda body (with params)" $ do
      let expr = EDefine "func" (ELambda [Param "x" Nothing] [ESymbol "x"])
          result = hasLambda expr
      result `shouldBe` True

    it "returns True for define with lambda body (with typed params)" $ do
      let expr = EDefine "typed" (ELambda [Param "x" (Just undefined)] [ENumber 1])
          result = hasLambda expr
      result `shouldBe` True

    it "returns False for non-define expressions" $ do
      let expr = ELambda [] [EString "test"]
          result = hasLambda expr
      result `shouldBe` False
