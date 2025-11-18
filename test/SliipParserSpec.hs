{-# LANGUAGE LambdaCase #-}

module SliipParserSpec (spec) where

import Data.List (null)
import Sliip.Parser (Expr (..), Param (..), parse)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses a simple symbol" $ do
      let input = "hoge"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ESymbol "hoge"]
                        )

    it "parses a simple application" $ do
      let input = "(hoge fuga)"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [EApp (ESymbol "hoge") [ESymbol "fuga"]]
                        )

    it "parses nested application" $ do
      let input = "(hoge (fuga piyo))"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [EApp (ESymbol "hoge") [EApp (ESymbol "fuga") [ESymbol "piyo"]]]
                        )

    it "ignores a comment line" $ do
      let input = "; hogehoge"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> null v
                        )

    it "parses an expr with comment lines" $ do
      let input = "; hogehoge\nhoge\n; fuga"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ESymbol "hoge"]
                        )

    it "parses symbols containing slash" $ do
      let input = "hoge/hoge"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ESymbol "hoge/hoge"]
                        )

    it "parses symbols containing plus" $ do
      let input = "hoge+hoge"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ESymbol "hoge+hoge"]
                        )

    it "parses string literals" $ do
      let input = "\"hoge\""
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [EString "hoge"]
                        )

    it "parses string literals with escape sequences" $ do
      let input = "\"hoge\\thoge\""
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [EString "hoge\thoge"]
                        )

    it "parses multiple expressions" $ do
      let input = "hoge\nfuga"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ESymbol "hoge", ESymbol "fuga"]
                        )

    it "parses define with lambda" $ do
      let input = "(define main (lambda () (write-line \"test\")))"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [EDefine "main" (ELambda params body)] -> 
                              params == [] && length body == 1
                            _ -> False
                        )

    it "parses numbers" $ do
      let input = "42"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ENumber 42]
                        )

    it "parses booleans" $ do
      let input = "true false"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [EBool True, EBool False]
                        )

    it "parses type ascriptions" $ do
      let input = "(as 42 Int)"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [EAscription (ENumber 42) _] -> True
                            _ -> False
                        )

    it "parses lambda with typed parameters" $ do
      let input = "(lambda ((x Int) (y String)) x)"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [ELambda params _] -> length params == 2
                            _ -> False
                        )
