{-# LANGUAGE LambdaCase #-}

module SliipParserSpec (spec) where

import Data.List (null)
import Sliip.Parser (Expr (..), Param (..), parseSliip)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = do
  describe "parseSliip" $ do
    it "parses a simple symbol" $ do
      let input = "hoge"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ESymbol "hoge"]
                        )

    it "parses a simple application" $ do
      let input = "(hoge fuga)"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [EApp (ESymbol "hoge") [ESymbol "fuga"]]
                        )

    it "parses nested application" $ do
      let input = "(hoge (fuga piyo))"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [EApp (ESymbol "hoge") [EApp (ESymbol "fuga") [ESymbol "piyo"]]]
                        )

    it "ignores a comment line" $ do
      let input = "; hogehoge"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> null v
                        )

    it "parses an expr with comment lines" $ do
      let input = "; hogehoge\nhoge\n; fuga"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ESymbol "hoge"]
                        )

    it "parses symbols containing slash" $ do
      let input = "hoge/hoge"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ESymbol "hoge/hoge"]
                        )

    it "parses symbols containing plus" $ do
      let input = "hoge+hoge"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ESymbol "hoge+hoge"]
                        )

    it "parses string literals" $ do
      let input = "\"hoge\""
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [EString "hoge"]
                        )

    it "parses string literals with escape sequences" $ do
      let input = "\"hoge\\thoge\""
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [EString "hoge\thoge"]
                        )

    it "parses multiple expressions" $ do
      let input = "hoge\nfuga"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ESymbol "hoge", ESymbol "fuga"]
                        )

    it "parses define with lambda" $ do
      let input = "(define main (lambda () (write-line \"test\")))"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [EDefine "main" (ELambda params body)] ->
                              null params && length body == 1
                            _ -> False
                        )

    it "parses numbers" $ do
      let input = "42"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ENumber 42]
                        )

    it "parses booleans" $ do
      let input = "true false"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [EBool True, EBool False]
                        )

    it "parses type ascriptions" $ do
      let input = "(as 42 Int)"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [EAscription (ENumber 42) _] -> True
                            _ -> False
                        )

    it "parses lambda with typed parameters" $ do
      let input = "(lambda ((x Int) (y String)) x)"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [ELambda params _] -> length params == 2
                            _ -> False
                        )

    it "parses floating point numbers" $ do
      let input = "3.14"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [EFloat 3.14]
                        )

    it "parses if expressions" $ do
      let input = "(if true 1 0)"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [EIf (EBool True) (ENumber 1) (ENumber 0)] -> True
                            _ -> False
                        )

    it "parses begin expressions" $ do
      let input = "(begin (write-line \"a\") (write-line \"b\"))"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [EBegin _] -> True
                            _ -> False
                        )

    it "parses let expressions" $ do
      let input = "(let ((x 1) (y 2)) (+ x y))"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [ELet bindings _] -> length bindings == 2
                            _ -> False
                        )

    it "parses let* expressions" $ do
      let input = "(let* ((x 1) (y x)) y)"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [ELetStar _ _] -> True
                            _ -> False
                        )

    it "parses letrec expressions" $ do
      let input = "(letrec ((f (lambda (x) (f x)))) (f 1))"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [ELetRec _ _] -> True
                            _ -> False
                        )

    it "parses quote expressions" $ do
      let input = "(quote (a b c))"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [EQuote _] -> True
                            _ -> False
                        )

    it "parses def-type expressions" $ do
      let input = "(def-type Maybe (T) (Just T) (Nothing))"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [EDefType "Maybe" ["T"] _] -> True
                            _ -> False
                        )

    it "parses match expressions" $ do
      let input = "(match x ((Just y) y) (_ 0))"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [EMatch (ESymbol "x") clauses] -> length clauses == 2
                            _ -> False
                        )

    it "parses nested let in lambda" $ do
      let input = "(lambda (x) (let ((y x)) (write-line y)))"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [ELambda _ body] -> not (null body)
                            _ -> False
                        )

    it "parses multiple defines" $ do
      let input = "(define x 1)\n(define y 2)\n(define z 3)"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> length v == 3
                        )

    it "parses complex type expressions" $ do
      let input = "(as f (-> Int String Bool))"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [EAscription _ _] -> True
                            _ -> False
                        )

    it "parses symbols with special characters" $ do
      let input = "<=>"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right [ESymbol "<=>"] -> True
                            _ -> False
                        )

    it "parses block comments" $ do
      let input = "#| block comment |# hoge"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ESymbol "hoge"]
                        )

    it "parses nested block comments" $ do
      let input = "#| outer #| inner |# outer |# fuga"
          result = parseSliip input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [ESymbol "fuga"]
                        )
