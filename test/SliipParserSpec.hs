{-# LANGUAGE LambdaCase #-}

module SliipParserSpec (spec) where

import Data.List (null)
import Sliip.Parser (SExpression (SExpr), Atom (Builtin, Reference, SExprV, StringLiteral), parse, programs)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses a simple SExpression" $ do
      let input = "( hoge )"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [SExpr [Reference "hoge"]]
                        )

    it "parses a nested SExpression" $ do
      let input = "( hoge ( fuga piyo ) )"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [SExpr [Reference "hoge", SExprV (SExpr [Reference "fuga", Reference "piyo"])]]
                        )

    it "don't parses a symbol" $ do
      let input = "( 'hoge )"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> True
                            Right _ -> False
                        )

    it "ignores a comment line" $ do
      let input = "; hogehoge"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> null v
                        )

    it "parses a sexpr with some comment line" $ do
      let input = "; hogehoge\n( hoge )\n; fuga"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [SExpr [Reference "hoge"]]
                        )

    it "parses a word in sexpr, which contains slash" $ do
      let input = "( hoge/hoge )"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [SExpr [Reference "hoge/hoge"]]
                        )

    it "parses a word in sexpr, which contains plus" $ do
      let input = "( hoge+hoge )"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [SExpr [Reference "hoge+hoge"]]
                        )

    it "parses a stringLiteral in sexpr" $ do
      let input = "( \"hoge\" )"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [SExpr [StringLiteral "hoge"]]
                        )

    it "parses a stringLiteral in sexpr, which contains escape sequence" $ do
      let input = "( \"hoge\\thoge\" )"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v -> v == [SExpr [StringLiteral "hoge\thoge"]]
                        )

    it "parses multipul SExpressions" $ do
      let input = "( hoge )\n( fuga )"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v ->
                              v
                                == [ SExpr [Reference "hoge"],
                                     SExpr [Reference "fuga"]
                                   ]
                        )

    it "parses some builtin words" $ do
      let input = "( define main ( lambda () ()) )"
          result = parse input
      result
        `shouldSatisfy` ( \case
                            Left _ -> False
                            Right v ->
                              v
                                == [SExpr [Builtin "define", Builtin "main", SExprV (SExpr [Builtin "lambda", SExprV (SExpr []), SExprV (SExpr [])])]]
                        )
