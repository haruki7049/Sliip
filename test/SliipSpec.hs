module SliipSpec (spec) where

import Sliip (SExpression (SExpr), Value (Reference, SExprV, StringLiteral), programs)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Text.Parsec (parse)

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses a simple SExpression" $ do
      let input = "( hoge )"
          result = parse Sliip.programs "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> False
                            Right v -> v == Just (SExpr [Reference "hoge"])
                        )

    it "parses a nested SExpression" $ do
      let input = "( hoge ( fuga piyo ) )"
          result = parse Sliip.programs "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> False
                            Right v -> v == Just (SExpr [Reference "hoge", SExprV (SExpr [Reference "fuga", Reference "piyo"])])
                        )

    it "don't parses a symbol" $ do
      let input = "( 'hoge )"
          result = parse Sliip.programs "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> True
                            Right _ -> False
                        )

    it "ignores a comment line" $ do
      let input = "; hogehoge"
          result = parse Sliip.programs "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> False
                            Right v -> v == Nothing
                        )

    it "parses a sexpr with some comment line" $ do
      let input = "; hogehoge\n( hoge )\n; fuga"
          result = parse Sliip.programs "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> False
                            Right v -> v == Just (SExpr [Reference "hoge"])
                        )

    it "parses a word in sexpr, which contains slash" $ do
      let input = "( hoge/hoge )"
          result = parse Sliip.programs "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> False
                            Right v -> v == Just (SExpr [Reference "hoge/hoge"])
                        )

    it "parses a word in sexpr, which contains plus" $ do
      let input = "( hoge+hoge )"
          result = parse Sliip.programs "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> False
                            Right v -> v == Just (SExpr [Reference "hoge+hoge"])
                        )

    it "parses a stringLiteral in sexpr" $ do
      let input = "( \"hoge\" )"
          result = parse Sliip.programs "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> False
                            Right v -> v == Just (SExpr [StringLiteral "hoge"])
                        )

    it "parses a stringLiteral in sexpr, which contains escape sequence" $ do
      let input = "( \"hoge\\thoge\" )"
          result = parse Sliip.programs "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> False
                            Right v -> v == Just (SExpr [StringLiteral "hoge\thoge"])
                        )
