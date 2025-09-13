module SliipSpec (spec) where

import Sliip (sexpr, SExpression(SExpr), Value(Reference, SubExpr))
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Text.Parsec (parse)

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses a simple SExpression" $ do
      let input = "( hoge )"
          result = parse Sliip.sexpr "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> False
                            Right v -> v == (SExpr [Reference "hoge"])
                        )

    it "parses a nested SExpression" $ do
      let input = "( hoge ( fuga piyo ) )"
          result = parse Sliip.sexpr "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> False
                            Right v -> v == (SExpr [Reference "hoge", SubExpr (SExpr [Reference "fuga", Reference "piyo"])])
                        )

    it "parses a symbol" $ do
      let input = "( 'hoge )"
          result = parse Sliip.sexpr "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> False
                            Right v -> v == (SExpr [Reference "\'hoge"])
                        )
