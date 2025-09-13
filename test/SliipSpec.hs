module SliipSpec (spec) where

import Sliip (SExpression (SExpr), Value (Reference, SExprV), programs)
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

    it "parses a symbol" $ do
      let input = "( 'hoge )"
          result = parse Sliip.programs "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> False
                            Right v -> v == Just (SExpr [Reference "\'hoge"])
                        )

    it "ignores a comment line" $ do
      let input = "; hogehoge"
          result = parse Sliip.programs "" input
      result
        `shouldSatisfy` ( \r -> case r of
                            Left _ -> False
                            Right v -> v == Nothing
                        )
