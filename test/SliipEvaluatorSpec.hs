module SliipEvaluatorSpec (spec) where

import Sliip.Parser (SExpression (SExpr), Value (Builtin, SExprV, StringLiteral))
import Sliip.Evaluator (Executable, EvaluationError, evalSExpr, Statement (WriteLine))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "evalSExpr" $ do
    it "evaluates SExpression without any statement in lambda, to Executable" $ do
      let result :: Either EvaluationError Executable
          result = evalSExpr (SExpr [Builtin "define"
                                    , Builtin "main"
                                    , SExprV (SExpr [Builtin "lambda"
                                                    , SExprV (SExpr [])
                                                    , SExprV (SExpr [])])])

      result `shouldBe` (Right [])

    it "evaluates SExpression with write-line in lambda, to Executable" $ do
      let result :: Either EvaluationError Executable
          result = evalSExpr (SExpr [Builtin "define"
                                    , Builtin "main"
                                    , SExprV (SExpr [Builtin "lambda"
                                                    , SExprV (SExpr [])
                                                    , SExprV (SExpr [Builtin "write-line"
                                                                    , StringLiteral "hoge"])])])

      result `shouldBe` (Right [WriteLine "hoge"])
