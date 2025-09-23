module SliipEvaluatorSpec (spec) where

import Sliip.Evaluator (eval)
import Sliip.Parser (Atom (Builtin, Reference, SExprV, StringLiteral), SExpression (SExpr))
import System.IO.Silently (capture_)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "eval" $ do
    it "can reads define after main definition" $ do
      let program =
            unlines
              [ "(define main",
                "  (lambda ()",
                "    (write-line fooString)))",
                "",
                "(define fooString \"foo\")"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "foo\n"

    it "can reads define before main definition" $ do
      let program =
            unlines
              [ "(define fooString \"foo\")",
                "",
                "(define main",
                "  (lambda ()",
                "    (write-line fooString)))"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "foo\n"

    it "prints result of main's write-line" $ do
      let program =
            unlines
              [ "(define main",
                "  (lambda ()",
                "    (write-line \"hello\")))"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "hello\n"
