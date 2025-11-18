module SliipEvaluatorSpec (spec) where

import Sliip.Evaluator (eval)
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

    it "can defines lambda and string in script" $ do
      program <- readFile "test/data/displayHoge.lisp"
      out <- capture_ (eval program)
      out `shouldBe` "hoge\n"

    it "supports function calls with parameters" $ do
      let program =
            unlines
              [ "(define greet (lambda (name) (write-line name)))",
                "(define main (lambda () (greet \"World\")))"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "World\n"

    it "ignores type annotations" $ do
      let program =
            unlines
              [ "(define typed (lambda ((x String)) (write-line x)))",
                "(define main (lambda () (typed \"Test\")))"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "Test\n"
