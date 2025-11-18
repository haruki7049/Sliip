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
      program <- readFile "test/data/display-hoge.sliip"
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

    it "supports nested function calls" $ do
      let program =
            unlines
              [ "(define inner (lambda (x) (write-line x)))",
                "(define outer (lambda (y) (inner y)))",
                "(define main (lambda () (outer \"Nested\")))"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "Nested\n"

    it "supports multiple variable references" $ do
      let program =
            unlines
              [ "(define a \"Hello\")",
                "(define b a)",
                "(define c b)",
                "(define main (lambda () (write-line c)))"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "Hello\n"

    it "supports lambda with multiple parameters" $ do
      let program =
            unlines
              [ "(define show (lambda (x y) (write-line x)))",
                "(define main (lambda () (show \"First\" \"Second\")))"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "First\n"

    it "supports type ascription on variables" $ do
      let program =
            unlines
              [ "(define msg (as \"TypedVar\" String))",
                "(define main (lambda () (write-line msg)))"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "TypedVar\n"

    it "supports empty lambda body returning no output" $ do
      let program =
            unlines
              [ "(define noop (lambda () (write-line \"\")))",
                "(define main (lambda () (noop)))"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "\n"

    it "supports chained function calls" $ do
      let program =
            unlines
              [ "(define f1 (lambda (x) x))",
                "(define f2 (lambda (x) x))",
                "(define main (lambda () (write-line (f1 (f2 \"Chain\")))))"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "Chain\n"

    it "handles variables defined in different order" $ do
      let program =
            unlines
              [ "(define c b)",
                "(define b a)",
                "(define a \"Order\")",
                "(define main (lambda () (write-line c)))"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "Order\n"

    it "supports functions that return functions (higher-order)" $ do
      let program =
            unlines
              [ "(define maker (lambda (x) (lambda () (write-line x))))",
                "(define printer (maker \"HigherOrder\"))",
                "(define main (lambda () (printer)))"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "HigherOrder\n"

    it "supports lambda with typed and untyped parameters mixed" $ do
      let program =
            unlines
              [ "(define mixed (lambda (x (y String)) (write-line y)))",
                "(define main (lambda () (mixed \"ignore\" \"Mixed\")))"
              ]
      out <- capture_ (eval program)
      out `shouldBe` "Mixed\n"
