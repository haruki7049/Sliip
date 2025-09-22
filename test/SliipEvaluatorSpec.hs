module SliipEvaluatorSpec (spec) where

import Sliip.Evaluator (Environment, EvaluationError (NoMainFound), Executable, Statement (WriteLine), buildEnv, evalSExpr, getMain, eval)
import Sliip.Parser (Atom (Builtin, Reference, SExprV, StringLiteral), SExpression (SExpr))
import Test.Hspec (Spec, describe, it, shouldBe)
import System.IO.Silently (capture_)

spec :: Spec
spec = do
  describe "eval" $ do
    it "can reads define after main definition" $ do
      let program = unlines
            [ "(define main"
            , "  (lambda ()"
            , "    (write-line fooString)))"
            , ""
            , "(define fooString \"foo\")"
            ]
      out <- capture_ (eval program)
      out `shouldBe` "foo\n"

    it "can reads define before main definition" $ do
      let program = unlines
            [ "(define fooString \"foo\")"
            , ""
            , "(define main"
            , "  (lambda ()"
            , "    (write-line fooString)))"
            ]
      out <- capture_ (eval program)
      out `shouldBe` "foo\n"

    it "prints result of main's write-line" $ do
      let program = unlines
            [ "(define main"
            , "  (lambda ()"
            , "    (write-line \"hello\")))"
            ]
      out <- capture_ (eval program)
      out `shouldBe` "hello\n"

  describe "evalSExpr" $ do
    it "evaluates SExpression without any statement in lambda, to Executable" $ do
      let programs :: [SExpression]
          programs =
            [ SExpr
                [ Builtin "define",
                  Builtin "main",
                  SExprV
                    ( SExpr
                        [ Builtin "lambda",
                          SExprV (SExpr []),
                          SExprV (SExpr [])
                        ]
                    )
                ]
            ]

          env :: Environment
          env = buildEnv programs

          mainSExpr :: Maybe SExpression
          mainSExpr = getMain programs

          result :: Either EvaluationError Executable
          result = case mainSExpr of
            Nothing -> Left NoMainFound
            Just sexpr -> evalSExpr env sexpr

      result `shouldBe` Right []

    it "evaluates SExpression with write-line in lambda, to Executable" $ do
      let programs :: [SExpression]
          programs =
            [ SExpr
                [ Builtin "define",
                  Builtin "main",
                  SExprV
                    ( SExpr
                        [ Builtin "lambda",
                          SExprV (SExpr []),
                          SExprV
                            ( SExpr
                                [ Builtin "write-line",
                                  StringLiteral "hoge"
                                ]
                            )
                        ]
                    )
                ]
            ]

          env :: Environment
          env = buildEnv programs

          mainSExpr :: Maybe SExpression
          mainSExpr = getMain programs

          result :: Either EvaluationError Executable
          result = case mainSExpr of
            Nothing -> Left NoMainFound
            Just sexpr -> evalSExpr env sexpr

      result `shouldBe` Right [WriteLine "hoge"]

    it "evaluates SExpression with a Reference in lambda, to Executable" $ do
      let programs :: [SExpression]
          programs =
            [ SExpr [Builtin "define", Reference "hoge", StringLiteral "hoge"],
              SExpr
                [ Builtin "define",
                  Builtin "main",
                  SExprV
                    ( SExpr
                        [ Builtin "lambda",
                          SExprV (SExpr []),
                          SExprV
                            ( SExpr
                                [ Builtin "write-line",
                                  StringLiteral "hoge"
                                ]
                            )
                        ]
                    )
                ]
            ]

          env :: Environment
          env = buildEnv programs

          mainSExpr :: Maybe SExpression
          mainSExpr = getMain programs

          result :: Either EvaluationError Executable
          result = case mainSExpr of
            Nothing -> Left NoMainFound
            Just sexpr -> evalSExpr env sexpr

      result `shouldBe` Right [WriteLine "hoge"]
