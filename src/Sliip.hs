module Sliip where


import Text.Parsec (Parsec, (<|>), many)
import qualified Text.Parsec.Token as TT
import qualified Text.Parsec.Language as Lang

type Parser a = Parsec String () a

newtype SExpression =
  SExpr [Value]
  deriving (Show)

data Value =
  StringLiteral String
  | Reference String
  | SubExpr SExpression
           deriving (Show)

lexer :: TT.TokenParser ()
lexer = TT.makeTokenParser Lang.haskellStyle

symbol :: String -> Parser String
symbol = TT.symbol Lang.haskell

sexpr :: Parser SExpression
sexpr = do
  _ <- symbol "("
  xs <- many value
  _ <- symbol ")"
  return (SExpr xs)

identifier :: Parser String
identifier = TT.identifier lexer

value :: Parser Value
value =
  (SubExpr <$> sexpr)
  <|> (StringLiteral <$> stringLiteral)
  <|> (Reference <$> identifier)

stringLiteral :: Parser String
stringLiteral = TT.stringLiteral Lang.haskell
