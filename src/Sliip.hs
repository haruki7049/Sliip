module Sliip where

import Text.Parsec (Parsec, many, (<|>))
import Text.Parsec.Char (alphaNum, char, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Language as Lang (haskell)
import Text.Parsec.Token (LanguageDef, TokenParser, caseSensitive, commentEnd, commentLine, commentStart, identLetter, identStart, makeTokenParser, nestedComments, opLetter, opStart, reservedNames, reservedOpNames)
import qualified Text.Parsec.Token as TT (identifier, stringLiteral, symbol)

sliipStyle :: LanguageDef st
sliipStyle =
  emptyDef
    { commentStart = "",
      commentEnd = "",
      commentLine = ";",
      nestedComments = True,
      identStart = letter <|> char '\'',
      identLetter = alphaNum <|> oneOf "_:!#$%&*+./<=>?@\\^|-~'",
      opStart = opLetter sliipStyle,
      opLetter = oneOf "",
      reservedOpNames = [],
      reservedNames = [],
      caseSensitive = True
    }

type Parser a = Parsec String () a

newtype SExpression
  = SExpr [Value]
  deriving (Show, Eq)

data Value
  = StringLiteral String
  | Reference String
  | SubExpr SExpression
  deriving (Show, Eq)

lexer :: TokenParser ()
lexer = makeTokenParser sliipStyle

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
