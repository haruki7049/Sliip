module Sliip where

import Text.Parsec (Parsec, many, (<|>))
import Text.Parsec.Char (letter, alphaNum, oneOf, char)
import Text.Parsec.Token (LanguageDef, commentStart, commentEnd, commentLine, nestedComments, identStart, identLetter, opStart, opLetter, reservedOpNames, reservedNames, caseSensitive)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token as TT

sliipStyle :: LanguageDef st
sliipStyle = emptyDef
             { commentStart = ""
             , commentEnd = ""
             , commentLine = ";"
             , nestedComments = True
             , identStart = letter <|> char '\''
             , identLetter = alphaNum <|> oneOf "_:!#$%&*+./<=>?@\\^|-~'"
             , opStart = opLetter sliipStyle
             , opLetter = oneOf ""
             , reservedOpNames = []
             , reservedNames = []
             , caseSensitive = True
             }

type Parser a = Parsec String () a

newtype SExpression
  = SExpr [Value]
  deriving (Show)

data Value
  = StringLiteral String
  | Reference String
  | SubExpr SExpression
  deriving (Show)

lexer :: TT.TokenParser ()
lexer = TT.makeTokenParser sliipStyle

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
