module Sliip.Parser
  ( programs,
    SExpression (..),
    Value (..),
    Programs,
  )
where

import Text.Parsec (Parsec, choice, eof, many, (<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (LanguageDef, TokenParser, caseSensitive, commentEnd, commentLine, commentStart, identLetter, identStart, makeTokenParser, nestedComments, opLetter, opStart, reservedNames, reservedOpNames)
import qualified Text.Parsec.Token as TT (identifier, reserved, stringLiteral, symbol, whiteSpace)

sliipStyle :: LanguageDef st
sliipStyle =
  emptyDef
    { commentStart = "",
      commentEnd = "",
      commentLine = ";",
      nestedComments = True,
      identStart = letter,
      identLetter = alphaNum <|> oneOf "_:!#$%&*+./<=>?@\\^|-~'",
      opStart = opLetter sliipStyle,
      opLetter = oneOf "",
      reservedOpNames = [],
      reservedNames = ["define", "lambda", "main"],
      caseSensitive = True
    }

type Parser a = Parsec String () a

newtype SExpression
  = SExpr [Value]
  deriving (Show, Eq)

data Value
  = StringLiteral String
  | Builtin String
  | Reference String
  | SExprV SExpression
  deriving (Show, Eq)

lexer :: TokenParser ()
lexer = makeTokenParser sliipStyle

symbol :: String -> Parser String
symbol = TT.symbol lexer

whitespace :: Parser ()
whitespace = TT.whiteSpace lexer

sexpr :: Parser SExpression
sexpr = do
  _ <- symbol "("
  xs <- many value
  _ <- symbol ")"
  return (SExpr xs)

identifier :: Parser String
identifier = TT.identifier lexer

builtin :: Parser String
builtin =
  choice (map (\kw -> TT.reserved lexer kw >> return kw) $ reservedNames sliipStyle)

value :: Parser Value
value =
  (SExprV <$> sexpr)
    <|> (Builtin <$> builtin)
    <|> (StringLiteral <$> stringLiteral)
    <|> (Reference <$> identifier)

stringLiteral :: Parser String
stringLiteral = TT.stringLiteral lexer

type Programs = [SExpression]

programs :: Parser Programs
programs = do
  whitespace
  expr <- many sexpr
  whitespace
  eof
  return expr
