module Sliip.Parser
  ( programs,
    SExpression (..),
    Atom (..),
    Programs,
    parse,
  )
where

import Text.Parsec (ParseError, Parsec, choice, eof, many, (<|>))
import qualified Text.Parsec (parse)
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
      reservedNames = ["define", "lambda", "main", "write-line"],
      caseSensitive = True
    }

type Parser a = Text.Parsec.Parsec String () a

newtype SExpression
  = SExpr [Atom]
  deriving (Show, Eq)

data Atom
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
  x <- many atom
  _ <- symbol ")"
  return (SExpr x)

identifier :: Parser String
identifier = TT.identifier lexer

builtin :: Parser String
builtin =
  choice (map (\kw -> TT.reserved lexer kw >> return kw) $ reservedNames sliipStyle)

atom :: Parser Atom
atom =
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

parse :: String -> Either Text.Parsec.ParseError Programs
parse = Text.Parsec.parse programs ""
