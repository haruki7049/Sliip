module Sliip.Parser
  ( -- * New Parser (Extended Syntax)
    parseExtended,
    ExtendedPrograms,
    Expr (..),
    Param (..),
    TypeExpr (..),
    Ctor (..),
    Pattern (..),
    -- * Legacy Parser (S-Expression) - Default for compatibility
    parse,
    parseLegacy,
    programs,
    Programs,
    SExpression (..),
    Atom (..),
  )
where

import Text.Parsec (ParseError, choice, eof, many, (<|>))
import qualified Text.Parsec as P
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (LanguageDef, TokenParser, caseSensitive, commentEnd, commentLine, commentStart, identLetter, identStart, makeTokenParser, nestedComments, opLetter, opStart, reservedNames, reservedOpNames)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Token as TT (identifier, reserved, stringLiteral, symbol, whiteSpace)

-- Legacy S-Expression AST (for backward compatibility) ----------------------

newtype SExpression
  = SExpr [Atom]
  deriving (Show, Eq)

data Atom
  = StringLiteral String
  | Builtin String
  | Reference String
  | SExprV SExpression
  deriving (Show, Eq)

-- New Extended AST ----------------------------------------------------------

data Expr
  = ENumber Integer
  | EFloat Double
  | EString String
  | EBool Bool
  | ESymbol String
  | EList [Expr]
  | EDefine String Expr
  | ELambda [Param] [Expr]
  | EIf Expr Expr Expr
  | ELet [(String, Expr)] [Expr]
  | ELetStar [(String, Expr)] [Expr]
  | ELetRec [(String, Expr)] [Expr]
  | EBegin [Expr]
  | EQuote Expr
  | EAscription Expr TypeExpr
  | EDefType String [String] [Ctor]
  | EMatch Expr [(Pattern, [Expr])]
  | EApp Expr [Expr]
  deriving (Show)

data Param = Param String (Maybe TypeExpr)
  deriving (Show)

data TypeExpr
  = TName String
  | TApp String [TypeExpr]
  | TArrow [TypeExpr] -- (-> a b c)
  deriving (Show)

data Ctor = Ctor String [TypeExpr]
  deriving (Show)

data Pattern
  = PVar String
  | PWildcard
  | PUnit
  | PCtor String [Pattern]
  deriving (Show)

type ExtendedPrograms = [Expr]

-- Lexer ---------------------------------------------------------------------

languageDef :: Tok.LanguageDef ()
languageDef =
  emptyDef
    { Tok.commentLine = ";",
      Tok.commentStart = "#|",
      Tok.commentEnd = "|#",
      Tok.nestedComments = True,
      Tok.identStart = P.letter P.<|> P.oneOf "+-*/<>=!?_",
      Tok.identLetter = P.alphaNum P.<|> P.oneOf "+-*/<>=!?_-'",
      Tok.reservedNames =
        [ "define",
          "lambda",
          "let",
          "let*",
          "letrec",
          "if",
          "begin",
          "quote",
          "def-type",
          "match",
          "as",
          "true",
          "false",
          "Nil",
          "Cons"
        ]
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser languageDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = Tok.naturalOrFloat lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

-- Basic parsers --------------------------------------------------------------

parseNumber :: Parser Expr
parseNumber = do
  nf <- naturalOrFloat
  return $ case nf of
    Left i -> ENumber i
    Right d -> EFloat d

parseString :: Parser Expr
parseString = EString <$> stringLiteral

parseBool :: Parser Expr
parseBool =
  (reserved "true" >> return (EBool True))
    P.<|> (reserved "false" >> return (EBool False))

parseSymbol :: Parser Expr
parseSymbol = ESymbol <$> identifier

-- Type parser ---------------------------------------------------------------

parseType :: Parser TypeExpr
parseType = P.try parseArrow P.<|> parseTypeAtom

parseTypeAtom :: Parser TypeExpr
parseTypeAtom =
  parens $
    do
      name <- identifier
      args <- P.many parseType
      return $ TApp name args
      P.<|> (TName <$> identifier)

parseArrow :: Parser TypeExpr
parseArrow = parens $ do
  _ <- Tok.symbol lexer "->"
  ts <- P.many1 parseType
  return $ TArrow ts

-- Pattern parser ------------------------------------------------------------

parsePattern :: Parser Pattern
parsePattern =
  (reservedOp "_" >> return PWildcard)
    P.<|> parens
      ( do
          name <- identifier
          pats <- P.many parsePattern
          return $ PCtor name pats
      )
    P.<|> P.try (P.string "()" >> return PUnit)
    P.<|> (PVar <$> identifier)

-- Constructors (def-type) --------------------------------------------------

parseCtor :: Parser Ctor
parseCtor = parens $ do
  name <- identifier
  tys <- P.many parseType
  return $ Ctor name tys

-- Expressions ---------------------------------------------------------------

parseAscription :: Parser Expr
parseAscription = do
  reserved "as"
  e <- parseExpr
  EAscription e <$> parseType

parseDefine :: Parser Expr
parseDefine = do
  reserved "define"
  name <- identifier
  EDefine name <$> parseExpr

parseLambda :: Parser Expr
parseLambda = do
  reserved "lambda"
  params <- parens (P.many parseParam)
  body <- P.many1 parseExpr
  return $ ELambda params body

parseParam :: Parser Param
parseParam =
  P.try
    ( parens $ do
        n <- identifier
        Param n . Just <$> parseType
    )
    P.<|> (Param <$> identifier <*> pure Nothing)

parseLetLike :: String -> Parser Expr
parseLetLike kw = do
  reserved kw
  binds <- parens (P.many parseBinding)
  body <- P.many1 parseExpr
  case kw of
    "let" -> return $ ELet binds body
    "let*" -> return $ ELetStar binds body
    "letrec" -> return $ ELetRec binds body
    _ -> fail "unknown let-like"

parseBinding :: Parser (String, Expr)
parseBinding = parens $ P.try bindingWithType P.<|> bindingSimple
  where
    bindingSimple = do
      name <- identifier
      expr <- parseExpr
      return (name, expr)
    bindingWithType = do
      name <- identifier
      -- try parse a type next; if it fails, backtrack and parse as expr
      t <- P.try parseType
      initExpr <- parseExpr
      return (name, EAscription initExpr t)

parseIf :: Parser Expr
parseIf = do
  reserved "if"
  c <- parseExpr
  t <- parseExpr
  EIf c t <$> parseExpr

parseBegin :: Parser Expr
parseBegin = do
  reserved "begin"
  es <- P.many1 parseExpr
  return $ EBegin es

parseQuote :: Parser Expr
parseQuote = do
  reserved "quote"
  EQuote <$> parseExpr

parseDefType :: Parser Expr
parseDefType = do
  reserved "def-type"
  name <- identifier
  params <- parens (P.many identifier)
  ctors <- P.many1 parseCtor
  return $ EDefType name params ctors

parseMatch :: Parser Expr
parseMatch = do
  reserved "match"
  expr <- parseExpr
  clauses <-
    P.many1
      ( parens $ do
          pat <- parsePattern
          body <- P.many1 parseExpr
          return (pat, body)
      )
  return $ EMatch expr clauses

-- Generic list/app parsing --------------------------------------------------

parseApplicationOrSpecial :: Parser Expr
parseApplicationOrSpecial = parens $ do
  whiteSpace
  look <- P.optionMaybe (P.lookAhead (P.many1 (P.letter P.<|> P.oneOf "+-*/<>=!?_-'")))
  case look of
    Just "define" -> parseDefine
    Just "lambda" -> parseLambda
    Just "let" -> parseLetLike "let"
    Just "let*" -> parseLetLike "let*"
    Just "letrec" -> parseLetLike "letrec"
    Just "if" -> parseIf
    Just "begin" -> parseBegin
    Just "quote" -> parseQuote
    Just "as" -> parseAscription
    Just "def-type" -> parseDefType
    Just "match" -> parseMatch
    _ -> do
      -- not one of the special forms: parse as application/list
      es <- P.many parseExpr
      case es of
        [] -> return $ EList []
        (f : args) -> return $ EApp f args

-- Top-level expression parser (includes atom parsers) ----------------------

parseAtom :: Parser Expr
parseAtom =
  P.try parseNumber
    P.<|> P.try parseString
    P.<|> P.try parseBool
    P.<|> P.try (parens (do _ <- P.char '\''; EQuote <$> parseExpr)) -- '( ...) rare
    P.<|> parseSymbol

parseExpr :: Parser Expr
parseExpr =
  whiteSpace
    *> ( P.try parseAtom
           P.<|> P.try parseApplicationOrSpecial
       )

-- Program -------------------------------------------------------------------

parseProgram :: Parser ExtendedPrograms
parseProgram = whiteSpace *> P.many parseExpr <* P.eof

-- Public API (New Parser) ---------------------------------------------------

parseExtended :: String -> Either ParseError ExtendedPrograms
parseExtended = P.parse parseProgram ""

-- Legacy S-Expression Parser (for backward compatibility) --------------------

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
      reservedNames = ["define", "lambda", "main", "write-line", "progn"],
      caseSensitive = True
    }

legacyLexer :: TokenParser ()
legacyLexer = makeTokenParser sliipStyle

legacySymbol :: String -> Parser String
legacySymbol = TT.symbol legacyLexer

legacyWhitespace :: Parser ()
legacyWhitespace = TT.whiteSpace legacyLexer

legacyIdentifier :: Parser String
legacyIdentifier = TT.identifier legacyLexer

legacyBuiltin :: Parser String
legacyBuiltin =
  choice (map (\kw -> TT.reserved legacyLexer kw >> return kw) $ reservedNames sliipStyle)

legacyStringLiteral :: Parser String
legacyStringLiteral = TT.stringLiteral legacyLexer

sexpr :: Parser SExpression
sexpr = do
  _ <- legacySymbol "("
  x <- many atom
  _ <- legacySymbol ")"
  return (SExpr x)

atom :: Parser Atom
atom =
  (SExprV <$> sexpr)
    <|> (Builtin <$> legacyBuiltin)
    <|> (StringLiteral <$> legacyStringLiteral)
    <|> (Reference <$> legacyIdentifier)

type Programs = [SExpression]

programs :: Parser Programs
programs = do
  legacyWhitespace
  expr <- many sexpr
  legacyWhitespace
  eof
  return expr

parseLegacy :: String -> Either ParseError Programs
parseLegacy = P.parse programs ""

-- Default parse uses the legacy parser for backward compatibility
parse :: String -> Either ParseError Programs
parse = parseLegacy
